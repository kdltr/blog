(use anaphora
     base64
     colorize
     html-parser
     hyde
     hyde-atom
     lowdown
     posix
     posix-extras
     srfi-1
     srfi-13
     srfi-18
     sxml-transforms
     uri-common)

;; Todo list:
;; * Tag atom feeds
;; * Navigation links in posts
;; * Table of content generation in posts
;; * Posts archive page
;; * Gallery-like history for the drawings page
;;
;; Done:
;; * Latest posts in posts.xhtml
;; * More color contrast
;; * Language links and language-independent URLs
;; * I18n of the remaining elements
;; * Try out the new versions of Hyde and SCSS
;; * Estimation of reading time
;; * Hide other (empty) pages for now
;; * Tagging system
;; * Atom feeds

(define (title-identify tag)
  `(,tag *preorder* .
         ,(lambda (tag args)
            `(,tag (@ (id ,(pathify (apply string-append (car args)))))
                   ,args))))

(define markdown-transforms-rules
  `((heading . ,(lambda (tag args)
                  (append (list tag (+ 1 (car args))) (cdr args))))
    (*text* . ,(lambda (tag args) args))
    (*default* . ,(lambda (tag args) (cons tag args)))))

(define html-transforms-rules
  `(,@(map title-identify '(h2 h3 h4 h5 h6))
    (*text* . ,(lambda (tag args) args))
    (*default* . ,(lambda (tag args) (cons tag args)))))

(set! sxml-conversion-rules
  (cons*
   `(*PI* *preorder* . ,(lambda (tag args)
                          (string-append "<?"
                                         (symbol->string (car args))
                                         " "
                                         (string-join (cdr args))
                                         "?>\n")))
   `(*COMMENT* *preorder* .
               ,(lambda (tag args)
                  ""))
   sxml-conversion-rules))


(define (translate/md)
  (let* ((input-md (markdown->sxml*))
         (transformed-md (pre-post-order* input-md markdown-transforms-rules))
         (html-md (markdown-sxml->html-sxml transformed-md))
         (transformed-html (pre-post-order* html-md html-transforms-rules))
         (html-output (pre-post-order* transformed-html sxml-conversion-rules)))
    (SRV:send-reply html-output)))

(translators
  (cons (list "md" translate/md)
        (translators)))


(define $ (environment-ref (page-eval-env) '$))

(default-page-vars '(((* any)
                       (main-title ("en" . "Kooda’s burrow")
                                   ("fr" . "Le terrier de Kooda"))
                       (base-uri . "//www.upyum.com")
                       (translated . #t))
                     ((: bos "en/" (+ any))
                       (lang . "en"))
                     ((: bos "fr/" (+ any))
                       (lang . "fr"))
                     ((: "post/" (+ any) eos)
                       (layouts "post.sxml" "default.sxml")
                       (category . "posts"))))

(default-extension "xhtml")

(define (i18n-cond en fr)
  (if (string=? ($ 'lang) "en")
      en
      fr))

(define (i18n-ref var)
  (alist-ref ($ 'lang) ($ var) string=?))

(define (i18n-link path)
  (string-append "/" ($ 'lang) "/" path))

(define (menu-link title path #!optional (id path))
  (let ((id (pathify id)))
    `(a (@ (href ,(string-append (i18n-link (pathify path)) ".xhtml"))
           (id ,(string-append id "-link"))
           ,(if (string=? ($ 'category) id) '(class selected) '()))
        ,title)))

(define (format-seconds seconds)
  (time->string (seconds->utc-time seconds)
                (i18n-cond
                 "%Y-%m-%d"
                 "%d/%m/%Y")))

(define (page-content source-path)
  (with-input-from-file (string-append "src/" source-path)
    (lambda ()
      (read)
      (read-string))))

(define (sort-by pages accessor)
  (sort pages (lambda (p1 p2) (> (accessor p1) (accessor p2)))))

(define (pages-matching regex)
  (map cdr (filter (lambda (p) (irregex-match regex (car p)))
                   ((environment-ref (page-eval-env) 'pages)))))

(define (all-posts)
  (sort-by (pages-matching `(: ,($ 'lang) "/post/" (+ any)))
           (cut $ 'date <>)))

;; <a href="https://flattr.com/submit/auto?fid=y79nzj&url=http%3A%2F%2Fwww.upyum.com%2F" target="_blank"><img src="//button.flattr.com/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0"></a>
(define (flattr-url #!optional (page (current-page)))
  (let* ((base-uri (uri-reference "https://flattr.com/submit/auto"))
         (attrs `((fid . "y79nzj")
                  (url . ,(string-append "https:" ($ 'base-uri page) (page-path page)))))
         (flattr-page-uri
          (parameterize ((form-urlencoded-separator "&"))
            (uri->string (update-uri base-uri query: attrs)))))
    flattr-page-uri))
