(use anaphora
     base64
     hyde
     hyde-atom
     lowdown
     posix
     posix-extras
     srfi-1
     srfi-18
     sxml-transforms)

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

;; (load "hyde-for-new-scss")

(set! sxml-conversion-rules
  (cons `(*PI* *preorder* . ,(lambda (tag args)
                              (string-append "<?"
                                             (symbol->string (car args))
                                             " "
                                             (string-join (cdr args))
                                             "?>\n")))
        sxml-conversion-rules))

(define markdown-transforms-rules
  `((heading . ,(lambda (tag args)
                  (append (list tag (+ 1 (car args))) (cdr args))))
    (*text* . ,(lambda (tag args) args))
    (*default* . ,(lambda (tag args) (cons tag args)))))

(define (translate/md)
  (let* ((input-md (markdown->sxml*))
         (transformed-md (pre-post-order* input-md markdown-transforms-rules))
         (html-md (markdown-sxml->html-sxml transformed-md))
         (html-output (pre-post-order* html-md sxml-conversion-rules)))
    (SRV:send-reply html-output)))

(translators
  (cons (list "md" translate/md)
        (translators)))


(define $ (environment-ref (page-eval-env) '$))

(default-page-vars '(((* any)
                       (main-title ("en" . "Kooda’s burrow")
                                   ("fr" . "Le terrier de Kooda"))
                       ; (base-uri . "//localhost:8080/")
                       (base-uri . "//www.upyum.com")
                       (footer ("en" . ("Website generated with "
                                        (a (@ (href "http://wiki.call-cc.org/egg/hyde")) "Hyde") "."))
                               ("fr" . ("Site généré avec "
                                        (a (@ (href "http://wiki.call-cc.org/egg/hyde")) "Hyde") "."))))
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

(define (file-data-url mime path)
  (string-append "url(data:"
                 mime
                 ";base64,"
                 (base64-encode
                  (call-with-input-file path (cut read-string #f <>)))
                 ")"))

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

(define (reading-time path)
  (let ((time (inexact->exact
               (ceiling (/ (length (string-split (page-content path))) 150)))))
    `(,time
      " "
      ,(if (< time 2) "minute" "minutes"))))

(define (sort-by pages accessor)
  (sort pages (lambda (p1 p2) (> (accessor p1) (accessor p2)))))

(define (pages-matching regex)
  (map cdr (filter (lambda (p) (irregex-match regex (car p)))
                   ((environment-ref (page-eval-env) 'pages)))))

(define (all-posts)
  (sort-by (pages-matching `(: ,($ 'lang) "/post/" (+ any)))
           (cut $ 'date <>)))
