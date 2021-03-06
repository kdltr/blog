;; deps: hyde lowdown sxml-transforms

(import
  (chicken io)
  (chicken irregex)
  (chicken sort)
  (chicken string)
  (chicken time posix)
  hyde
  (hyde atom)
  lowdown
  srfi-1
  srfi-13
  sxml-transforms)

;; Todo list:
;; * Gopher phlog output
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
                     ((: bos (or "fr" "en") "/post/" (+ any) eos)
                      (layouts "post.sxml" "default.sxml"))))

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
    `(a (@ (href ,(string-append (i18n-link (pathify path)) ".xhtml")))
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

(define (all-scheme-posts)
  (filter (lambda (p) (eq? ($ 'category p) 'scheme)) (all-posts)))

(define (reading-time path)
  (let ((time (inexact->exact
               (ceiling (/ (length (string-split (page-content path))) 150)))))
    `(,(i18n-cond "Estimated reading time: "
                  "Durée de lecture estimée : ")
      ,time
      " "
      ,(if (< time 2) "minute" "minutes"))))
