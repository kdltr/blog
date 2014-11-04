(use scss)

(define (translate/scss)
  (let loop ((sexp (read)))
    (unless (eof-object? sexp)
      (let ((scss (environment-eval sexp (page-eval-env))))
        (write-css scss))
      (loop (read)))))

(translators (cons (list "scss" translate/scss '(ext . css) '(layouts))
             (translators)))
