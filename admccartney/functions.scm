(define-module (admccartney functions)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports))


(define (apply-template template-string value-alist)
  (regexp-substitute/global #f
                            "\\$\\{([A-Za-z/\\-]+)\\}"
                            template-string
                            'pre
                            (lambda (m)
                              (let ((entry (assq (string->symbol (match:substring m 1))
                                                 value-alist)))
                                (if entry
                                    (cdr entry)
                                    "VALUE NOT FOUND")))
                            'post))

(define (apply-template-file file-path value-alist)
  (call-with-input-file file-path
    (lambda (port)
      (apply-template (get-string-all port)
                      value-alist))))
  


