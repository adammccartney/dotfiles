#!/usr/bin/guile3.0 -s
!#

(use-modules (ice-9 format)
             (ice-9 regex))

(define (kubeconfig? str)
  ;; test if the string contains KUBECONFIG
  ;; (kubeconfig? "KUBECONFIG_PROD=/path/to/thing")
  ;; KUBECONFIG_PROD=/path/to/thing
  (if (string-match "KUBECONFIG" str)
      (let ((res str))
        res)
      #f))

(define (create-menu opts)
  (display "Please select one of the following:")
  (newline)
    (define (iter-create-menu opts pos res)
      ;; add a numeric prefix and display a list of options
      (if (null? opts)
          menu-ls
          (begin
            (set! menu-ls (acons pos (car opts) menu-ls))
            (display (format #f "~d →  ~s" pos (cdr (assq pos menu-ls))))
            (newline)
            (iter-create-menu (cdr opts) (+ pos 1) menu-ls))))
  (define menu-ls '())
  (iter-create-menu opts 1 menu-ls))

(define (send-mq pxmq msg)
  ;; write to a posix msg queue
    (system* "pmsg_send" "-n" pxmq msg))

(define (main mq)
  ;; Allow the user to select from existing KUBECONFIG_<TYPE> files
  ;; these should be available in the current shell environment
  ;; upon selection, start a subshell where where the selected
  ;; variable is used to set the KUBECONFIG variable.
  ;; Note that the first arg is a temporary output file
  (define uinput (lambda ()(read)))
  (define kube-config-files)
  (define choice)
  (define (getcfg choice kube-config-files)
     (let ((envvar (cdr (assq choice kube-config-files))))
       (car (cdr (string-split envvar #\=)))))
  (let ([kconfig-opts (filter kubeconfig? (environ))])
    (if kconfig-opts
        (begin  ;; this is the main io loop
          (set! kube-config-files (create-menu kconfig-opts))
          (set! choice (uinput))
          (let ([kcfg (getcfg choice kube-config-files)])
            (send-mq mq kcfg))))))

;; Here starts the stuff
(main (cadr (command-line)))
(newline)

;; tests
(define test-env-kubeconfig
  (list "PATH=/k/y/z" "KUBECONFIG_PROD=/somewhere/config.yaml"))

(filter kubeconfig? '("KUBECONFIG_PROD" "NOTHING"))

;; scratch

