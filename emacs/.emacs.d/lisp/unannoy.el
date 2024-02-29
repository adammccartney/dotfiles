;;; unannoy.el --- disable Emacs' annoying bits
;;; originally written by nullprogram (Chris Wellons)

;; GUIs are for newbs
;;(menu-bar-mode -1)
;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Too distracting
(blink-cursor-mode -1)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; Do sensible clipboard things, please
(setf backup-inhibited t
      autosave-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      initial-scratch-message nil
      wdired-allow-tochange-permissions t
      echo-keystrokes 0.1
      select-enable-clipboard nil
      select-enable-primary t
      mouse-drag-copy-region t
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      mouse-yank-at-point t)

;; Lexical binding by default. Must be delayed since Emacs sets this
;; on its own to nil after initialization.
(run-at-time 0 nil (lambda ()
                     (setq-default lexical-binding t)))

;; Tabs suck
(setq-default indent-tabs-mode nil)

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Insert key is stupid
(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)

;; I hate hitting this by accident
(global-set-key (kbd "C-<up>") #'previous-line)
(global-set-key (kbd "C-<down>") #'next-line)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)

(provide 'unannoy)


