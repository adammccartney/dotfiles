;; init.el --- -*- lexical-binding: t; -*-

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; init.el --- -*- lexical-binding: t; -*-

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; bootstrap straight.el
   (defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)) 
  
;; Emacs version >= 27
(setq package-enable-at-startup nil)
  
;; Use by default
(straight-use-package 'use-package)

;; Saves typing `:straight t` after every use-package expression  
(setq straight-use-package-by-default t)

;; Clean up unused repos with `straight-remove-unused-repos'

(push "~/dotfiles/.emacs.d/lisp" load-path)  

(require 'unannoy)
(require 'ad-mail)

;; Some global keybindings
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(column-number-mode)
(global-display-line-numbers-mode t)

;; Global line width
(setq-default fill-column 72)

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))

;; Frames and fonts

(defvar my-preferred-fonts
  '("Noto Mono-10"
    "Inconsolata-12"))

(defun my-set-preferred-font (&optional frame)
  "Set the first available font from `my-preferred-fonts'."
  (catch 'done
    (with-selected-frame (or frame (selected-frame))
      (dolist (font my-preferred-fonts)
        (when (ignore-errors (x-list-fonts font))
          (set-frame-font font)
          (throw 'done nil))))))

(defun my-set-frame-fullscreen (&optional frame)
  (set-frame-parameter frame 'fullscreen 'fullheight))

(add-hook 'after-make-frame-functions #'my-set-preferred-font)
(add-hook 'after-make-frame-functions #'my-set-frame-fullscreen t)


;; Calendar and planner notification stuff
(appt-activate t)

(use-package color-theme-sanityinc-tomorrow
  :config
  (setf custom-safe-themes t)
  (color-theme-sanityinc-tomorrow-night)
  (global-hl-line-mode 1)
  (custom-set-faces
   '(cursor ((t :background "#eebb28")))))
