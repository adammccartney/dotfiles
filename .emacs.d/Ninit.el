;;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setq initial-scratch-message
      (concat
       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
       ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n"
       ";;\n"
       ";; __          __  _                            \n"
       ";; \\ \\        / / | |                           \n"
       ";;  \\ \\  /\\  / /__| | ___ ___  _ __ ___   ___   \n"
       ";;   \\ \\/  \\/ / _ \\ |/ __/ _ \\| '_ ` _ \\ / _ \\  \n"
       ";;    \\  /\\  /  __/ | (_| (_) | | | | | |  __/_ \n"
       ";;     \\/  \\/ \\___|_|\\___\\___/|_| |_| |_|\\___(_)\n"))

;; Leave this here, or package.el will just add it again.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Also add all directories within "lisp"
;; I use this for packages I'm actively working on, mostly.
(let ((files (directory-files-and-attributes (expand-file-name "lisp" user-emacs-directory) t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(if (not (eq window-system 'w32))
    (add-to-list 'exec-path "/usr/local/bin"))

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'init-utils)
(require 'init-elpa)

;; Use Package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
;;(menu-bar-mode -1)
;;(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 4)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)

;; Allow "confusing" functions
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(defun air--delete-trailing-whitespace-in-prog-and-org-files ()
  "Delete trailing whitespace if the buffer is in `prog-' or `org-mode'."
  (if (or (derived-mode-p 'prog-mode)
          (derived-mode-p 'org-mode))
      (delete-trailing-whitespace)))
(add-to-list 'write-file-functions 'air--delete-trailing-whitespace-in-prog-and-org-files)

(defun my-minibuffer-setup-hook ()
  "Increase GC cons threshold."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Set GC cons threshold to its default value."
  (setq gc-cons-threshold 1000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))

;;; Larger package-specific configurations.
(require 'init-fonts)
(require 'init-evil)
(require 'init-maps)
(require 'init-w3m)
(require 'init-flycheck)
(require 'init-tmux)


;; ===================================
;; Use Package by John Wiegly
;; ===================================
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (sml/setup))

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :defer t)

(use-package helm-make
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-c m") 'helm-make-projectile))

(use-package helm-org
  :ensure t
  :commands helm-org-agenda-files-headings)

(use-package dired
  :defer t
  :config
  (require 'dired-x)
  (setq dired-omit-files "^\\.?#\\|^\\.[^.].*")

  (defun air-dired-buffer-dir-or-home ()
    "Open dired to the current buffer's dir, or $HOME."
    (interactive)
    (let ((cwd (or (file-name-directory (or (buffer-file-name) ""))
                   (expand-file-name "~"))))
      (dired cwd)))

  (add-hook 'dired-mode-hook (lambda ()
                               (dired-omit-mode t)
                               (all-the-icons-dired-mode t)))
  (define-key dired-mode-map (kbd "RET")     'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")       (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-.")     'dired-omit-mode)
  (define-key dired-mode-map (kbd "c")       'find-file)
  (define-key dired-mode-map (kbd "/")       'counsel-grep-or-swiper)
  (define-key dired-mode-map (kbd "?")       'evil-search-backward)
  (define-key dired-mode-map (kbd "C-c C-c") 'dired-toggle-read-only))

(eval-after-load 'wdired
  (add-hook 'wdired-mode-hook 'evil-normal-state))

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (defun rjsx-mode-config ()
    "Configure RJSX Mode"
    (define-key rjsx-mode-map (kbd "C-j") 'rjsx-delete-creates-full-tag))
  (add-hook 'rjsx-mode-hook 'rjsx-mode-config))

(use-package elpy
  :ensure t
  :mode "\\.py\\'"
  :config
  (elpy-enable))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (if (not (string-match "go" compile-command))
                                (set (make-local-variable 'compile-command)
                                     "go build -v && go test -v && go vet"))
                            (setq compilation-read-command nil)
                            (add-hook 'before-save-hook 'gofmt-before-save nil t)
                            (define-key go-mode-map (kbd "C-c C-C") 'compile))))


(use-package gnuplot
  :ensure t
  :defer t)

(use-package gnuplot-mode)

(use-package better-defaults)       ;; Set up some better Emacs defaults

(use-package plantuml-mode)

(use-package flycheck
  :ensure t
  :init)              ;; On the fly syntax checking

(use-package helm
  :bind(("M-x" . helm-M-x)
	("M-<f5>" . helm-find-files)
	([f10] . helm-buffers-list)
	([S-f10] . helm-recentf)))

(use-package irony
  :hook (c++-mode-hook . irony-mode)
  :hook (c-mode-hook . irony-mode)
  :hook (objc-mode-hook . irony-mode)

  :hook (irony-mode-hook . irony-cdb-autosetup-compile-options))

(use-package org                   ;; improve your life with org-mode
  :init
   (setq org-agenda-include-diary t)
  :bind(("C-c l" . org-store-link)
	("C-c a" . org-agenda)
	("C-c c" . org-capture)))

(use-package dictionary
  :ensure t
  :defer t)

(use-package py-autopep8)           ;; Run autopep8 on save

(use-package blacken)               ;; Black formatting on save

;;(use-package material-theme)        ;; Theme
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc --from markdown_github-hard_line_breaks --to html")
  (define-key markdown-mode-map (kbd "<C-return>") 'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c '")      'fence-edit-code-at-point)
  (define-key markdown-mode-map (kbd "C-c 1")      'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2")      'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3")      'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4")      'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5")      'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6")      'markdown-insert-header-atx-6)

  (add-hook 'markdown-mode-hook (lambda ()
                                  (visual-line-mode t)
                                  (set-fill-column 80)
                                  (yas-minor-mode-on)
                                  (hugo-minor-mode t)
                                  (turn-on-auto-fill)
                                  ;; Don't wrap Liquid tags
                                  (setq auto-fill-inhibit-regexp (rx "{" (? "{") (1+ (or "%" "<" " ")) (1+ letter)))
                                  (flyspell-mode))))

(use-package yaml-mode
  :ensure t
  ;; .yaml or .yml
  :mode "\\(?:\\(?:\\.y\\(?:a?ml\\)\\)\\)\\'")

(use-package yasnippet
  :ensure t
  :defer t
  :config
  ;;(yas-reload-all)
  (setq tab-always-indent 'complete)
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-zenburn t))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook
            (lambda ()
              (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook
            (lambda ()
              (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

;;; Flycheck mode:
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))


;; ===================================
;; Programming Modes
;;; All programming modes
(defun air--set-up-prog-mode ()
  "Configure global `prog-mode'."
  (setq-local comment-auto-fill-only-comments t)
  (electric-pair-local-mode))
(add-hook 'prog-mode-hook 'air--set-up-prog-mode)

;;; If `display-line-numbers-mode' is available (only in Emacs 26),
;;; use it! Otherwise, install and run nlinum-relative.
(if (functionp 'display-line-numbers-mode)
    (and (add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type 'relative)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package nlinum-relative
    :ensure t
    :config
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook #'nlinum-relative-mode)))

;;; Python mode:
(use-package virtualenvwrapper
  :ensure t
  :defer t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(defun air-python-setup ()
  "Configure Python environment."
  (let* ((root (air--get-vc-root))
         (venv-name (car (last (remove "" (split-string root "/")))))
         (venv-path (expand-file-name venv-name venv-location)))
    (if (and venv-name
             venv-path
             (file-directory-p venv-path))
        (venv-workon venv-name))))

(add-hook 'python-mode-hook
          (lambda ()
            ;; I'm rudely redefining this function to do a comparison of `point'
            ;; to the end marker of the `comint-last-prompt' because the original
            ;; method of using `looking-back' to match the prompt was never
            ;; matching, which hangs the shell startup forever.
            (defun python-shell-accept-process-output (process &optional timeout regexp)
              "Redefined to actually work."
              (let ((regexp (or regexp comint-prompt-regexp)))
                (catch 'found
                  (while t
                    (when (not (accept-process-output process timeout))
                      (throw 'found nil))
                    (when (= (point) (cdr (python-util-comint-last-prompt)))
                      (throw 'found t))))))

            ;; Additional settings follow.
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (air-python-setup)))



;; ===================================
;; Basic Customization
;; ===================================

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq inhibit-startup-message t)    ;; Hide the startup message
(load-theme 'material t)            ;; Load material theme
(global-linum-mode t)               ;; Enable line numbers globally

(require 'evil)
(evil-mode t)

;; To get plantuml with org mode
(require 'ob-plantuml)

;; ==================
;; plant uml with Org mode
;; =================
(with-eval-after-load 'org
  (setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
   (latex . t)
   (python . t)
   (plantuml . t)
   ))) ; this line activates dot


;;==============================
;; Plantuml config
;;==============================
;; Set up plantuml

(setq plantuml-jar-path "~/usr/share/plantuml/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

;; executable configuration
(setq plantuml-executable-path "/usr/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; User-Defined .emacs ends here



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
 '(fci-rule-color "#37474f")
 '(font-use-system-font t)
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   (quote
    (powerline-evil powerline jedi flycheck blacken gnuplot-mode gnuplot org-babel-eval-in-repl plantuml-mode org latex-math-preview helm use-package ## evil-visual-mark-mode ein material-theme)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "light gray" :foreground "pale green")))))
