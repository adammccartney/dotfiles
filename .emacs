;; .emacs

;; To Install:
;;====================================
;;Package Support
;;====================================
;;Enables basic packaging support
(require 'package)

;;Adds the Melpa archive to the list of available repositories

(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/stable/"))

(setq package-enable-at-startup nil)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
;;(defvar myPackages
;;  '(better-defaults                 ;; Set up some better Emacs defaults
;;    elpy                            ;; Emacs Lisp Python Environment
;;    flycheck                        ;; On the fly syntax checking
;;    py-autopep8                     ;; Run autopep8 on save
;;    blacken                         ;; Black formatting on save
;;    material-theme                  ;; Theme
;;    )
;;  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
;;(mapc #'(lambda (package)
;;          (unless (package-installed-p package)
;;            (package-install package)))
;;      myPackages)

;; ===================================
;; Use Package by John Wiegly
;; ===================================

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package gnuplot)
(use-package gnuplot-mode)
(use-package markdown-mode
  :ensure t)
;;(use-package better-defaults)       ;; Set up some better Emacs defaults
(use-package plantuml-mode)
(use-package elpy)                  ;; Emacs Lisp Python Environment
(use-package flycheck)              ;; On the fly syntax checking
(use-package helm
  :bind(("M-x" . helm-M-x)
	("M-<f5>" . helm-find-files)
	([f10] . helm-buffers-list)
	([S-f10] . helm-recentf)))
(use-package latex-math-preview)
(use-package org                   ;; improve your life with org-mode
  :init
   (setq org-agenda-include-diary t)
  :bind(("C-c l" . org-store-link)
	("C-c a" . org-agenda)
	("C-c c" . org-capture)))

(use-package py-autopep8)           ;; Run autopep8 on save
(use-package blacken)               ;; Black formatting on save
(use-package material-theme)        ;; Theme


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


;;===================================
;; Development Setup
;;===================================
;; Enable elpy
(elpy-enable)


;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")


;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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
    (gnuplot-mode gnuplot org-babel-eval-in-repl plantuml-mode org latex-math-preview helm use-package ## evil-visual-mark-mode ein material-theme)))
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
