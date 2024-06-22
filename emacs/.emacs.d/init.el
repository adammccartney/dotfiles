;; init.el --- -*- lexical-binding: t; -*-

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; ;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;---------------------------------------------------------------------------
;; Packaging
;;---------------------------------------------------------------------------
;; Just use GUIX!
;;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;			 ("elpa" . "https://elpa.gnu.org/packages/")
;;			 ("org" . "http://orgmode.org/elpa/")))
;;
;;;; Install and load `quelpa-use-package'.
;;(package-install 'quelpa-use-package)
;;(require 'quelpa-use-package)

;; Set up use-package
(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path '"~/dotfiles/emacs/.emacs.d/lisp")
;; Make sure to load packages that were installed by guix
(add-to-list 'load-path (format "%s/share/emacs/site-lisp" (getenv "GUIX_PROFILE")))
;(use-package guix-emacs)
;(guix-emacs-autoload-packages)

;;--------------------------------------------------------------------------
;; Some global settings
;;--------------------------------------------------------------------------

;; Suppress warnings generated by native compilation stuff
(setq native-comp-async-report-warnings-errors nil)

;; Movement, add new lines to end of file
(setq next-line-add-newlines t)
;; Copy to system clipboard
(setq x-select-enable-clipboard t)

;; Some global keybindings
(column-number-mode)
(global-display-line-numbers-mode t)

;; Global line width
(setq-default fill-column 80)


;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :config
  ;; Keep customization settings in a temporary file (thanks Ambrevar!)
  (setq custom-file
        (if (boundp 'server-socket-dir)
            (expand-file-name "custom.el" server-socket-dir)
          (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
  (load custom-file t))

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; ignore anything pulled in from gnome
(define-key special-event-map [config-changed-event] 'ignore)

(set-default-coding-systems 'utf-8)
(server-start)

;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))

;; Calendar and planner notification stuff
(appt-activate t)

(global-set-key [remap list-buffers] 'ibuffer)

;; Window management
(winner-mode)
(global-set-key (kbd "M-o") 'other-window)


(use-package emacs
  :init
  ;; TAB cycle if there are only a few candidates
  (setq completion-cycle-threshold 3)

  ;; Hide commands in M-x which do not apply to the current mode
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; completion-at-point is often bound to M-TAB
  (setq tab-always-indent 'complete))

; EasyPG for encrypting files
(require 'epa-file)
(epa-file-enable)

(require 'transient)

;;----------------------------------------------------------------------------
;; load extra config.el files
;;---------------------------------------------------------------------------

;; Custom lispy stuff
(require 'adlisp)
(require 'ad-display)
(require 'ad-mail)
(require 'ad-org)
(require 'ad-pg)
(require 'ad-sqlite)
(require 'unannoy) ;; handy stuff from Chris Wellons
;; Load custom org config after org-roam

;;(require 'faust-mode) ;; from rukano
;;(setq auto-mode-alist (cons '("\\.dsp$" . faust-mode) auto-mode-alist))

;;---------------------------------------------------------------------------
;; UI stuff
;;---------------------------------------------------------------------------

;; themes
(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-winter :no-confirm))

(use-package fontaine)

(use-package tmr)


(use-package all-the-icons
  :if (display-graphic-p))

;; Drawing
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))


;;--------------------------------------------------------------------------
;; Writing (markdown)
;;--------------------------------------------------------------------------

(use-package markdown-mode
  :defer t
  :mode ("\\.md$" "\\.markdown$" "vimperator-.+\\.tmp$")
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (remove-hook 'fill-nobreak-predicate
                           'markdown-inside-link-p t)))
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'conditionally-turn-on-pandoc)
  (setf sentence-end-double-space nil
        markdown-indent-on-enter nil
        markdown-command
        "pandoc -f markdown -t html"))

;; mark for removal
;;(use-package imenu-list
;;  :bind (("C-'" . imenu-list-smart-toggle))
;;  :config
;;  (setq imenu-list-focus-after-activation t
;;        imenu-list-auto-resize nil))


;; TODO: refactor Spelling (or just get it working reliably)
(setq dictionary-server "localhost")
;; mandatory, as the dictionary misbehaves!
(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
   '("^\\*Dictionary\\*" display-buffer-in-side-window
     (side . left)
     (window-width . 50)))

;; close to ispell lookup
(global-set-key (kbd "M-§") #'dictionary-lookup-definition)

;;(add-to-list 'Info-directory-list "/usr/share/info")


;;--------------------------------------------------------------------------
;; Project, workspace & directory management
;;--------------------------------------------------------------------------


(use-package dired
  :config
  (progn
    (add-hook 'dired-mode-hook #'toggle-truncate-lines)
    (setf dired-listing-switches "-alhG"
          dired-guess-shell-alist-user
          '(("\\.pdf\\'" "evince")
            ("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")
            ("\\(\\.png\\|\\.jpe?g\\)\\'" "qiv")
            ("\\.gif\\'" "animate")))))

;; some pretty schnick-schnack for dired
(use-package all-the-icons-dired)

;;--------------------------------------------------------------------------
;; Completion frameworks
;;--------------------------------------------------------------------------

; Mark for removal
;;(use-package helm
;;  :init
;;  ;; a good chunk of the following config is from
;;  ;; http://tuhdo.github.io/helm-intro.html
;;  ;; default "C-x c" is close to quit "C-x C-c"
;;  ;; "C-h"
;;  (global-set-key (kbd "C-c h") 'helm-command-prefix)
;;  (global-unset-key (kbd "C-x c"))
;;  (global-set-key (kbd "M-x") 'helm-M-x)
;;  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;  (global-set-key (kbd "C-x b") 'helm-mini)
;;  (global-set-key (kbd "C-x C-f") 'helm-find-files)
;;  
;;  (when (executable-find "curl")
;;    (setq helm-google-suggest-use-curl-p t))
;;
;;  (setq helm-autoresize-max-height 0)
;;  (setq helm-autoresize-min-height 20)
;;  (setq helm-M-x-fuzzy-match t)
;;  (setq helm-buffers-fuzzy-matching t
;;        helm-recentf-fuzzy-match t)
;;  
;;  :config
;;  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;  (define-key helm-map (kbd "C-z") 'helm-select-action)
;;  (helm-autoresize-mode 1))
;;
;;(use-package helm-pass)

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;(use-package company
;;  :config
;;  (setq company-global-mode t)
;;  (setq company-minimum-prefix-length 2)
;;  (setq company-idle-delay
;;        (lambda () (if (company-in-string-or-comment) nil 0.3)))
;;  (setq company-global-modes '(not org-mode))
;;  (setq company-selection-wrap-around t))


(use-package which-key
  :config
  (which-key-mode))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package dumb-jump)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

;;----------------------------------------------------------------------------
;; dev stuff
;;---------------------------------------------------------------------------

(use-package go-mode)
;; BUG: emacs pics up the PATH in a funny way, sometimes gopath is absent,
;; run these two lines if it's acting up.
;;(setenv "PATH" (concat (getenv "PATH") ":/home/amccartn/go/bin"))
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/go/bin"))


;; lsp
(use-package jsonrpc)
(use-package eglot
  :hook (((c-mode c++-mode) . eglot-ensure)       
         (python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (lua-mode . eglot-ensure)
         (go-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t))

;;(with-eval-after-load 'eglot
;;  (add-to-list 'eglot-server-programs
;;               '(python-mode . ("/usr/bin/pylsp"))))


(use-package magit
  :init (if (not (boundp 'project-switch-commands)) 
            (setq project-switch-commands nil))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package project)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-reload-all))


;; Tree-sitter 
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (supercollider "https://github.com/madskjeldgaard/tree-sitter-supercollider")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; This expression will fetch and install the grammars listed above
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

;; parenthesis stuff

(use-package paren
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

(use-package browse-url
  :defer t
  :init
  (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "firefox")
    (setf browse-url-browser-function #'browse-url-firefox)))

;; geiser is pretty cool, in fairness

(use-package geiser
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (setq geiser-guile-binary (format "%s/bin/guile" (getenv "GUIX_PROFILE"))))

(use-package geiser-guile)

;; ensure some c related goodness
(use-package cc-mode
  :defer t
  :hook (cc-mode . lsp-deferred)
  :init
  (defun my/c-hook ()
    (setf c-basic-offset 4)   
    (c-set-offset 'case-label '+)
    (c-set-offset 'access-label '/)
    (c-set-offset 'label '/))
  :config
  (progn
    (define-key java-mode-map (kbd "C-x I") 'add-java-import)
    (add-hook 'c-mode-hook #'my/c-hook)
    (add-hook 'c++-mode-hook #'my/c-hook)
    (add-to-list 'c-default-style '(c-mode . "k&r"))
    (add-to-list 'c-default-style '(c++-mode . "k&r"))))


;; asm related packages
(use-package nasm-mode
  :defer t
  :mode ("\\.nasm$" "\\.asm$" "\\.s$")
  :config
  (add-hook 'nasm-mode-hook (lambda () (setf indent-tabs-mode t))))

(use-package asm-mode
  :defer t
  :init
  (add-hook 'asm-mode-hook (lambda () (setf indent-tabs-mode t
                                            tab-always-indent t))))

(use-package x86-lookup
  :defer t
  :init
  (setq x86-lookup-pdf '"~/Documents/bookstaging/325383-sdm-vol-2abcd.pdf")
  (global-set-key (kbd "C-h x") #'x86-lookup))

;; Rust
(use-package rust-mode
  :config
  (setq rust-mode-treeqsitter-derive t))

(use-package dockerfile-mode)

(use-package paredit)

(use-package eshell
  :defer t
  :bind ([f1] . eshell-as)
  :init
  (setf eshell-directory-name (locate-user-emacs-file "local/eshell"))
  :config
  (add-hook 'eshell-mode-hook ; Bad, eshell, bad!
            (lambda ()
              (define-key eshell-mode-map (kbd "<f1>") #'quit-window))))


;; gdb setup
(setq gdb-many-windows t)

(use-package slime
  :init 
  (setq inferior-lisp-program "/usr/bin/sbcl")
  ;;  (setq slime-lisp-implementations
  ;;        (let ((core-file (format "%s" (substitute-in-file-name "$HOME/.cache/emacs/sbcl.core-for-slime"))))
  ;;        '((sbcl ("sbcl" "--core" core-file)))))
  :config
  (add-hook 'slime-load-hook
            (lambda ()
              (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))))


; Tramp for tramping
;;(tramp-set-completion-function
;; "ssh"
;; '((tramp-parse-sconfig "/etc/ssh_config")
;;   (tramp-parse-sconfig "~/.ssh/config")))
(customize-set-variable 'tramp-default-method "ssh")
;;(setq debug-on-error t
;;      debug-on-signal t)
(customize-set-variable 'tramp-debug-to-file t)
(setq tramp-verbose 6)

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/src/guix/etc/snippets/yas"))


(use-package nix-mode
  ;; https://github.com/NixOS/nix-mode
  :mode "\\.nix\\'")

(use-package pg)


;;----------------------------------------------------------------------
;; shells
;;----------------------------------------------------------------------

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;;---------------------------------------------------------------------
;; matrix
;;---------------------------------------------------------------------
;;(use-package ement
;;  :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))


;;---------------------------------------------------------------------
;; reading
;;---------------------------------------------------------------------
(use-package i-ching
  :ensure f
  :config (setq i-ching-hexagram-size 18
                i-ching-hexagram-font "DejaVu Sans"
                i-ching-divination-method '3-coins
                i-ching-randomness-source 'pseudo)
  :bind (("C-c i h" . i-ching-insert-hexagram)))

(use-package elfeed
  :ensure f
  :config
  (setq elfeed-feeds
        '("https://nullprogram.com/feed/"
          "https://drewdevault.com/blog/index.xml")))

;;(require 'transient-showcase)
