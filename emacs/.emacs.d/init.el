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

;; grab the right version of org
(straight-use-package 'org)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :straight t)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(define-key special-event-map [config-changed-event] 'ignore)

(push "~/.emacs.d/lisp" load-path)  
(require 'unannoy)
;;(require 'ad-mail)
(require 'adlisp)
;;(require 'mu4e)

(set-default-coding-systems 'utf-8)

(server-start)

;; Copy to system clipboard
(setq x-select-enable-clipboard t)

;; Some global keybindings
(column-number-mode)
(global-display-line-numbers-mode t)

;; Global line width
(setq-default fill-column 80)

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))

(use-package all-the-icons
  :if (display-graphic-p))

;; You must run (all-the-icons-install-fonts) one time after installing
;; this package
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :ensure t
  :after eshell
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-intactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-ls t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e t)
  (doom-modeline-irc nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file.name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

;; Calendar and planner notification stuff
(appt-activate t)

(use-package visual-fill-column
  :config
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))

(use-package all-the-icons-dired)

(straight-use-package '(dired :type built-in))
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

(global-set-key [remap list-buffers] 'ibuffer)

;; Org mode
(use-package org 
  :defer t
  :after (org-roam ob-go) 
  :custom
  (org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  :config
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'after-save-hook #'org-babel-tangle
                                 :append :local)))



;; todo-keywords
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; evaluate inline only
  (setq org-export-babel-evaluate 'inline-only)

;; use imagemagick to preview latex
  (setq org-latex-create-formula-image-program 'imagemagick)

  ;; set up tikz as one of the default packages for LaTeX
  (setq org-latex-packges-alist
        (quote (("" "color" t)
                ("" "minted" t)
                ("" "parskip" t)
                ("" "tikz" t))))

;; org babel

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (C . t)
     (scheme . t)
     (ruby . t)
     (go . t)
     (gnuplot . t)
     (dot . t)
     (latex . t)
     (shell . t)
     ;;(jupyter . t)
     ))

  (setq ob-async-no-async-languages-alist '("jupyter-python")) 

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  (setq org-refile-targets '((nil :maxlevel . 1)
                         (org-agenda-files :maxlevel . 1)))

  ;; Capture templates
  (setq org-capture-templates
        '(("w" "Work Todo" entry (file+headline "~/Documents/org/Planner-mdw2022.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("h" "Home Todo" entry (file+headline "~/Documents/org/Planner-home2022.org" "Tasks")
           "* TODO %?\n %i\n %a")))
  (setq org-startup-folded "overview"))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("asm" . "src asm"))
(add-to-list 'org-structure-template-alist '("dot" . "src dot"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("gnpl" . "src gnuplot"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))
(add-to-list 'org-structure-template-alist '("krc" . "src C"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("jpy" . "src jupyter-python"))
(add-to-list 'org-structure-template-alist '("yml" . "src yaml"))

(defun my/org-roam-node-list ()
  "Return all nodes stored in the database as a list of `org-roam-node's."
  (let ((rows (org-roam-db-query
               "SELECT
  id,
  file,
  filetitle,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    filetitle,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      files.title as filetitle,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-title file-title
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :aliases aliases
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (my/org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))


(use-package org-roam
  :demand t
  :straight t
  :init 
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Notes/org-roam/")
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain 
      "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n** Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: Project\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain 
      "\n* Thanks\n\n %?\n\n* WorkingOn\n\n* WorkingTowards\n\n* Excited About\n\n* Woes\n\n* Ideas\n\n* Housekeeping\n\n* Family Planning\n\n* Thanks"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-dailies-capture-today)
         :map org-mode-map
         (("C-c n i" . org-roam-node-insert)
         ("C-M-i" . completion-at-point)))
  :config
  (org-roam-db-autosync-mode)
  (my/org-roam-refresh-agenda-list))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
       org-roam-ui-follow t
       org-roam-ui-update-on-save t
       org-roam-ui-open-on-start nil))

;; center the screen

(defun ad/org-present-start ()
;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun ad/org-present-end ()
  (visual-fill-column-mode 0)
  (visual-line-mode 0))
  
(use-package org-present
  :config
  (add-hook 'org-present-mode-hook 'ad/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'ad/org-present-end))

(use-package org-tree-slide)

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package erc
  :commands erc
  :config
  (setq
   erc-server "irc.libera.chat"
   erc-nick "amccart"
   erc-user-full-name "Adam McCartney"
   erc-track-shorten-start 8
   erc-auto-join-channels '(("irc.libera.chat" "#emacs"))
   erc-kill-buffer-on-part t
   erc-auto-query 'bury
   erc-fill-function 'erc-fill-static
   erc-fill-static-center 20
   erc-track-exclude '("#emacs")
   erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
   erc-track-exclude-server-buffer t))

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

(use-package imenu-list
  :ensure t
  :bind (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
      imenu-list-auto-resize nil))

(use-package pandoc-mode
  :ensure t)

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package yasnippet)

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

(use-package dabbrev
  :ensure t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package dumb-jump
  :ensure t)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package hydra
  :ensure t)

(global-set-key
 (kbd "C-M-o")
  (defhydra hydra-window ()
    "window"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("v" (lambda ()
      (interactive)
      (split-window-right)
      (windmove-right))
      "vert")
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
       "horz")
    ("o" delete-other-windows "one" :color blue)
    ("a" ace-window "ace")
    ("s" ace-swap "swap")
    ("d" ace-delete-window "ace-one" :color blue)
    ("b" ido-switch-buffer "buf")
    ("m" headlong-bookmark "buf")
    ("q" nil "cancel")))

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
    (dap-session-created . (lambda (&_rest) (dap-hydra)))
    (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(require 'dap-gdb-lldb)
(require 'dap-python)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

(use-package treemacs-evil)

(use-package treemacs-projectile)

(use-package lsp-mode 
  :init 
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (c-mode . lsp)
         ((typescript-mode js2-mode web-mode) . lsp)
         (docker-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (lsp-ui-doc-show))

;;(use-package lsp-ivy)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
 (setq which-key-idle-delay 1))

(use-package eglot
  :ensure t
  :hook
  (python-mode-hook . eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

(use-package lsp-pyright
  :after lsp-mode
  :custom
  (lsp-pyright-auto-import-completions nil)
  (lsp-pyright-typechecking-mode "off")
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package magit
  :straight t
  :init (if (not (boundp 'project-switch-commands)) 
        (setq project-switch-commands nil))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;;(use-package lsp-pyright
;;  :straight t 
;;  :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp)))) ;; or lsp-deferred

(use-package gnuplot-mode)
(use-package gnuplot)

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-ofset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(require 'cl-lib)

(use-package slime
  :init 
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :init
  (setq python-shell-interpreter "python3")
  :config
  (setq python-indent-level 4))

(use-package pyvenv
 :ensure t
 :after python-mode)

(use-package python-test
  :after python-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))
  
(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package geiser
  :straight t
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (setq geiser-guile-binary "/usr/bin/guile3.0"))

(use-package geiser-guile
  :straight t)

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

(use-package go-mode
  :hook (go-mode . lsp-deferred))

(use-package ob-go)

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

(use-package yaml-mode
   :mode "\\.ya?ml\\'")

(use-package riscv-mode)

(use-package dockerfile-mode
  :ensure t)

(setq gdb-many-windows t)

(use-package flycheck
  :defer t
  :hook
  (eglot . flycheck-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(setq-default indent-tabs-mode nil)

(use-package zmq)

(use-package jupyter
  :demand t
  :after ob

:config
(add-to-list 'org-babel-load-languages '(jupyter . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("https://nullprogram.com/feed/"
          "https://drewdevault.com/blog/index.xml")))
