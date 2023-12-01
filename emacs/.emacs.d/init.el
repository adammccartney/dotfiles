;; min.init.el --- -*- lexical-binding: t; -*-

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

;; Some global settings
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

(load-theme 'leuven-dark)

;; ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :ensure t)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; ignore anything pulled in from gnome
(define-key special-event-map [config-changed-event] 'ignore)

;; Custom lispy stuff
(push "~/.emacs.d/lisp" load-path)  
(require 'adlisp)
(require 'unannoy) ;; handy stuff from Chris Wellons
(require 'faust-mode) ;; from rukano
(setq auto-mode-alist (cons '("\\.dsp$" . faust-mode) auto-mode-alist))

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


;; Package repos
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

;; Install and load `quelpa-use-package'.
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)


;; Set up use-package
(eval-when-compile
  (require 'use-package))

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
  :ensure t
  :init 
  (setq org-roam-v2-ack t)
  (setq org-roam-directory "~/Notes/org-roam/")
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
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
  (setq org-roam-dailies-capture-templates
   '(("d" "default" plain 
      "\n* Thanks\n\n %?\n\n* WorkingOn\n\n* WorkingTowards\n\n* Excited About\n\n* Woes\n\n* Ideas\n\n* Housekeeping\n\n* Family Planning\n\n* Thanks"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")
      :unnarrowed t)
     ("a" "autobiography" plain
      "\n* Event\n\n* Analysis\n\n** Event\n\n** On Experience"
    :if-new (file+head "%<%Y-%m-%d>-autobiography.org"
                       "#+title: %<%Y-%m-%d>-autobiography\n")
    :unarrowed t)
   ("f" "future writing" plain
    "\n* OneThing\n\n* Learning\n\n** Habits\n\n** Social\n\n** Family\n\n** Career"
    :if-new (file+head "%<%Y-%m-%d>-future-writing.org"
                       "#+title: %<%Y-%m-%d>-future-writing\n")
    :unarrowed t)))
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
  :ensure t
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
       org-roam-ui-follow t
       org-roam-ui-update-on-save t
       org-roam-ui-open-on-start nil))


;; Load custom org config after org-roam
(require 'ad-org)

;; UI stuff
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; Drawing
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;; Writing (markdown)
(use-package markdown-mode
  :ensure t
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

;; Useability
(use-package all-the-icons-dired
  :ensure t)

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

;; Window management
(winner-mode)
(global-set-key (kbd "M-o") 'other-window)

;; Completions
(use-package helm
  :ensure t
  :init
  ;; a good chunk of the following config is from
  ;; http://tuhdo.github.io/helm-intro.html
  ;; default "C-x c" is close to quit "C-x C-c"
  ;; "C-h"
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
    
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (helm-autoresize-mode 1))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
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

(use-package company
  :ensure t
  :config
  (setq company-global-mode t)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay
        (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (setq company-global-modes '(not org-mode))
  (setq company-selection-wrap-around t))
   
(use-package emacs
  :ensure t
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

;; tree-sitter setup

;; lsp
(use-package eglot
  :ensure t
  :hook
  (python-mode-hook . eglot-ensure)
  :config
  (setq eglot-autoshutdown t))


(use-package magit
  :ensure t
  :init (if (not (boundp 'project-switch-commands)) 
        (setq project-switch-commands nil))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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
  :ensure t
  :config (show-paren-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4")
  (setf rainbow-delimiters-max-face-count 1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error)
  (set-face-foreground 'rainbow-delimiters-depth-1-face "snow4"))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

(use-package browse-url
  :ensure t
  :defer t
  :init
  (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "firefox")
    (setf browse-url-browser-function #'browse-url-firefox)))

;; lsp with eglot
(use-package eglot
  :ensure t
  :hook
  (python-ts-mode-hook . eglot-ensure)
  :config
  (setq eglot-autoshutdown t))

;; geiser is pretty cool, in fairness

(use-package geiser
  :ensure t
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (setq geiser-guile-binary "/usr/bin/guile3.0"))

(use-package geiser-guile
  :ensure t)

;; ensure some c related goodness
(use-package cc-mode
  :ensure t
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
  :ensure t
  :defer t
  :mode ("\\.nasm$" "\\.asm$" "\\.s$")
  :config
  (add-hook 'nasm-mode-hook (lambda () (setf indent-tabs-mode t))))

(use-package asm-mode
  :ensure t
  :defer t
  :init
  (add-hook 'asm-mode-hook (lambda () (setf indent-tabs-mode t
                                            tab-always-indent t))))

(use-package x86-lookup
  :ensure t
  :defer t
  :init
  (setq x86-lookup-pdf '"~/Documents/bookstaging/325383-sdm-vol-2abcd.pdf")
  (global-set-key (kbd "C-h x") #'x86-lookup))

(use-package dockerfile-mode
  :ensure t)

(use-package paredit
  :ensure t)

(use-package eshell
  :ensure t
  :defer t
  :bind ([f1] . eshell-as)
  :init
  (setf eshell-directory-name (locate-user-emacs-file "local/eshell"))
  :config
  (add-hook 'eshell-mode-hook ; Bad, eshell, bad!
            (lambda ()
              (define-key eshell-mode-map (kbd "<f1>") #'quit-window))))

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("https://nullprogram.com/feed/"
          "https://drewdevault.com/blog/index.xml")))

;; gdb setup
(setq gdb-many-windows t)

; EasyPG for encrypting files
(require 'epa-file)
(epa-file-enable)


;; Supercollider support
(add-to-list 'load-path "~/.local/src/supercollider/scel/el")
(require 'sclang)

;; matrix
(use-package ement
  :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

; Tramp for tramping
(tramp-set-completion-function
 "ssh" (append (tramp-get-completion-function "ssh")
               (mapcar (lambda (file) `(tramp-parse-sconfig ,file))
                       (directory-files
                        "~/.ssh/"
                        'full directory-files-no-dot-files-regexp))))
(customize-set-variable 'tramp-default-method "ssh")
;;(setq debug-on-error t
;;      debug-on-signal t)
(customize-set-variable 'tramp-debug-to-file t)
(setq tramp-verbose 6)

(use-package slime
  :ensure t
  :init 
  (setq inferior-lisp-program "/usr/bin/sbcl")
  :config
  (add-hook 'slime-load-hook
            (lambda ()
              (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))))

;; Writing
(setq dictionary-server "localhost")
;; mandatory, as the dictionary misbehaves!
(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
   '("^\\*Dictionary\\*" display-buffer-in-side-window
     (side . left)
     (window-width . 50)))

;; close to ispell lookup
(global-set-key (kbd "M-§") #'dictionary-lookup-definition)

