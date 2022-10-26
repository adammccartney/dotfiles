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

(push "~/.emacs.d/lisp" load-path)  
(require 'unannoy)
(require 'ad-mail)
(require 'adlisp)
(require 'mu4e)

(set-default-coding-systems 'utf-8)

(server-start)

(global-set-key (kbd "C-x k") #'kill-this-buffer)

(global-set-key (kbd "C-M-u") 'universal-argument)

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

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi)
  ;; Main typeface
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 110)
  ;; Proportionally spaced typeface
  (set-face-attribute 'variable-pitch nil :family "DejaVu Serif" :height 1.0)
  ;; Monospaced typeface
  (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 1.0)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers t
        modus-themes-intense-mouseovers nil
        modus-themes-deuteranopia t
        modus-themes-tabs-accented t
        modus-themes-variable-pitch-ui nil
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        modus-themes-fringes nil ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers nil

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', a natural number for extra padding (or a cons cell
        ;; of padding and NATNUM), and a floating point for the height of
        ;; the text relative to the base font size (or a cons cell of
        ;; height and FLOAT)
        modus-themes-mode-line '(3d accented borderless (padding . 4) (height . 0.9))

        ;; Same as above:
        ;; modus-themes-mode-line '(accented borderless 4 0.9)

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup '(background italic)

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax nil

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(underline accented)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline background)

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat', `accented',
        ;; `faint', `variable-pitch', `underline', `all-buttons', the
        ;; symbol of any font weight as listed in `modus-themes-weights',
        ;; and a floating point number (e.g. 0.9) for the height of the
        ;; button's text.
        modus-themes-box-buttons '(variable-pitch flat faint 0.9)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', `text-also' WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))

        modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(bg-only no-extend)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs 'desaturated

        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch 1.3))
          (header-date . (grayscale workaholic bold-today 1.1))
          (event . (accented varied))
          (scheduled . uniform)
          (habit . traffic-light))

        modus-themes-headings ; this is an alist: read the manual or its doc string
        '((1 . (overline background variable-pitch 1.3))
          (2 . (rainbow overline 1.1))
          (t . (semibold)))))

(use-package spacegray-theme
  :defer t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;;(load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; treemacs 
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-set-faces
 `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
 `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))

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
  (doom-modeline-lsp t)
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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package paren
  :config (show-paren-mode))

(use-package rainbow-delimiters
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
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))


(use-package javadoc-lookup
  :defer t
  :bind ("C-h j" . javadoc-lookup)
  :config
  (ignore-errors
    (setf javadoc-lookup-cache-dir (locate-user-emacs-file "local/javadoc"))))

(use-package browse-url
  :defer t
  :init
  (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "firefox")
    (setf browse-url-browser-function #'browse-url-firefox)))

(use-package bufler
  :disabled
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
       (kbd "RET") 'bufler-list-buffer-switch
       (kbd "M-RET") 'bufler-list-buffer-peek
       "D" 'bufler-list-buffer-kill)
  (setf bufler-groups
        (bufler-defgroups
         ;; Subgroup collecting all named workspaces
         (group (auto-workspace))
         ;; Subgoup collecting buffers in a projectile project.
         (group (auto-projectile))
         (group
            ;; Group all 
          (group-or "Help/Info"
                     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                     (mode-match "*Info*" (rx bos "info-"))))
         (group
          ;; Collect all special buffers
           (group-and "*Special*"
                      (name-match "**Special**"
                                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*"))))
          ;; group remaining buffers by major mode
         (auto-mode))))

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
     (shell . t)))

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
  (setf sentence-end-double-space nil
        markdown-indent-on-enter nil
        markdown-command
        "pandoc -f markdown -t html5 -s --self-contained --smart"))

(use-package consult
  :ensure t)

(use-package vertico
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package yasnippet)

(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-other-buffers t) ; search buffers with the same major mode
  :hook
  (dashboard-after-initialize . global-company-mode)
  :config
  (add-to-list 'company-begin-commands 'backwards-delete-char-untabify)
  

  ;; Show YASnippet snippets in company

  (defun fk/company-backend-with-yas (backend)
    "Add ':with company-yasnippet' to the given company backend."
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend)
                  backend
                (list backend))
              '(:with company-yasnippet))))

  (defun fk/company-smart-snippets (fn command &optional arg &rest _)
    "Do not show yasnippet candidates after dot."
    ;;Source:
    ;;https://www.reddit.com/r/emacs/comments/7dnbxl/how_to_temporally_filter_companymode_candidates/
    (unless (when (and (equal command 'prefix) (> (point) 0))
              (let* ((prefix (company-grab-symbol))
                     (point-before-prefix (if (> (- (point) (length prefix) 1) 0)
                                              (- (point) (length prefix) 1)
                                            1))
                     (char (buffer-substring-no-properties point-before-prefix (1+ point-before-prefix))))
                (string= char ".")))
      (funcall fn command arg)))

  ;; TODO: maybe show snippets at first?
  (defun fk/company-enable-snippets ()
    "Enable snippet suggestions in company by adding ':with
company-yasnippet' to all company backends."
    (interactive)
    (setq company-backends (mapcar 'fk/company-backend-with-yas company-backends))
    (advice-add 'company-yasnippet :around 'fk/company-smart-snippets))

  (fk/company-enable-snippets))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

(use-package treemacs-evil)

(use-package treemacs-projectile)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
 (setq which-key-idle-delay 1))

(use-package eglot
  :ensure t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/.local/src")
    (setq projectile-project-search-path '("~/.local/src" "~/Code" "~/.local/src/mdw")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

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
  :init
  (setq python-shell-interpreter "python3")
  :config
  (setq python-indent-level 4))

(use-package pyvenv
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
  (setq geiser-guile-binary "/usr/bin/guile"))

(use-package geiser-guile
  :straight t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
          ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)           
  (add-hook 'clojure-mode-hook #'smartparens-mode)       
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode))

(use-package cider
  :ensure t
  :defer t)

(use-package cc-mode
  :defer t
  :init
  (defun my/c-hook ()
    (setf c-basic-offset 8)   ;; follow linux kernel style guide
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

(use-package yaml-mode
   :mode "\\.ya?ml\\'")

(use-package dockerfile-mode
  :ensure t)

(setq gdb-many-windows t)

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(setq-default indent-tabs-mode nil)

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
        '("https://nullprogram.com/feed/")))
