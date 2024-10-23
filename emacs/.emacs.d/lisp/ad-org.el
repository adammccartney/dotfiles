;;(org-format-latex-options (plist-put org-format-latex-options :scale 2.0))


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

(require 'org-roam-export)

(use-package org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;;(use-package ob-typescript)

(use-package ob-go)

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
     (js . t)
;;     (typescript . t)
     (go . t)
     (gnuplot . t)
     (dot . t)
     (latex . t)
     (shell . t)
     (lisp . t)
     ))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(setq org-refile-targets '((nil :maxlevel . 1)
                         (org-agenda-files :maxlevel . 1)))

  ;; Capture templates
(setq org-capture-templates
        '(("w" "Work Todo" entry (file+headline "~/Documents/org/Planner-mdw2022.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("h" "Home Todo" entry (file+headline "~/Documents/org/Planner-home2022.org" "Tasks")
           "* TODO %?\n %i\n %a")))

(setq org-startup-folded "overview")

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("asm" . "src asm"))
(add-to-list 'org-structure-template-alist '("dot" . "src dot"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("gnpl" . "src gnuplot"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("krc" . "src C"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
(add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("sql" . "src sql"))
(add-to-list 'org-structure-template-alist '("doc" . "src docker-build"))
(add-to-list 'org-structure-template-alist '("yml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("lisp" . "src lisp"))

(provide 'ad-org)


