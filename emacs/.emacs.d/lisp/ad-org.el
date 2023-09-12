;;(org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(use-package ob-typescript
  :ensure t)

(use-package ob-go
  :ensure t)

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
     (typescript . t)
     (go . t)
     (gnuplot . t)
     (dot . t)
     (latex . t)
     (shell . t)
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

(provide 'ad-org)


