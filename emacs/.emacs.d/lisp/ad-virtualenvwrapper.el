;; ad-virtualenvwrapper.el --- a modern, minamlist wrapper for virtualenvwrapper

;; Copyright (C) 2024 Adam McCartney
;; Copyright (C) 2013 - 2015 James J Porter and [contributors](https://github.com/porterjamesj/virtualenvwrapper.el/graphs/contributors)

;; Author: Adam McCartney <adam@mur.at>

;; Commentary:

;; A minimal virtualenvwrapper that leverages transient.
;; Some functionality was adapted from virtualenvwrapper.el, see copyright notice above.

;; Helpers
(defun advenv--get-lhs (tuple)
  "Return lhs of tuple"
  (car tuple))

(defun advenv--get-rhs (tuple)
  "Return rhs of tuple"
  (car (reverse tuple)))

;; Err, should probably become a defcustom maybe...
(defvar virtualenvs-parent (concat (getenv "HOME") "/.virtualenvs/"))


;; internal variables that you probably shouldn't mess with

;; copied from virtualenvwrapper.el
(defvar venv-history nil "The history of venvs we have worked on.")

(defvar venv-current-name nil "Name of current virtualenv.")

(defvar venv-current-dir nil "Directory of current virtualenv.")


;; copy from virtualenv.el
(defvar venv-executables-dir
  (if (eq system-type 'windows-nt) "Scripts" "bin")
  "The name of the directory containing executables. It is system dependent.")

;; copied from virtualenvwrapper.el
(defun venv-get-stripped-path (path)
  "Return what the PATH would look like if we weren't in a
virtualenv. PATH should be a list of strings specifiying directories."
  (-filter
    (lambda (s) (not (s-equals? s (concat venv-current-dir venv-executables-dir))))
    path))


(defun advenv--path->dirlist (path)
  "Get all *named* directories at PATH
exclude the '.' and '..' dirs by defalt
Throws an error if the directory doesn't exist."
  (directory-files path nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))


(defun advenv--get-choices-venv (&optional venvparent)
  "Read the $HOME/.virtualenvs and make a list of all directories
Get a list of direcories that can be used as candidates for
`python-shell-virtualenv-root'"
  (if (null venvparent)
      (setq venvparent virtualenvs-parent))
  (let* ((venv-roots (advenv--path->dirlist venvparent)))
    venv-roots))


;; Simplified version of similar function from virtualenvwrapper.el
(defun advenv--activate-dir (dir)
  "Activate the virtual environment.
Assumes that VENV is a path to a virtual environment.
Prepend VENV/bin to PATH and set the VIRTUAL_ENV variable."
  (setq venv-current-dir (file-name-as-directory dir))

  ;; setup the python shell (note that this should automatically do the next bit, but it appears buggy
  (setq python-shell-virtualenv-root venv-current-dir)

  ;; setup emacs exec-path
  (add-to-list 'exec-path (concat venv-current-dir venv-executables-dir))
  ;; setup the environment for subprocesses
  (let ((path (concat venv-current-dir
                      venv-executables-dir
                      path-separator
                      (getenv "PATH"))))
    (setenv "PATH" path)
    ;; keep eshell path in sync
    (setq eshell-path-env path))
  (setenv "VIRTUAL_ENV" venv-current-dir))

;;  (setq python-shell-process-environment
;;        (list
;;         (format "PATH=%s" (mapconcat
;;                            #'identity
;;                            (reverse
;;                             (cons (getenv "PATH")
;;                                   '("/home/amccartn/.virtualenvs/workflow-test/bin")))
;;                            ":"))
;;         (format "VIRTUAL_ENV=%s" venv-current-dir))))

;;;###autoload
(defun advenv-deactivate ()
  "Deactivate the virtual environment.
Assumes the DIR is a valid path to a virtual environment.
Restore the PATH to it's value before advenv--activate was run.
Unset the VIRTUAL_ENV variable."
  (interactive)
  (setq python-shell-virtualenv-root nil)
  (setq exec-path (venv-get-stripped-path exec-path))
  (setenv "PATH" (s-join path-separator
                  (venv-get-stripped-path
                   (s-split path-separator (getenv "PATH")))))
  (setenv "VIRTUAL_ENV" nil)
  (setq venv-current-name nil)
  (setq venv-current-dir nil)
  (setq eshell-path-env (getenv "PATH"))
  (when (called-interactively-p 'interactive)
    (message "virtualenv deactivated")))



(transient-define-suffix advenv--pre-activate (the-prefix-arg)
  "Set the `python-shell-virtualenv-root' using the PREFIX-ARG
We perform a little bit of manipulation of ARGS once they are determined from context.
Assumes that the virtualenvironment name we want to set appears as the first item in the
ARGS list in the form '--venv=NAME' we parse out the rhs of this expression and use it
to set the desired variable."
  :transient 'transient--do-call
  (interactive "P")
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (venv-name (advenv--get-rhs (split-string (car args) "="))))
    (progn
      (advenv--activate-dir (concat virtualenvs-parent venv-name))
      (message (format "activated: %s" venv-name)))))


;;(transient-define-argument ad-venv--get-pyvenv-root-options-switch ()
;;  :description "Possible virtual environment roots for python"
;;  :class 'transient-switches
;;  :key "v"
;;  :argument-format "%s"
;;  :argument-regexp nil
;;  :choices (advenv--get-choices-venv))

(transient-define-prefix advenv-workon ()
  "Provide a set of choices for python virtual environments.
The selection will be passed to `ad-venv-set-python-shell-virtualenv-root'"
  ["Virtual Environments"
   ("-v" "Virtual Environment" "--venv="
    :always-read t ; don't allow unsetting, just read a new value
    :choices advenv--get-choices-venv)]
  ["Show Environment"
;;   ("s" "show selected environment" tsc-suffix-print-args)
   ("a" "activate selected environment" advenv--pre-activate)])

;;(ad-venv-choices-with-completions)

(provide 'ad-virtualenvwrapper)







