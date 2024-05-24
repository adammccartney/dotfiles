;; Some custom elisp functions

(defun ad/buffer-insert-sh-output (cmd)
 "Run a shell command and insert the output to the current buffer at point."

 (interactive)
 (insert (format "%s" (shell-command-to-string cmd))))

(defun ad/get-rfc (rfcnum)
  "Get an pull an rfc via http. Plonk the contents into buffer at point."
  (interactive)
  (insert (format "%s" (shell-command-to-string (format "curl https://www.rfc-editor.org/rfc/rfc%s.txt" rfcnum)))))



(defun ad/get-lhs (tuple)
  "Return lhs of tuple"
  (car tuple))

(defun ad/get-rhs (tuple)
  "Return rhs of tuple"
  (car (reverse tuple)))



;; Conversion functions

(defun elisp->json-list-str (lst)
  "Convert an elisp list to a json formatted list
i.e. string output as [item1, item2, ..., itemn]"
  (defun iter-elisp->json-list-str (lst res)
    ;; Iterate until end of lst, then return res
    (if (null lst)
          res
      (progn
        (if (= 1 (length lst))
            (setq res (format "%s\"%s\"]" res (car lst)))
          (setq res (format "%s\"%s\"," res (car lst))))
        (iter-elisp->json-list-str (cdr lst) res))))
  (iter-elisp->json-list-str lst "["))


(defun test-elisp->json-list-str ()
  (setq tlst (list "this"))
  (setq lst (list "Network Boot" "UEFI IPv4: Network 00 at Riser 01 Slot 02" "UEFI HTTPv6: Network 00 at Riser 01 Slot 02" "UEFI HTTPv4: Network 00 at Riser 01 Slot 02" "UEFI HTTPv6: Network 00 at Riser 02 Slot 02" "UEFI HTTPv4: Network 00 at Riser 02 Slot 02" "UEFI IPv4: Network 00 at Riser 02 Slot 02" "UEFI IPv6: Network 00 at Riser 02 Slot 02" "UEFI HTTPv6: Network 01 at Riser 02 Slot 02" "UEFI HTTPv4: Network 01 at Riser 02 Slot 02" "UEFI IPv4: Network 01 at Riser 02 Slot 02" "UEFI IPv6: Network 01 at Riser 02 Slot 02" "UEFI HTTPv6: Intel I210 Network 00 at Baseboard" "UEFI HTTPv4: Intel I210 Network 00 at Baseboard" "UEFI IPv4: Intel I210 Network 00 at Baseboard" "UEFI IPv6: Intel I210 Network 00 at Baseboard" "Launch EFI Shell" "Enter Setup" "Boot Device List"))
  (setq res "[\"Network Boot\",\"UEFI IPv4: Network 00 at Riser 01 Slot 02\",\"UEFI HTTPv6: Network 00 at Riser 01 Slot 02\",\"UEFI HTTPv4: Network 00 at Riser 01 Slot 02\",\"UEFI HTTPv6: Network 00 at Riser 02 Slot 02\",\"UEFI HTTPv4: Network 00 at Riser 02 Slot 02\",\"UEFI IPv4: Network 00 at Riser 02 Slot 02\",\"UEFI IPv6: Network 00 at Riser 02 Slot 02\",\"UEFI HTTPv6: Network 01 at Riser 02 Slot 02\",\"UEFI HTTPv4: Network 01 at Riser 02 Slot 02\",\"UEFI IPv4: Network 01 at Riser 02 Slot 02\",\"UEFI IPv6: Network 01 at Riser 02 Slot 02\",\"UEFI HTTPv6: Intel I210 Network 00 at Baseboard\",\"UEFI HTTPv4: Intel I210 Network 00 at Baseboard\",\"UEFI IPv4: Intel I210 Network 00 at Baseboard\",\"UEFI IPv6: Intel I210 Network 00 at Baseboard\",\"Launch EFI Shell\",\"Enter Setup\",\"Boot Device List\"]")
  (elisp->json-list-str lst))

(defun ad/path->dirlist (path)
  "Get all *named* directories at PATH
exclude the '.' and '..' dirs by defalt
Throws an error if the directory doesn't exist."
  (directory-files path nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

(defun ad/iter-print-opts (lst)
  ;; Iterate through a list and print the options
  (if (null lst)
      '()
    (progn
      (print (car lst))
      (ad/iter-print-opts (cdr lst)))))

;; Python stuff
(defun ad/get-choices-venv (&optional venvparent)
  "Read the ~/.virtualenvs and make a list of all directories
Get a list of direcories that can be used as candidates for
`python-shell-virtualenv-root'"
  (if (null venvparent)
      (setq venvparent "~/.virtualenvs"))
  (let* ((venv-roots (ad/path->dirlist venvparent)))
    venv-roots))


(transient-define-suffix ad--venv-set-python-shell-virtualenv-root (the-prefix-arg)
  "Set the `python-shell-virtualenv-root' using the PREFIX-ARG
We perform a little bit of manipulation of ARGS once they are determined from context.
Assumes that the virtualenvironment name we want to set appears as the first item in the
ARGS list in the form '--venv=NAME' we parse out the rhs of this expression and use it
to set the desired variable."
  :transient 'transient--do-call
  (interactive "P")
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (venv-name (ad/get-rhs (split-string (car args) "="))))
    (progn
      (setq python-shell-virtualenv-root (format "~/.virtualenv/%s/" venv-name))
      (message (format "python-shell-virtualenv-root: %s" python-shell-virtualenv-root)))))


(transient-define-argument ad-venv--get-pyvenv-root-options-switch ()
  :description "Possible virtual environment roots for python"
  :class 'transient-switches
  :key "v"
  :argument-format "%s"
  :argument-regexp nil
  :choices (ad/get-choices-venv))

(transient-define-prefix ad-venv-choices-with-completions ()
  "Provide a set of choices for python virtual environments.
The selection will be passed to `ad-venv-set-python-shell-virtualenv-root'"
  ["Virtual Environments"
   ("-v" "Virtual Environment" "--venv="
    :always-read t ; don't allow unsetting, just read a new value
    :choices ad/get-choices-venv)]
  ["Show Environment"
   ("s" "show selected environment" tsc-suffix-print-args)
   ("e" "enable selected environment" ad--venv-set-python-shell-virtualenv-root)])

(ad-venv-choices-with-completions)

(provide 'adlisp)

