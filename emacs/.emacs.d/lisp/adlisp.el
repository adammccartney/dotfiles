;; Some custom elisp functions

(defun ad/buffer-insert-sh-output (cmd)
 "Run a shell command and insert the output to the current buffer at point"

 (interactive)
 (insert (format "%s" (shell-command-to-string cmd))))

(provide 'adlisp)
