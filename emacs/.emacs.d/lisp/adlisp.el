;; Some custom elisp functions

(defun ad/buffer-insert-sh-output (cmd)
 "Run a shell command and insert the output to the current buffer at point."

 (interactive)
 (insert (format "%s" (shell-command-to-string cmd))))

(defun ad/get-rfc (rfcnum)
  "Get an pull an rfc via http. Plonk the contents into buffer at point."
  (interactive)
  (insert (format "%s" (shell-command-to-string (format "curl https://www.rfc-editor.org/rfc/rfc%s.txt" rfcnum)))))

(provide 'adlisp)


