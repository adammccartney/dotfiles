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


;; Conversion functions

(defun elisp->json-list-str (lst)
  ;; Convert an elisp list to a json formatted list
  ;; i.e. string output as
  ;; [item1, item2, ..., itemn]

  (defun iter-elisp->json-list-str (lst res)
    ;; Iterate until end of lst, then return res
    (if (null lst)
        (progn
          res)
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
