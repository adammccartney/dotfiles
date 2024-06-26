(use-package mu4e
:defer 20
:config

;; Turn off auto fill
(add-hook 'mu4e-compose-mode-hook 'turn-off-auto-fill)
;; automatically add a gpg signature to every email (signed as adam@mur.at) 
(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;; Refresh mail using isync every 10 minutes
(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/.mail")

(setq mu4e-attachment-dir
    (lambda (fname mtype)
    (cond 
        ;; docfiles go to ~/Documents/email-attachments
        ((and fname (string-match "\\.doc$" fname)) "~/Documents/email-attachments")
        ((and fname (string-match "\\.pdf$" fname)) "~/Documents/email-attachments")
        (t "~/Downloads/email-attachments")))) ;; everything else

;; use msmtp to send mail
(setq sendmail-program "/usr/bin/msmtp"
      send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)


;; Make sure that moving a message (like to Trash) causes the
;; message to get a new file name.  This helps to avoid the
;; dreaded "UID is N beyond highest assigned" error.
;; See this link for more info: https://stackoverflow.com/a/43461973
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-contexts
        `(,(make-mu4e-context
            :name "murat"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/murat" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Adam McCartney")
                    (user-mail-address . "adam@mur.at")
                    ;;(smtpmail-smtp-server . "smtp.mur.at")
                    ;;(smtpmail-smtp-service . 465)
                    ;;(smtpmail-stream-type . starttls)
                    (mu4e-sent-folder . "/murat/Sent")
                    (mu4e-trash-folder . "/murat/Trash")
                    (mu4e-drafts-folder . "/murat/Drafts")
                    (mu4e-refile-folder . "/murat/Archives")
                    (mu4e-sent-messages-behavior . sent)
                    (mu4e-compose-signature .
                                            (concat
                                                "Adam McCartney | https://admccartney.mur.at \n"
                                                "/\n"))))
            ,(make-mu4e-context
            :name "tuw"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/tuw" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Adam McCartney")
                    (user-mail-address . "adam.mccartney@tuwien.ac.at")
                    ;;(smtpmail-smtp-server . "mail.intern.tuwien.ac.at")
                    ;;(smtpmail-smtp-service . 587)
                    ;;(smtpmail-stream-type . starttls)
                    (mu4e-send-folder . "/tuw/Sent Items")
                    (mu4e-trash-folder . "/tuw/Deleted Items")
                    (mu4e-junk-folder . "/tuw/Junk Email")
                    (mu4e-drafts-folder . "/tuw/Drafts")
                    (mu4e-refile-folder . "/tuw/Cabinet")
                    (mu4e-sent-message-behavior . sent)
                    (mu4e-compose-signature .
                                            (concat
                                             "Adam McCartney - Technische Universität Wien, Service Unit of High Performance Computing, E020-04\n"
                                             "A-1040 Wien, Operngasse 11/E020 (DF0532)  -   Tel: +43 664 605883001\n"
                                             "//-----------------------------------------------------------------------------------------------\n"))))
        ))
(setq mu4e-context-policy 'pick-first)
;; Prevent mu4e from permanently deleting trashed items
;; This snippet was taken from the following article:
;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
(defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
(setq mu4e-marks (remove-nth-element 5 mu4e-marks))
(add-to-list 'mu4e-marks
            '(trash
                :char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                :action (lambda (docid msg target)
                        (mu4e~proc-move docid
                                        (mu4e~mark-check-target target) "-N"))))

;; Display options
(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses 't)

;; Composing mail
(setq mu4e-compose-dont-reply-to-self t)

;; Use mu4e for sending e-mail
(setq message-send-mail-function 'smtpmail-send-it)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Use a specific key for signing by referencing its thumbprint
(setq mml-secure-openpgp-signers '("C5BF27EE0290CDE5BC8A8801A5FCE0B0A42EFDA8"))

;; Start mu4e in the background so that it syncs mail periodically
(mu4e t))

(provide 'ad-mail)
