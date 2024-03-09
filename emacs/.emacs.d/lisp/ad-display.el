;; Stuff related to display functionality of emacs

(setq display-buffer-alist nil)

(setq display-buffer-alist
      '(
        ;; Anatomy of an entry
        ;; ( BUFFER-MATCHER
        ;;   LIST-OF-DISPLAY-FUNCTIONS
        ;;   &optional PARAMETERS)

        ("\\*Occur\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected
          ;; params
          (dedicated . t) (window-height . fit-window-to-buffer)))
        ("\\*Org Src .*\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected
          ;; params
          (dedicated . t) (window-height . fit-window-to-buffer)))
        ))

(provide 'ad-display)
