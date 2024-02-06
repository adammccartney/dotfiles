;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.
(define-module (admu home-configuration)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "guile-ics"
                                            "guile-readline"
                                            "emacs"
                                            "haunt"
                                            "emacs-geiser-guile"
                                            "emacs-geiser"
                                            "git"
                                            "neovim"
                                            "guile"
                                            "glibc-locales")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list
    (service home-dotfiles-service-type
             (home-dotfiles-configuration
              (directories (list "emacs" "vim" "nvim" "bash" "git" "tmux")))))))

