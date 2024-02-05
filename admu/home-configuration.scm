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
     (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("!P" . "PS1='\\''# '\\''")
                              ("bup" . "bkhome-wrapper.sh")
                              ("guile" . "/home/amccartn/.guix-profile/bin/guile")
                              ("k" . "kubectl")
                              ("ls" . "ls --color=auto")
                              ("ls0" . "find . -maxdepth 1 -print0")
                              ("ltucfg" . "source ~/dotfiles/bash/tu.aliases.bash")
                              ("mk" . "minikube kubectl --")
                              ("slack" . "slack --enable-features=WebRTCPipewireCapturer")
                              ("train" . "source /home/amccartn/bin/train")
                              ("vpnui" . "/opt/cisco/anyconnect/bin/vpnui")
                              ("wgd" . "sudo wg-quick down wg0")
                              ("wgu" . "sudo wg-quick up wg0")
                              ("zoom" . "zoom --enable-features=WebRTCPipewireCapturer")))
                   (bashrc (list (local-file "./.bashrc" "bashrc")))
                   (bash-profile (list (local-file "./.bash_profile"
                                                   "bash_profile"))))))))
