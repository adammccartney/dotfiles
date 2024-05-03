(define-module (minimal-home-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp))

(home-environment
 (packages (specifications->packages (list "coreutils"
                                           "glibc-locales"
                                           "guile"
                                           "guile-colorized"
                                           "guile-readline"
                                           "guile-ics"
                                           "guile-ssh"
                                           "emacs-no-x-toolkit"  ; TODO figure out why regular emacs borks the desktop cursor
                                           "emacs-geiser"
                                           "emacs-yasnippet"
                                           "emacs-rg"
                                           "ripgrep"
                                           "git"
                                           "fzf"
                                           "vim"
                                           "vim-guix-vim"
                                           "vim-fugitive"
                                           "vim-nerdtree"
                                           "vim-slime"
                                           "mu"
                                           "tmux"
                                           "neomutt"
                                           "libvterm"
                                           "libtool")))
 
 (services
  (list
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories (list "git"
                                "emacs"
                                "vim"
                                "tmux"
                                "mail"
                                "guile"
                                "mutt"))))

   ;; Shell setup
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)

             (bash-profile (list (plain-file "bash-profile" "\
export HISTFILE=$XDG_CACHE_HOME/.bash_history")))

             (aliases '(("train" . "source $HOME/bin/train")
                        ("k" . "kubectl")
                        ("slack" . "slack --enable-features=WebRTCPipewireCapturer")
                        ("zoom" . "zoom --enable-features . WebRTCPipewireCapturer")
                        ("ltucfg" . "source ~/dotfiles/bash/tu.aliases.bash")
                        ("ls0" . "find . -maxdepth 1 -print0")
                        ("mk" . "minikube kubectl --")
                        ("bup" . "bkhome-wrapper.sh")
                        ("wgu" . "sudo wg-quick up wg0")
                        ("wgd" . "sudo wg-quick down wg0")
                        ("emacs" . "XMODIFIERS='' emacs &")))
             
             (environment-variables
              '(("TERMINFO" . "/usr/share/terminfo")
                ("EDITOR" . "vim")
                ("MANWIDTH" . "80")
                ("SSL_CERT_DIR" . "/etc/ssl/certs")  ;; This are configured for foreign distro usage
                ("SSL_CERT_FILE" . "/etc/ssl/certs/ca-certificates.crt")
                ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
                ("TRAIN" . "$HOME/Code/trainlog/docs/training24.md")
                ("MAILDIR" . "$HOME/.mail")))
             
            (bashrc
             `(,(local-file "files/bash-prompt")
               ,(local-file "files/bash-functions")
               ,(local-file "files/bash-rc"))))))))








