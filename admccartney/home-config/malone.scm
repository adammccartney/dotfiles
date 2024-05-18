;; -*- mode: scheme; -*-
;; palais-de-mari.scm - defines a tempalte for a minimal desktop system for guix

(define-module (admccartney home-config malone)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1))


(home-environment
 (packages (specifications->packages (list "coreutils"
                                           "glibc-locales"
                                           "stow"

                                           ;; Scheme
                                           "guile"
                                           "guile-colorized"
                                           "guile-readline"

                                           ;; Emacs
                                           "emacs-no-x-toolkit"  ; TODO figure out why regular emacs borks the desktop cursor
                                           "emacs-geiser"
                                           "emacs-yasnippet"
                                           "emacs-rg"
                                           "libvterm"
                                           "libtool"

                                           ;; CLI apps
                                           "ripgrep"
                                           "git"
                                           "fzf"
                                           "tmux"

                                           ;; vim
                                           "vim"
                                           "vim-guix-vim"
                                           "vim-fugitive"
                                           "vim-nerdtree"
                                           "vim-slime"
                                           
                                           ;; mail
                                           "mu"
                                           "tmux"
                                           "neomutt")))
 
 (services
  (list
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories (list "../../git"
                                "../../emacs"
                                "../../sway"
                                "../../vim"
                                "../../tmux"
                                "../../mail"
                                "../../guile"
                                "../../readline"
                                "../../mutt"))))

   ;; Shell setup
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)

             (bash-profile (list (plain-file "bash-profile" "\
export HISTFILE=$XDG_CACHE_HOME/.bash_history")))

             (aliases '(("train" . "source $HOME/bin/train")
                        ("k" . "kubectl")
                        ;;("slack" . "slack --enable-features=WebRTCPipewireCapturer")
                        ;;("zoom" . "zoom --enable-features . WebRTCPipewireCapturer")
                        ;;("ltucfg" . "source ~/dotfiles/bash/tu.aliases.bash")
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
                ;;("SSL_CERT_DIR" . "/etc/ssl/certs")  ;; This are configured for foreign distro usage
                ;;("SSL_CERT_FILE" . "/etc/ssl/certs/ca-certificates.crt")
                ;;("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
                ("TRAIN" . "$HOME/Code/trainlog/docs/training24.md")
                ("MAILDIR" . "$HOME/.mail")))
             
             (bashrc
              `(,(local-file "../../files/bash-prompt")
                ,(local-file "../../files/bash-profile")
                ,(local-file "../../files/bash-functions")
                ,(local-file "../../files/bash-rc")))))

   ;; setup desktop for vm
   (service xfce-desktop-service-type)
   
   ;; Uncomment the line below to add an SSH server.
   ;;(service openssh-service-type)

   ;; Add support for the SPICE protocol, which enables dynamic
   ;; resizing of the guest screen resolution, clipboard
   ;; integration with the host, etc.
   (service spice-vdagent-service-type)

   ;; Use the DHCP client service rather than NetworkManager.
   (service dhcp-client-service-type)

  ;; Remove some services that don't make sense in a VM.
  (remove (lambda (service)
            (let ((type (service-kind service)))
              (or (memq type
                        (list gdm-service-type
                              sddm-service-type
                              wpa-supplicant-service-type
                              cups-pk-helper-service-type
                              network-manager-service-type
                              modem-manager-service-type))
                  (eq? 'network-manager-applet
                       (service-type-name type)))))
          (modify-services %desktop-services
                           (login-service-type config =>
                                               (login-configuration
                                                (inherit config)
                                                (motd vm-image-motd)))
                           ;; Install and run the current Guix rather than an older
                           ;; snapshot.
                           (guix-service-type config =>
                                              (guix-configuration
                                               (inherit config)
                                               (guix (current-guix)))))))))











