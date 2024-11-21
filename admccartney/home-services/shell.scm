;; -*- mode: scheme; -*-
;; shell.scm - defines a shell service
(define-module (admccartney home-services shell)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)

  #:export (ad/shell-service))

(define ad/shell-service
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
               ("SSL_CERT_DIR" . "/etc/ssl/certs")  ;; This are configured for foreign distro usage
               ("SSL_CERT_FILE" . "/etc/ssl/certs/ca-certificates.crt")
               ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
               ("TRAIN" . "$HOME/src/github.com/adammccartney/trainlog/docs/training24.md")
               ("MAILDIR" . "$HOME/.mail")))
            
            (bashrc
             `(,(local-file "../../files/bash-prompt")
               ,(local-file "../../files/bash-fzf-keybindings")
               ,(local-file "../../files/bash-git-completion")
               ,(local-file "../../files/bash-functions")
               ,(local-file "../../files/bash-rc"))))))
