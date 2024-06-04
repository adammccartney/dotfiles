;; -*- mode: scheme; -*-
;; ad-rocky-guix.scm - defines a home config for host

(define-module (admccartney home-config ad-rocky-guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (admccartney home-services shell)
  #:use-module (admccartney home-services desktop))

(use-service-modules mcron networking spice ssh)
(use-package-modules bootloaders fonts package-management)

(home-environment
 (packages (cons*
            coreutils
            glibc-locales
            git
            mcron
            nss-certs
            openssl
            stow

            ;; cli apps
            ripgrep
            git
            fzf
            tmux
            tree

            ;; guile scheme
            guile
            guile-colorized
            guile-readline
            guile-ics
            guile-ssh

            ;; vim + plugins
            vim-full
            vim-guix-vim
            vim-fugitive
            vim-nerdtree
            vim-slime)))
 
 (services
  (append
   (list (service home-dotfiles-service-type
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

         ;; Shell service
         `(,@ad/shell-service)))))
