;; -*- mode: scheme; -*-
;; ad-rocky-guix.scm - defines a home config for host

(define-module (admccartney home-config ad-rocky-guix)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (admccartney home-services shell)
  #:use-module (admccartney home-services emacs))

(use-service-modules guix admin sysctl desktop linux mcron
                     networking spice ssh xorg ssh)

(use-package-modules bootloaders fonts guile guile-xyz linux bash emacs gnome
                     package-management version-control certs tmux vim)

(home-environment
 (packages (append
            (cons* coreutils
                   git
                   stow

                   ;; cli apps
                   tmux

                   ;; vim + plugins
                   vim-full
                   %base-packages)
            ad/emacs-packages)

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
