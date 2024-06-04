;; -*- mode: scheme; -*-
;; ad-rocky-guix.scm - defines a home config for host

(define-module (admccartney home-config ad-rocky-guix)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (admccartney home-services shell)
  #:use-module (admccartney home-services desktop))

(use-service-modules admin mcron networking spice ssh)
(use-package-modules admin bootloaders fonts guile guile-xyz
                     package-management version-control certs tls rust-apps terminals tmux vim)

(home-environment
 (packages (cons* coreutils
                  git
                  stow

                  ;; cli apps
                  tmux

                  ;; vim + plugins
                  vim-full
                  %base-packages)))

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
