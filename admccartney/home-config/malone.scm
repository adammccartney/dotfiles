;; -*- mode: scheme; -*-
;; malone.scm - defines a home config for host

(define-module (admccartney home-config malone)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (admccartney home-services shell)
  #:use-module (admccartney home-services desktop))

(use-service-modules desktop mcron networking spice ssh xorg sddm)
(use-package-modules bootloaders fonts
                     package-management xdisorg xorg)

(home-environment
 (packages (specifications->packages (list "coreutils"
                                           "glibc-locales"
                                           "stow"

                                           ;; desktop
                                           "sway"

                                           ;; Browsers
                                           "password-store"

                                           ;; General utilities
                                           "curl"
                                           "wget"
                                           "openssh"
                                           "zip"
                                           "unzip"
                                           "trash-cli"
                                           "cmake"
                                           
                                           ;; Scheme
                                           "guile"
                                           "guile-colorized"
                                           "guile-readline"

                                           ;; Emacs
                                           "emacs-no-x-toolkit"  ; TODO figure out why regular emacs borks the desktop cursor
                                           "emacs-geiser"
                                           "emacs-geiser-guile"
                                           "emacs-yasnippet"
                                           "emacs-rg"
                                           "emacs-dumb-jump"
                                           "libvterm"
                                           "libtool"

                                           ;; CLI apps
                                           "ripgrep"
                                           "git"
                                           "fzf"
                                           "tmux"

                                           ;; mail
                                           "mu"
                                           "tmux"
                                           "neomutt"                                           
                                           )))
 
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


