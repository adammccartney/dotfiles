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
  #:use-module (admccartney home-services shell))

(use-service-modules guix admin sysctl desktop linux mcron
                     networking spice ssh xorg ssh)

(use-package-modules bootloaders fonts guile guile-xyz linux bash emacs gnome
                     package-management version-control certs tmux vim)

(home-environment
 (packages (cons* coreutils
                  git
                  stow

                  ;; cli apps
                  tmux

                  ;; vim + plugins
                  vim-full

                  ;; emacs
                  emacs
                  emacs-geiser
                  emacs-geiser-guile
                  emacs-yasnippet
                  emacs-rg

                  ;; Fonts
                  emacs-ef-themes
                  emacs-fontaine
                  emacs-all-the-icons
                  emacs-graphviz-dot-mode
                  emacs-markdown-mode

                  ;; Org mode
                  emacs-org
                  emacs-org-roam
                  emacs-org-roam-ui
                  emacs-ob-typescript
                  emacs-ob-go
                  
                  ;; Completions
                  emacs-vertico
                  emacs-marginalia
                  emacs-orderless
                  emacs-which-key

                  emacs-dumb-jump

                  ;; Development
                  emacs-go-mode
                  emacs-eglot
                  emacs-magit
                  emacs-yasnippet
                  emacs-yasnippet-snippets
                  emacs-rainbow-delimiters
                  emacs-rainbow-mode
                  emacs-nasm-mode
                  emacs-rust-mode
                  emacs-dockerfile-mode
                  emacs-paredit
                  emacs-slime
                  emacs-nix-mode
                  emacs-pg
                  emacs-vterm
                  emacs-guix
                  emacs-yaml-mode

                  emacs-ement
                  emacs-elfeed

                  ;; TODO: check if we really need these
                  libvterm
                  libtool
                  
                  %base-packages))

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
