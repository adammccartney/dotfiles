;; -*- mode: scheme; -*-
;; emacs.scm - defines an emacs home service

;; WIP: we're kind of just copy-and-pasting some emalgam of work by Andrew Tropin and David Wilson

(define-module (admccartney home-services emacs)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services emacs)

  #:export (ad/emacs-packages))

(define ad/emacs-packages
  (list
   emacs-use-package
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
   libtool))
   

(define ad/home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "Applies a personal configuration to Emacs")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-config-profile-service)))
                (default-value #f)))
