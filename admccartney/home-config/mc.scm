;; -*- mode: scheme; -*-
;; malloy.scm - defines a home config for host
(define-module (admccartney home-config mc)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (admccartney home-services shell))

(use-service-modules desktop mcron networking spice ssh xorg sddm)
(use-package-modules bootloaders fonts
                     package-management xdisorg xorg)

(home-environment
 (packages (specifications->packages (list 
   "coreutils"
   "glibc-locales"
   "curl"
   "openssl"
   "stow"
   "tree"
   "glibc-locales"
   "fontconfig"

   ;; passwords
   "password-store"

   ;; build tools
   "cmake"

   ;; tty
   "foot"
   "zstd"
   
   ;; cli apps
   "ripgrep"
   "git"
   "git-crypt"
   "fzf"
   "tmux"
   
   ;; guile scheme
   "guile"
   "guile-colorized"
   "guile-readline"
   "nyacc"

   ;; go
   "go@1.23.0"

   ;; Emacs
   "emacs"
   "emacs-use-package"
   "emacs-geiser"
   "emacs-geiser-guile"
   "emacs-no-littering"
   "emacs-consult"
   "emacs-all-the-icons-dired"
   "emacs-yasnippet"
   "emacs-rg"

   ;; Fonts
   "emacs-ef-themes"
   "emacs-fontaine"
   "emacs-tmr"
   "emacs-all-the-icons"
   "emacs-graphviz-dot-mode"
   "emacs-markdown-mode"

   ;; Org mode
   "emacs-org"
   "emacs-org-roam"
;;   "emacs-org-roam-ui"
;;   "emacs-ob-typescript"
   "emacs-ob-go"
   "emacs-org-roam-ui"
   
   ;; Completions
   "emacs-vertico"
   "emacs-marginalia"
   "emacs-orderless"
   "emacs-which-key"

   "emacs-dumb-jump"

   ;; Development
   "emacs-go-mode"
   "emacs-eglot"
   "emacs-magit"
   "emacs-yasnippet"
   "emacs-yasnippet-snippets"
   "emacs-rainbow-delimiters"
   "emacs-rainbow-mode"
   "emacs-nasm-mode"
   "emacs-rust-mode"
   "emacs-dockerfile-mode"
   "emacs-paredit"
   "emacs-slime"
   "emacs-nix-mode"
   "emacs-pg"
   "emacs-vterm"
   "emacs-guix"
   "emacs-yaml-mode"

   "emacs-ement"
   "emacs-elfeed"

   ;; vim + plugins
   "vim-full"
   "vim-guix-vim"
   "vim-fugitive"
   "vim-nerdtree"
   "vim-slime"

   ;; mail
   "mu"
   "isync"
   "imapfilter"
   "mutt"
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

         ;; XDG_CONFIG_DIR services
         (service home-xdg-configuration-files-service-type
                  ;;-> $XDG_CONFIG_DIR/foot/foot.init
                  `(("foot/foot.ini" ,(local-file "../../config/.config/foot/foot.ini"))))

         ;; Shell service
         `(,@ad/shell-service)))))
