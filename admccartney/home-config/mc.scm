;; -*- mode: scheme; -*-
;; malloy.scm - defines a home config for host
(define-module (admccartney home-config mc)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services guix)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (admccartney home-services shell))

(use-service-modules desktop mcron networking spice ssh xorg sddm)
(use-package-modules bootloaders fonts
                     package-management xdisorg xorg)

(home-environment
 (packages (specifications->packages (list 
   "coreutils"
   "curl"
   "openssl"
   "stow"
   "tree"
   "glibc-locales"

   ;; wayland extras
   "fuzzel"

   ;; audio
   "reaper"

   ;; passwords
   "password-store"

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
   "xurls"

   ;; Emacs
   "emacs-no-x-toolkit"
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
;;   "emacs-ob-typescript"
   "emacs-ob-go"
   
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
   "emacs-hcl-mode"
   "emacs-terraform-mode"

   "emacs-ement"
   "emacs-elfeed"

   ;; mail
   "mu"
   "isync"
   "imapfilter"
   "mutt"
   "neomutt")))

 (services
  (append
   (list (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (directories (list "../../git"
                                      "../../emacs"
                                      "../../systemd"
                                      "../../sway"
                                      "../../tmux"
                                      "../../mail"
                                      "../../guile"
                                      "../../readline"
                                      "../../mutt"))))

         ;; XDG_CONFIG_DIR services
         (service home-xdg-configuration-files-service-type
                  ;;-> $XDG_CONFIG_DIR/foot/foot.init
                  `(("foot/foot.ini" ,(local-file "../../config/.config/foot/foot.ini"))))

         ;; Add additional channels, for work we need HPC, having nonguix is also handy
         (simple-service 'additional-packages-service
                         home-channels-service-type
                         (list
                          (channel
                           (name 'guix-hpc)
                           (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
                           (branch "master"))
                           (channel
                            (name 'nonguix)
                            (url "https://gitlab.com/nonguix/nonguix")
                            ;; Enable signature verification:
                            (introduction
                             (make-channel-introduction
                              "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                              (openpgp-fingerprint
                               "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
                           (channel
                            (name 'bin-guix)
                            (url "https://github.com/ieugen/bin-guix")
                            (branch "main"))))))))
