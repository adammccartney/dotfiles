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

(home-environment
 (packages (specifications->packages (list "coreutils"
                                           "glibc-locales"
                                           "stow"

                                           ;; desktop
                                           "sway"

                                           ;; Compatibility for older Xorg applications
                                           "xorg-server-xwayland"

                                           ;; Flatpak and XDG utilities
                                           "flatpak"
                                           "xdg-desktop-portal"
                                           "xdg-desktop-portal-gtk"
                                           "xdg-desktop-portal-wlr"
                                           "xdg-utils" ;; For xdg-open, etc
                                           "xdg-dbus-proxy"
                                           "shared-mime-info"

                                           ;; Appearance
                                           "gnome-themes-extra"
                                           "adwaita-icon-theme"

                                           ;; Fonts
                                           "font-iosevka"
                                           "font-liberation"

                                           ;; Browsers
                                           "vimb"

                                           ;; Authentication
                                           "password-store"

                                           ;; PDF reader
                                           "zathura"
                                           "zathura-pdf-mupdf"

                                           ;; General utilities
                                           "curl"
                                           "wget"
                                           "openssh"
                                           "zip"
                                           "unzip"
                                           "trash-cli"
                                           
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
         `(,@ad/shell-service)
          (,@ad/home-desktop-service-type)))))









