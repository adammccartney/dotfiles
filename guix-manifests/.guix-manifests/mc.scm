;; this manifest is designed for running with guix on foreign distro
(specifications->manifest
 '( ;; basic system utils
   "coreutils"
   "curl"
   "git"
   "openssl"
   "stow"
   "tree"
   "glibc-locales"
   "fontconfig"

   ;; passwords
   "password-store"

   ;; cli apps
   "ripgrep"
   "git"
   "git-crypt"
   "fzf"
   "tmux"

   ;; graphics
   "gimp"
   
   ;; guile scheme
   "guile"
;;   "guile-next"
;;   "guile-ares-rs"
   "guile-chickadee"
   "guile-colorized"
   "guile-readline"
   "guile-sdl2@0.8.0"

   "nyacc"

   "sdl2"
   "sdl2-image"

   ;; go

   ;; python
   "python-lsp-server"

   ;; Emacs
   "emacs"
   "emacs-use-package"
   "emacs-no-littering"
   "emacs-geiser"
   "emacs-geiser-guile"
   "emacs-consult"
   "emacs-all-the-icons-dired"
   "emacs-yasnippet"
   "emacs-rg"
;;   "emacs-arei"

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
   "emacs-jsonrpc"
   "emacs-eglot"
   "emacs-magit"
   "emacs-yasnippet"
   "emacs-yasnippet-snippets"
   "emacs-rainbow-mode"
   "emacs-rainbow-delimiters"
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
   "emacs-pyvenv"
   "emacs-terraform-mode"
   "emacs-flycheck"

   "emacs-ement"
   "emacs-elfeed"
   
   "libvterm"
   ;;"automake"
   ;;"autoconf"
   ;;"libtool"
   ;;"gettext"

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
   "neomutt"))

