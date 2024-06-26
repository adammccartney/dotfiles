;; this manifest is designed for running with guix on foreign distro
(specifications->manifest
 '( ;; basic system utils
   "coreutils"
   "glibc-locales"
   "git"
   "nss-certs"
   "mcron"
   "openssl"
   "stow"
   "tree"

   ;; passwords
   "password-store"

   ;; build tools
   "cmake"
   
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

   ;; go
   "go"

   ;; python
   "python-pytest"
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
   
   "libvterm"
   "libtool"

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






