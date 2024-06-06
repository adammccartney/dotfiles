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

   ;; Emacs
   "emacs"
   "emacs-guix"
   "emacs-geiser"
   "emacs-geiser-guile"
   "emacs-yasnippet"
   "emacs-rg"
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

