;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

;;(list (channel
;;        (name 'guix)
;;        (url "https://git.savannah.gnu.org/git/guix.git")
;;        (branch "master")
;;        (commit
;;          "8d29f416a9378d30f63c2a95f1bd1a420d9ccab4")
;;        (introduction
;;          (make-channel-introduction
;;            "9edb3f66fd807b096b48283debdcddccfea34bad"
;;            (openpgp-fingerprint
;;              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(specifications->manifest
  (list "guile-ssh"
        "libtool"
        "libvterm"
        "mu"
        "guile-readline"
        "emacs-no-x-toolkit"
        "emacs-geiser-guile"
        "git"
        "neovim"
        "guile-git"
        "emacs-yasnippet"
        "emacs-geiser"
        "guile"
        "glibc-locales"))








