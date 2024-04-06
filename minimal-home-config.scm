(use-modules (gnu home)
             (gnu home services)
             (gnu home services dotfiles)
             (gnu home services shells)
             (gnu services)
             (gnu packages admin)
             (guix gexp))


(home-environment
 (packages (list htop))
 (services
  (list
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (directories (list "git"
                                "emacs"
;;                                "bash"
                                "vim"
                                "nvim"
                                "tmux"
                                "mail"
                                "guile"
                                "mutt"
                                "sway"))
             (excluded '(".*~" ".*\\.swp" "\\.git" "\\.gitignore"
                         "plugged/*"))))
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\
export HISTFILE=$XDG_CACHE_HOME/.bash_history")))))
;;             (bashrc (list `(".bash_prompt"
;;                            ".bash_aliases"
;;                            ".bash_functions"
;;                            ".bash_env")))))

   (simple-service 'test-config
                   home-xdg-configuration-files-service-type
                   (list `("test.conf"
                           ,(plain-file "tmp-file.txt"
                                        "the content of
                                          ~/.config/test.conf")))))))
