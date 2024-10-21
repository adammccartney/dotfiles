(use-modules (gnu))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
 (locale "en_IE.utf8")
 (timezone "Europe/Vienna")
 (keyboard-layout (keyboard-layout "de" "nodeadkeys"))

 ;; User accounts 'root' is implicit
 (users (cons* (user-account
                (name "adam")
                (comment "Adam McCartney")
                (group "users")
                (home-directory "/home/adam")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; System-wide packages
 (packages (append (list (specification->package "i3-wm")
                         (specification->package "i3status")
                         (specification->package "dmenu")
                         (specification->package "st")
                         (specification->package "nss-certs")
                         (specification->package "python-cloud-init")
                         (specification->package "git")
                         (specification->package "tmux")
                         (specification->package "vim-full")
                         (specification->package "emacs-no-x-toolkit")
                         (specification->package "coreutils"))
                   %base-packages))

 ;; System services
 ;; Search available: `guix system search KEYWORD`
 (services
  (append (list

           ;; OpenSSH can be configured by passing 'openssh-configuration'
           ;; record as a second argument to 'service' below
           (service openssh-service-type
                    (openssh-configuration
                     (permit-root-login 'prohibit-password)
                     (authorized-keys
                      `(("adam" ,(local-file "/home/adam/.ssh/id_ed25519_mc.it.tuwien.ac.at.pub"))))))
           
           (set-xorg-configuration
            (xorg-configuration
             (xorg-configuration (keyboard-layout keyboard-layout))))

           ;; Default list of services we are appending to
           %desktop-services))
  (bootloader (bootloader-configuration
               (bootloader grub-efi-bootloader)
               (targets (list "/boot/efi"))
               (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                       (target (uuid
                                "21f5fc8b-e3f2-46b7-af58-f880cb9d8711")))))

  ;; List of file systems that get "mounted".
  (file-systems (cons* (file-system
                        (mount-point "/boot/efi")
                        (device (uuid "5A86-3572"
                                      'fat32))
                        (type "vfat"))
                       (file-system
                        (mount-point "/")
                        (device (uuid
                                 "bc92c8a6-500a-44ce-ac50-5274066acecc"
                                 'ext4))
                        (type "ext4")) %base-file-systems)))
                         
