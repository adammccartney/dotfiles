;; This was generated by the graphical installer the first time we configured
;; the system via graphical install using qemu

(use-modules (gnu) (gnu system nss) (guix utils))
(use-service-modules desktop sddm xorg)
(use-package-modules gnome)

(operating-system
 (locale "en_GB.utf8")
 (timezone "Europe/Vienna")
 (keyboard-layout (keyboard-layout "de" "nodeadkeys"))
 (host-name "watt")

 ;; List of user accounts
 (users (cons* (user-account
                (name "admccartney")
                (comment "Adam McCartney")
                (group "users")
                (home-directory "/home/admccartney")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))


 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets `("/boot/efi"))
              (keyboard-layout keyboard-layout)))
 

 ;; list of filesystems that get "mounted", their ("UUIDs") can be obtained by running 'bklid' in a terminal.
 (file-systems (append
                (list (file-system
                       (device (file-system-label "ad-root"))
                       (mount-point "/")
                       (type "ext4"))
                      (file-system
                       (device (uuid "165D-E706" `fat))
                       (mount-point "/boot/efi")
                       (type "vfat"))
                %base-file-systems))

 ;; Specify a swap parition (note that at swap file might be handier)
 (swap-devices (list (swap-space
                      (target "/swapfile"))))

 
 ;; Add GNOME and Xfce---we can choose at the log-in screen by clicking the gear.
 ;; Use the "desktop" services, which include the X11 log-in service, networking with
 ;; NetworkManager, and more
 (services (if (target-x86-64?)
               (append (list (service gnome-desktop-service-type)
                             (service xfce-desktop-service-type)
                             (set-xorg-configuration
                              (xorg-configuration
                               (keyboard-layout keyboard-layout))))
                       %desktop-services)

               ;; FIXME: Since GDM depends on Rust (gdm -> gnome-shell -> gjs -> mozjs -> rust) and Rust
               ;; is currently unavailable on non-x86_64 platforms, we use SDDM and Mate here instead of
               ;; GNOME and GDM
               (append (list (service mate-desktop-service-type)
                             (service xfce-desktop-service-type)
                             (set-xorg-configuration
                              (xorg-configuration
                               (keyboard-layout keyboard-layout))
                              sddm-service-type))
                       %desktop-services)))

 ;; Allow resolution of '.local' host names with mDNS
 (name-service-switch %mdns-host-lookup-nss))

