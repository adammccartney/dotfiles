;; -*- mode: scheme; -*-
;; palais-de-mari.scm - defines a minimal desktop system for guix

;; -*- mode: scheme; -*-
;; This is an operating system configuration template
;; for a "desktop" setup without full-blown desktop
;; environments.

(use-modules (gnu) (guix) (srfi srfi-1))
(use-service-modules desktop mcron networking spice ssh xorg sddm)
(use-package-modules bootloaders fonts
                     package-management xdisorg xorg)

(define vm-image-motd (plain-file "motd" "
\x1b[1;37mThis is the GNU system.  Welcome!\x1b[0m

This instance of Guix is a template for virtualized environments.
You can reconfigure the whole system by adjusting /etc/config.scm
and running:

  guix system reconfigure /etc/config.scm

Run '\x1b[1;37minfo guix\x1b[0m' to browse documentation.

\x1b[1;33mConsider setting a password for the 'root' and 'guest' \
accounts.\x1b[0m
"))

(operating-system
  (host-name "malone")
  (timezone "Europe/Vienna")
  (locale "en_IE.utf8")
  (keyboard-layout (keyboard-layout "de" "nodeadkeys"))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))))

  ;; Assume the target root file system is labelled "my-root",
  ;; and the EFI System Partition has UUID 1234-ABCD.
  (file-systems (append
                 (list (file-system
                         (device (file-system-label "ad-root"))
                         (mount-point "/")
                         (type "ext4"))
                       (file-system
                         (device (uuid "1234-ABCD" 'fat))
                         (mount-point "/boot/efi")
                         (type "vfat")))
                 %base-file-systems))

  (users (cons (user-account
                (name "adam")
                (comment "none")
                (group "users")
                (home-directory "/home/adam")
                (supplementary-groups '("wheel" "netdev"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Add a bunch of window managers; we can choose one at
  ;; the log-in screen with F1.
  (packages (append (map specification->package
                         ;; window managers
                         '("emacs-exwm"
                           "sway"
                           ;; terminal emulator
                           "gnome-shell"
                           "foot"
                           ;; editors
                           "emacs-no-x-toolkit"
                           "vim"
                           ;; cli apps
                           "tmux"
                           "x-resize"
                           "gvfs")) ;; for use mounts
                    %base-packages))

 ;; Tailor a set of services that work well in a vm  
 (services
   (append (list (service xfce-desktop-service-type)
       
                 ;; Uncomment the line below to add an SSH server.
                 ;;(service openssh-service-type)

                 ;; Add support for the SPICE protocol, which enables dynamic
                 ;; resizing of the guest screen resolution, clipboard
                 ;; integration with the host, etc.
                 (service spice-vdagent-service-type)

                 ;; Use the DHCP client service rather than NetworkManager.
                 (service dhcp-client-service-type))

           ;; Remove some services that don't make sense in a VM.
           (remove (lambda (service)
                     (let ((type (service-kind service)))
                       (or (memq type
                                 (list gdm-service-type
                                       sddm-service-type
                                       wpa-supplicant-service-type
                                       cups-pk-helper-service-type
                                       network-manager-service-type
                                       modem-manager-service-type))
                           (eq? 'network-manager-applet
                                (service-type-name type)))))
                   (modify-services %desktop-services
                     (login-service-type config =>
                                         (login-configuration
                                          (inherit config)
                                          (motd vm-image-motd)))
                     ;; Install and run the current Guix rather than an older
                     ;; snapshot.
                     (guix-service-type config =>
                                        (guix-configuration
                                         (inherit config)
                                         (guix (current-guix))))))))

  ;; Allow resolution of '.local' host names with mDNS.
  (name-service-switch %mdns-host-lookup-nss))






