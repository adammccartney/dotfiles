(use-modules (gnu)
             (guix modules))
(use-service-modules networking
                     ssh)
(use-package-modules admin
                     package-management
                     ssh
                     tls)

(operating-system
  (host-name "guix-srv1")
  (timezone "Europe/Vienna")
  (locale "en_IE.UTF-8")
  (keyboard-layout (keyboard-layout "de" "nodeadkeys"))

  ;; copy over grub.cfg
  (bootloader (bootloader-configuration
               (bootloader
                (bootloader
                 (inherit grub-bootloader)
                 (installer #~(const #true))))))
  (file-systems (cons (file-system
                        (device "/dev/vda")
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))

  (initrd-modules (cons "virtio_scsi" ;; Needed to find the disk
                        %base-initrd-modules))

  ;; User accounts 'root' is implicit
  (users (cons* (user-account
                 (name "adam")
                 (comment "Adam McCartney")
                 (group "users")
                 (home-directory "/home/adam")
                 (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

   (packages (cons* openssh-sans-x
                    %base-packages))

   (services (cons*
              (service dhcp-client-service-type)
              (service openssh-service-type
                       (openssh-configuration
                        (openssh openssh-sans-x)
                        (password-authentication? #false)
                        (authorized-keys
                         `(("adam" ,(local-file "/home/adam/.ssh/adam.pubkey"))))))
              %base-services)))
