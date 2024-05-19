;; Based on David Wilson's desktop service
;; https://raw.githubusercontent.com/daviwil/dotfiles/058ac0af09aa2577747b1bf99d6b1f0b018ef6ce/daviwil/home-services/desktop.scm
(define-module (admccartney home-services desktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages glib)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)

  #:export (ad/home-desktop-service-type))

(define ad/home-desktop-service-type
  (service xfce-desktop-service-type))
