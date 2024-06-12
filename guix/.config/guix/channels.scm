(list
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (commit "143b597422bf01a8fa0e8b4e6e3ece3b3ebe9752")
  ;; Enable signature verification:
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'guix)
  (branch "master")
  (url "https://git.savannah.gnu.org/git/guix.git") 
  (commit "bb73faea028cc9a15af62cb8ade15d58da51bea")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"     ;2020-05-26
    (openpgp-fingerprint                           ;mbakke
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

