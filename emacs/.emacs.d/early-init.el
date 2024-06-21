;;Trying to stop eglot complaining about missing bindings in emacs 29.3
;;problem is with jsonrpc
;;see: https://issues.guix.gnu.org/70211
(setq load-no-native t)
