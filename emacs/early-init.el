;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
(setq load-prefer-newer t)
(add-to-list 'load-path "~/.emacs.d/elpa/auto-compile-20250531.2214/")
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
;;; early-init.el ends here
