;;; early-init.el  -*-  lexical-binding: t; no-byte-compile: t -*-

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html says:
;; This file is loaded before the package system and GUI is initialized, so in
;; it you can customize variables that affect the package initialization
;; process, such as package-enable-at-startup, package-load-list, and
;; package-user-dir. Note that variables like package-archives which only affect
;; the installation of new packages, and not the process of making
;; already-installed packages available, may be customized in the regular init
;; file.

(setq load-prefer-newer t)

(if (equal "sl7" (system-name))
    (progn
      (setq no-native-compile t)
      (setq tramp-gvfs-enabled nil)))


;;; early-init.el ends here
