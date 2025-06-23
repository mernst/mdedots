;;; .emacs file
;;; Michael D. Ernst

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; (package-initialize)

;; So that the main startup file can be byte-compiled, and so that all my
;; Emacs customizations are in a single directory.
;; (Not "~/emacs/dot-emacs" because that fails when run as another user.)
(load (substitute-in-file-name "$HOME/emacs/dot-emacs"))


;;; Everything below was automatically inserted (and should perhaps be
;;; merged into my Emacs initialization files).

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jdee-server-dir (expand-file-name "~/.emacs.d/jdee-server"))
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   '(adaptive-wrap apheleia cargo company "company" dap-mode dtrt-indent
                   flycheck flyspell ggtags gnu-elpa-keyring-update
                   groovy-mode helm helm-lsp lsp-java lsp-javacomp
                   lsp-ui "lsp-ui" magit mmm-mode projectile
                   python-mode rg rustic shfmt transient tree-sitter
                   tree-sitter-langs treesit treesit-auto which-key
                   yaml-mode yasnippet))
 '(safe-local-variable-values
   '((TeX-master . "../main") (TeX-master . "main") (auto-fill-mode)
     (require-final-newline . t) (mangle-whitespace . t)
     (TeX-command-default . "PDF") (TeX-master . t)
     (inleft-string . "% ") (major-mode . text-mode)
     (auto-fill-function)))
 '(tramp-password-prompt-regexp "^.*\\([pP]assword\\|passphrase\\|Response\\).*:\0? *")
 '(visual-line-fringe-indicators '(left-curly-arrow nil))
 '(warning-suppress-types '((comp) (undo discard-info))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
