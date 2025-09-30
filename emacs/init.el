;;; -*- lexical-binding: t -*-

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
 '(ignored-local-variable-values '((TeX-master . "dataflow")))
 '(jdee-server-dir (expand-file-name "~/.emacs.d/jdee-server"))
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   '(0blayout adaptive-wrap apheleia auto-compile cargo claude-code
              claude-shell dtrt-indent eat editorconfig eglot faceup
              flycheck flymake ggtags gnu-elpa-keyring-update
              groovy-mode idlwave lsp-java lsp-ui magit org python
              python-mode recompile-on-save rg rustic shell-maker
              track-changes tramp transient tree-sitter-langs
              treesit-auto verilog-mode which-key window-tool-bar
              yaml-mode yasnippet
              0blayout adaptive-wrap aidermacs apheleia auto-compile cargo cfrs
              claude-code dtrt-indent eat editorconfig eglot eldoc
              faceup flycheck flymake ggtags gnu-elpa-keyring-update
              gptel groovy-mode hydra idlwave lsp-java lsp-ui magit org
              pfuture python python-mode recompile-on-save rg rustic
              string-inflection track-changes tramp transient
              tree-sitter-langs treesit-auto verilog-mode which-key
              window-tool-bar yaml-mode yasnippet
              adaptive-wrap apheleia auto-compile cargo claude-code cfrs
              dtrt-indent eat editorconfig eglot eldoc faceup flycheck
              flymake ggtags gnu-elpa-keyring-update groovy-mode hydra
              idlwave lsp-java lsp-ui magit org pfuture python python-mode
              recompile-on-save rg rustic track-changes tramp
              transient tree-sitter-langs treesit-auto verilog-mode
              which-key window-tool-bar yaml-mode yasnippet
              adaptive-wrap aidermacs apheleia auto-compile cargo cfrs
              claude-code dtrt-indent eat editorconfig eglot faceup eldoc
              flycheck flymake ggtags gnu-elpa-keyring-update gptel
              groovy-mode hydra idlwave lsp-java lsp-ui magit org
              pfuture python python-mode recompile-on-save rg rustic
              string-inflection track-changes tramp transient
              tree-sitter-langs treesit-auto verilog-mode which-key
              window-tool-bar yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(safe-local-variable-values
   '((checkdoc-allow-quoting-nil-and-t . t) (TeX-master . "../main")
     (TeX-master . "main") (auto-fill-mode)
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
 '(claude-code-repl-face ((t (:family "JuliaMono"))) t))
