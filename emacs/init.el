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
   '(0blayout 0blayout 0blayout adaptive-wrap adaptive-wrap adaptive-wrap
              adaptive-wrap adaptive-wrap aidermacs aidermacs
              aidermacs apheleia apheleia apheleia apheleia apheleia
              auto-compile auto-compile auto-compile auto-compile
              auto-compile cargo cargo cargo cargo cargo cfrs cfrs
              cfrs cfrs claude-code claude-code claude-code
              claude-code claude-code claude-shell company dtrt-indent
              dtrt-indent dtrt-indent dtrt-indent dtrt-indent eat eat
              eat eat eat editorconfig editorconfig editorconfig
              editorconfig editorconfig eglot eglot eglot eglot eglot
              eldoc eldoc eldoc faceup faceup faceup faceup faceup
              flycheck flycheck flycheck flycheck flymake flymake
              flymake flymake ggtags ggtags ggtags ggtags
              gnu-elpa-keyring-update gnu-elpa-keyring-update
              gnu-elpa-keyring-update gnu-elpa-keyring-update gptel
              gptel groovy-mode groovy-mode groovy-mode groovy-mode
              hydra hydra hydra idlwave idlwave idlwave idlwave
              lsp-java lsp-java lsp-java lsp-java lsp-ui lsp-ui lsp-ui
              lsp-ui magit magit magit magit org org org org pfuture
              pfuture pfuture python python python python python-mode
              python-mode python-mode python-mode recompile-on-save
              recompile-on-save recompile-on-save recompile-on-save rg
              rg rg rg rustic rustic rustic rustic shell-maker
              track-changes track-changes track-changes track-changes
              tramp tramp tramp tramp transient transient transient
              transient tree-sitter-langs tree-sitter-langs
              tree-sitter-langs tree-sitter-langs treesit-auto
              treesit-auto treesit-auto treesit-auto verilog-mode
              verilog-mode verilog-mode verilog-mode which-key
              which-key which-key which-key whisper window-tool-bar
              window-tool-bar window-tool-bar window-tool-bar
              yaml-mode yaml-mode yaml-mode yaml-mode yasnippet
              yasnippet yasnippet yasnippet))
 '(package-vc-selected-packages
   '((whisper :url "https://github.com/natrys/whisper.el" :branch
              "master")
     (claude-code :url
                  "https://github.com/stevemolitor/claude-code.el")))
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
