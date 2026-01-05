;;; -*- lexical-binding: t -*-

;;; autoloads-mde --- autoloaded packages

;;; Commentary:
;; (none)

;;; Code:

(require 'transient)

(eval-when-compile
  (use-package transient)
  (use-package magit
    :after transient))
(eval-when-compile
  (require 'edebug))
(eval-when-compile
  (require 'elide-head))
(eval-when-compile
  (require 'info))

;; To recompile all packages:
;; (byte-recompile-directory package-user-dir 0 'force)

(defun package-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))
;; Running this finally did the trick!
;; (package-reinstall-all-activated-packages)

;;;
;;; Coding/programming
;;;

(defalias 'copmile 'compile)
(setq-default compile-command "make -k -j ")	; default: "make -k "

;; Coding
                                        ; GNU boilerplate
(push '("is free software; you can redistribute it" . 
	        "Boston, MA 02111-1307, USA\\.\\|Cambridge, MA 02139, USA\\.")
      elide-head-headers-to-hide)


(autoload 'symbol-at-point "thingatpt" nil nil)
(autoload 'word-at-point "thingatpt" nil nil)

;; Python coding
;; python-mode is autoloaded from python.el

;; Scheme coding
(autoload 'scheme-inspector "scheme-inspector" "Scheme inspector" t)
(autoload 'si-jump "scheme-inspector" "Scheme inspector" t)
(autoload 'si-expression "scheme-inspector" "Scheme inspector" t)
(autoload 'run-big-scheme "scheme-mde" "Scheme with big heap" t)
(autoload 'chez-scheme-mode "chezscheme" "Chez Scheme mode" t)
(setq auto-mode-alist
      (cons '("\\.ss$" . scheme-mode)
	    auto-mode-alist))

(autoload 'cecil-mode "cecil-mode" "Cecil programming" t)
(setq auto-mode-alist
      (cons '("\\.cecil$" . cecil-mode)
	    auto-mode-alist))

;; Running Lisp
(autoload 'run-kcl "/u/mic/el/kcl-mode" "Run kcl (not loaded)" t)
                                        ;(autoload 'scheme "/a/kcl/goodies/kcl" "KCL, T, and R^3 Scheme" t)
                                        ;(autoload 'kcl "/a/kcl/goodies/kcl" "KCL, T, and R^3 Scheme" t)
;; (autoload 'run-ilisp "ilisp-mde" "Select a new inferior LISP." t)
;; (autoload 'kcl       "ilisp-mde" "Inferior Kyoto Common LISP." t)
;; (autoload 'akcl      "ilisp-mde" "Inferior Austin Kyoto Common LISP." t)
;; (autoload 'scheme    "ilisp-mde" "Inferior generic Scheme." t)
;; I can't get Lucid to run correctly; the default-directory is messed up.
                                        ; (setq lucid-program "lucid")
                                        ; (autoload 'lucid     "ilisp-mde" "Inferior Lucid Common LISP." t)

;; 6.846
(autoload 'run-tea "$PARARCHPATH/bin/t" "Run an inferior T process." t)
(autoload 'run-asim "$PARARCHPATH/bin/asim"
  "Run an inferior ASIM (experimental) process." t)
(setq auto-mode-alist
      (cons '("\\.t$" . scheme-mode)	; Scheme mode for T files.
	    auto-mode-alist))

(autoload 'javadoc-lookup "javadoc-lookup" "Look up Java entity." t)
(autoload 'jlookup "javadoc-lookup" "Look up Java entity." t)
(autoload 'java-insert-import "javadoc-lookup" "Insert Java import statement." t)
(autoload 'jimport "javadoc-lookup" "Insert Java import statement." t)
(autoload 'compilation-fix-java-imports "javadoc-lookup" "Insert Java import statements to fix compilation errors" t)

;;; Version control

(autoload 'magit-status "magit")
(global-set-key "\C-cg" 'magit-status)
;; disable Magit warning messages
(setq magit-last-seen-setup-instructions "1.4.0"
      magit-push-always-verify nil)
;; Don't show recently-pushed commits.  In other words, if the git status
;; is clean, the magit status should be clean too.
(with-eval-after-load "magit"
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-unpushed-to-upstream
			  'magit-insert-unpushed-to-upstream-or-recent
			  'replace))
;; with-eval-after-load would be better, but was only introduced in Emacs 24.4.
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       "~/emacs/magit/Documentation/"))
;; Work around: Key sequence C-x M-g starts with non-prefix key C-x ESC
;; https://gitter.im/magit/magit?at=601c19379fa6765ef8f9eb8d
(setq magit-define-global-key-bindings nil)


;;;
;;; Emacs Lisp Coding
;;;

(autoload 'headers "headers" "Definition headers." t)
;; (autoload 'docstring-substitute "docstring"
;; 	  "Substitute variable or function documentation into a buffer, to keep manuals consistent with code." t)

;; util-mde
(autoload 'in-window "util-mde")
(autoload 'bury-or-raise-buffer "util-mde")

(defun mde-edebug-setup-hook ()
  "Mike's edebug setup code."
  ;; Initial-mode = step is OK if we only rarely edebug our functions.
  ;; (setq edebug-initial-mode 'go)
  (defun edebug-go-initial-mode ()
    (interactive)
    (setq edebug-initial-mode 'go))
  (defun edebug-step-initial-mode ()
    (interactive)
    (setq edebug-initial-mode 'step))
  (defun edebug-current-buffer ()
    "Enable edebug for all definitions in the current buffer."
    (interactive)
    (require 'edebug)
    (let ((edebug-all-defs t))
      (eval-buffer)))
  (defalias 'edebug-buffer 'edebug-current-buffer)
  (defun edebug-region (beg end)
    "Enable edebug for all definitions in the region."
    (interactive "r")
    (require 'edebug)
    (let ((edebug-all-defs t))
      (eval-region beg end)))
  )
(add-hook 'edebug-setup-hook 'mde-edebug-setup-hook)

(autoload 'faq-insert-matching-questions "emacs-faq" "Answer Emacs FAQ" t)



;; This gets overridden by mde-mail-mode-hook.  Sigh.
(with-eval-after-load "sendmail"
  (define-key mail-mode-map "\C-c\C-a" 'mde-mail-interactive-insert-alias))


;;;
;;; Editing text
;;;

(autoload 'conference-wc "conf-wc" nil t)

;; Use M-x hscroll-mode instead of M-x truncate-lines
(autoload 'turn-on-hscroll "hscroll" nil t)
(autoload 'hscroll-mode "hscroll" nil t)
;;(autoload 'hscroll-global-mode "hscroll" nil t)	; don't use this.

;; Spelling
(defalias 'ispell 'ispell-buffer)  ; in ispell.el, but want short version early
(defalias 'spell-buffer 'ispell-buffer)
(defalias 'spell-word 'ispell-word)
(defalias 'spell-region 'ispell-region)

;; No replacement for spell-string.
;; Taken from webster.el
(defun webster-prompt (prompt)
  (let ((default-word (current-word))
        word)
    (cond
     ((string= default-word "")
      (setq prompt (concat prompt ": ")))
     (t
      (setq prompt (format "%s (%s): " prompt default-word))))
    (setq word (read-string prompt))
    (if (string= word "")
        (list default-word)
      (list word))))

(defun dictionary-www (word)
  "Look up definition of WORD in a browser."
  (interactive (webster-prompt "Define word"))
  (funcall browse-url-browser-function
	   ;; (concat "http://www.google.com/dictionary?aq=f&langpair=en|en&hl=en&q=" word)
	   ;; (concat "http://work.ucsd.edu:5141/cgi-bin/http_webster?" word)
	   ;; (concat "http://dictionary.com/browse/" word)
	   (concat "https://www.merriam-webster.com/dictionary/" word)
	   ))

(defun thesaurus-www (word)
  "Look up WORD in Merriam-Webster's thesaurus on the WWW."
  (interactive (webster-prompt "Thesaurus word"))
  (setq word (replace-regexp-in-string " " "%20" word))
  (funcall browse-url-browser-function
	   ;; (concat "http://work.ucsd.edu:5141/cgi-bin/http_webster?" word)
	   ;; (concat "http://www.m-w.com/cgi-bin/thesaurus?book=Thesaurus&va=" word)
	   ;; (concat "http://thesaurus.reference.com/search?q=" word)
	   ;; (concat "http://thesaurus.com/browse/" word)
	   (concat "https://www.merriam-webster.com/thesaurus/" word)
	   ))

;; Love22
(autoload 'show-abc-chart "love22"
  "Show the ABC chart in the *Help* window." t)
(autoload 'abc-chart-word "love22"
  "Compute and display the ABC chart value of the word at point." t)
(autoload 'abc-chart-region "love22"
  "Compute and display the ABC chart value of the current region." t)
(autoload 'love22-buffer "love22"
  "Process a buffer to look like Love22 wrote it." t)
(autoload 'love22-region "love22"
  "Process a region to look like Love22 wrote it." t)
(autoload 'love22-mode "love22"
  "Minor mode for writing Love22 text." t)

;; LaTeXinfo
(autoload 'get-latexinfo-node "get-node" "Get help on a LaTeXinfo topic" t)
;; (define-key help-map "g" 'get-latexinfo-node)
(autoload 'latexinfo-format-buffer "latexinfo" "Format a buffer for LaTeXinfo" t)
(autoload 'latexinfo-mode "latexinfomd" "An editing for LaTeXinfo files" t)
(autoload 'nodify-file "nodify" "Create nodes structure for a LaTeXinfo file." t)
(autoload 'tex-to-latexinfo "tolatexinfo" "Convert a buffer from TeXinfo to LaTeXinfo" t)
;; This is like nodify-file, built into LaTeXinfo (but that fails)
(autoload 'latexinfo-structure "latexinfo-structure" "Add node and menu commands to LaTeXinfo commands" t)


;; (require 'vm-autoload)

(autoload 'vm "vm" "Start VM on your primary inbox." t)
(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
(autoload 'vm-mail "vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)
(autoload 'vm-visit-imap-folder "vm" "Visit a IMAP mailbox." t)


;; Mailcrypt
;; (autoload 'mc-install-write-mode "mailcrypt" nil t)
;; (autoload 'mc-install-read-mode "mailcrypt" nil t)
;; (autoload 'mc-setversion "mc-setversion" nil t)
;; ; writing
;; (add-hook 'mail-mode-hook 'mc-install-write-mode)
;; (setq mc-pgp-comment nil)		; eliminate gratuituous advertising
;; (setq mc-pgp50-comment nil)		; eliminate gratuituous advertising
;; ; reading
;; (add-hook 'rmail-mode-hook 'mc-install-read-mode)
;; (add-hook 'rmail-summary-mode-hook 'mc-install-read-mode)
;; (add-hook 'vm-mode-hook 'mc-install-read-mode)
;; (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
;; (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
;; (add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
;; (add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
;; (add-hook 'news-reply-mode-hook 'mc-install-write-mode)


(autoload 'clean-mail-buffer "mail-simplify" nil t)

(autoload 'compose-mail-address-buffer "bulk-mail" nil t)
(autoload 'compose-mail-address-buffer-and-bury "bulk-mail" nil t)

;; I don't like adoc-mode because it changes font sizes too much -- some
;; text becoomes ridiculously large and other text becomes unreadably small.
;; (autoload 'adoc-mode "adoc-mode" nil t)
;; (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))


;;;
;;; Information
;;;

;; I mirror this locally every week.  David Jones no longer does at aviary.
(setq lisp-code-directory "~/emacs/LCD-datafile.gz")
;; ;; Add slash after colon
;; (setq lisp-code-directory
;;       "/anonymous@archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/LCD-datafile.gz")
(autoload 'lisp-dir-apropos "lispdir" "Search a database of elisp code" t)
(autoload 'lisp-dir-retrieve "lispdir" "Retrieve elisp code" t)
(autoload 'submit-lcd-entry "lispdir" "Submit code for the LCD" t)
(autoload 'tex-macro-catalogue "tex-macro-catalogue"
  "TeX macro catalogue browser" t)
(autoload (function find-faq) "faq"
  "*Find the archived Usenet NEWSGROUP FAQ file..." t)

;;;
;;; Differences
;;;

;; Bdiff
(define-key Buffer-menu-mode-map "c" 'Buffer-menu-bdiff)
(define-key Buffer-menu-mode-map "=" 'Buffer-menu-bdiff)
(define-key Buffer-menu-mode-map "R" 'Buffer-menu-revert)
(define-key Buffer-menu-mode-map "r" 'Buffer-menu-revert-select)
(setq bdiff-context-lines 'unidiff)	; default 1; 2 is better; 'unidiff best
(setq bdiff-ignore-whitespace 'compare-whitespace-equal)
;; (setq bdiff-extra-flags "u")		; default "a"
(autoload 'bdiff "bdiff" "Buffer differences from disk." t)
(autoload 'list-munged-buffers "bdiff" "List buffers with modified disk files." t)
(autoload 'list-unsaved-buffers "bdiff" "List modified buffers." t)
(autoload 'Buffer-menu-bdiff "bdiff" "Diff buffer under point with file contents." t)
(autoload 'Buffer-menu-revert "bdiff" "Revert buffer under point from file, then select it." t)
(autoload 'Buffer-menu-revert-select "bdiff" "Doc" t)

;;;
;;; World Wide Web
;;;

(autoload 'browse-url-netscape "browse-url" "Invoke Netscape on URL" t)
(defalias 'netscape-browse-url 'browse-url-netscape)
(defalias 'mozilla-browse-url 'browse-url-netscape)

;; (defun browse-url--abort-if-empty-string (s)
;;   "Abort if the given string is empty."
;;   (if (equal s "")
;;       (error "Empty URL")))
;; (advice-add 'browse-url :before #'browse-url--abort-if-empty-string)


(autoload 'update-menu-bars "www-mde" nil t)
(autoload 'extra-html "www-mde" nil t)
(autoload 'fair-html "www-mde" nil t)
(autoload 'process-indexees-for-file "www-index" nil t)
(autoload 'process-indexees-for-directory "www-index" nil t)
;; (setq top-directory-prefix "/fair@wideopen.igc.apc.org:/c/home2/fair/WWW/")
(setq top-directory-prefix "/fair@wideopen.igc.apc.org:/d/root.home/fair/WWW/")
(setq menu-bar-file "/fair@wideopen.igc.apc.org:/d/root.home/fair/WWW/menu-bar-structure")
(setq index-obarray-file (concat top-directory-prefix "index/raw-index.el"))



;;;
;;; Miscellaneous
;;;

;; buffer-menu
(autoload 'do-buffer-menu-replacements "buffer-menu-mde" "Abbreviate filenames" t)
(define-key ctl-x-map "\C-b" 'buffer-menu)	; was list-buffers
(with-eval-after-load "buff-menu" (require 'buffer-menu-mde))
;; (setq Buffer-menu-use-header-line t)	; default t

;; Dired
(autoload 'dired-jump "dired-x" "Tree dired" t)


;; Games, silly and serious.
(autoload 'games	"games" "Games comint mode." t)
(autoload 'games-ask	"games" "Games comint mode." t)
(autoload 'konane-substitute "games-k-exp"
  "Substitute Konane games into file." t)
(autoload 'games-defexpectation "games" "Define a new games expectation form.")

(autoload 'madlib "madlib" "Mad Lib game" t)
(setq madlib-directories "~/random/madlib/")

;; Wasting time
;; ; gnus customizations in gnus-mike-batch.el or gnus-mike.el
(setq gnus-init-file (expand-file-name "~/emacs/gnus-mike"))
(autoload 'caesar-region "rnews" "Rot-13 the region." t)
                                        ;(autoload 'irc "~/emacs/irc-2.0" "Internet Relay Chat" t)

;; Inleft; Emacs-19 comment-region isn't as versatile.
;; This seems to be necessary to get correct behavior from, eg, cecil-mode
(make-variable-buffer-local 'inleft-string)
(autoload 'inleft "~/emacs/inleft" "Comment-out-like utility." t)
(autoload 'inleft-internal "~/emacs/inleft" "Comment-out-like utility." t)
(autoload 'uncomment-region "~/emacs/inleft" "Uncomment-out-like utility." t)

(autoload 'pages-directory "page-ext" "Page handling extensions" t)

(setq message-buffer-at-end-p t
      message-buffer-timestamp-p nil)

(autoload 'background "background" nil t)

(autoload 'global-replace-lines "globrep" nil t)

;;; speedbar
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(define-key-after (lookup-key global-map [menu-bar tools])
  [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])
;; Texinfo fancy chapter tags (for speedbar)
;; (add-hook 'texinfo-mode-hook (lambda () (require 'sb-texinfo)))
;; for Linux systems
(autoload 'rpm "sb-rpm" "Rpm package listing in speedbar.")

;;; Common Lisp
;; Didn't I have another way of doing this before?

(autoload 'travis-cleanup-output "one-off" "Remove extraneous characters from a Travis CI log." t)
(autoload 'azure-cleanup-output "one-off" "Remove extraneous characters from an Azure Pipelines CI log." t)
(autoload 'kindle-prepare-for-transfer "one-off" "Prepare to transfer Kindle books" t)

;; Searching.
(if (not (fboundp 'deadgrep))
    (autoload 'deadgrep "deadgrep" "Start a ripgrep search for SEARCH-TERM."))

(autoload 'ical-available (substitute-in-file-name "$HOME/java/plume-lib/icalavailable/src/main/elisp/ical-available.el") "Insert a summary of my available times." t)

;; Testing only:
(autoload 'mw-test-setup "miniwindow" nil t)
(autoload 'mw-mde "miniwindow" nil t)
(autoload 'test-region-edit "regionedit" nil t)

;; Also don't forget these libraries:
                                        ; rmailsort

;;; autoloads-mde.el ends here
