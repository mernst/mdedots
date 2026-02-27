;;; -*- lexical-binding: t -*-

;;; mode-hooks-mde.el --- Mike's Emacs mode hooks and associated functions

;;; Commentary:

;; See prog-modes-mde.el for programming language modes.

;; Much of the hook stuff should use add-hook instead of just setq.


;;; Code:

(eval-when-compile
  (require 'inleft)
  (require 'compile)
  (require 'util-mde)
  (require 'cl-lib)				; for second
  (require 'replace-paragraphs)
  (require 'startup-functions-mde)
  (require 'browse-url-once)
  )

(require 'compile)

(autoload 'replace-regexp-noninteractive "util-mde")
(autoload 'replace-string-noninteractive "util-mde")
(autoload 'browse-url-once "browse-url-once")
(autoload 'browse-url-once "browse-url-once")
(autoload 'browse-url-if-matched "browse-url-once")
(autoload 'browse-url-once-if-matched "browse-url-once")
(autoload 'browse-url-once-via-text-properties "browse-url-once")



(make-variable-buffer-local 'before-save-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

;; A minor mode to visualize TAB, (HARD) SPACE, NEWLINE
;; It can optionally do cleanup as well.  I don't seem to be using it now, though,
;; and other packages can do cleanup.
;; (require 'whitespace)
;; Possible customization
;; (setq whitespace-action '(cleanup auto-cleanup))
;; (setq whitespace-style '(empty space-before-tab trailing space-after-tab))
;;; The below disables whitespace mode in certain buffers.
;;; I should implement this more generally for all minor modes, maybe.
;; (defvar whitespace-ignore-filename-regexps
;;   nil
;;   "List of regular expressions. If any of them match a file name, then
;; trailing whitespace is not removed from the file.
;; These are typically source files whose style I shouldn't modify, because
;; they are maintained by someone else, and I wish to minimize differences/patches."
;; )
;; (setq whitespace-ignore-filename-regexps
;;   (list
;;    ;; Emacs source files
;;    "mernst/emacs/x?lisp/"
;;    "emacs[-/][0-9]+\.[0-9]+\\(\.[0-9]+\\)?/\\(lisp\\|src\\)/"
;;    "viewmail/lisp/"
;;    ;; Javac compiler
;;    "annotations/\\(vendor-\\)?compiler/"
;;    ;; ASM bytecode manipulation library
;;    "annotations/asmx?/"
;;    "org/objectweb/asm/"
;;    ;; FreePastry
;;    "pastry/src/"
;;    ;; Valgrind (part of Kvasir)
;;    "valgrind-3/"
;;    ))
;; (defun whitespace-ignored-filename (filename)
;;   "Return t if FILENAME should not have trailing whitespace removed."
;;   (let ((match nil)
;; 	(regexps whitespace-ignore-filename-regexps))
;;     (while regexps
;;       (let ((regexp (car regexps)))
;; 	(setq regexps (cdr regexps))
;; 	(if (string-match regexp filename)
;; 	    (setq match t
;; 		  regexps nil))))
;;     match))
;; (defun whitespace-disable-maybe ()
;;   (if (whitespace-ignored-filename (buffer-file-name))
;;       (whitespace-mode nil)))


;; todo: The delsp issue only requires changing a few characters, not all. Create a special routine to do only those changes. It includes this:
;;   change " " to " "
;; and also probably the single forward quote (which seems like a bug, actually).

(defun uneducate-quotes-iso-2022-jp ()
  "Change smart quotes to dumb (ASCII) quotes.
This prevents assumption of iso-2022-jp charset."
  (interactive)
  (save-excursion
    (replace-string-noninteractive "“" "\""))
  (save-excursion
    (replace-string-noninteractive "”" "\""))
  (save-excursion
    (replace-string-noninteractive "’" "'"))
  (save-excursion
    (replace-string-noninteractive "‘" "`"))
  )



(defun uneducate-quotes ()
  "Change smart quotes to dumb (ASCII) quotes."
  (interactive)
  (save-excursion
    (replace-string-noninteractive "“" "\""))
  (save-excursion
    (replace-string-noninteractive "”" "\""))
  (save-excursion
    (replace-string-noninteractive "’" "'"))
  (save-excursion
    (replace-string-noninteractive "‘" "`"))
  ;; Also do hyphens
  (save-excursion
    (replace-string-noninteractive "–" "-"))
  (save-excursion
    (replace-regexp-noninteractive " ?— ?" " -- "))
  (save-excursion
    (replace-string-noninteractive "…" "..."))
  )

(defun uneducate-quotes-for-latex ()
  "Change smart quotes to LaTeX quotes, from point forward in the buffer."
  (interactive)
  (save-excursion
    (replace-string-noninteractive "“" "``"))
  (save-excursion
    (replace-string-noninteractive "”" "''"))
  (save-excursion
    (replace-string-noninteractive "’" "'"))
  (save-excursion
    (replace-string-noninteractive "‘" "`"))
  (save-excursion
    (replace-string-noninteractive "—" "---"))
  (save-excursion
    (replace-string-noninteractive "…" "\\ldots"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text
;;;

(setq-default major-mode 'text-mode)
(defun mde-text-mode-hook ()
  "Mike's text mode hook."

  ;; Modes
  (abbrev-mode 1)
  (visual-line-mode 1)
  (setq fill-column 99999)

  ;; Set fill column and auto-fill-mode automatically
  (require 'util-mde)
  (let* ((len-table (line-lengths-histogram (point-min) (point-max)))
	 (filled-lines 0)
	 (non-filled-lines 0)
	 (process-table-entry #'(lambda (key value)
				  (if (and (>= key 65) (<= key 80))
				      (setq filled-lines (+ filled-lines value))
				    (setq non-filled-lines (+ non-filled-lines value))))))
    (maphash process-table-entry len-table)
    ;; for debugging:
    ;; (message "filled-lines %d; non-filled-lines %d; ratio %s" filled-lines non-filled-lines (/ filled-lines (+ filled-lines non-filled-lines) 1.0))
    (if (> (/ (* 1.0 filled-lines) (+ filled-lines non-filled-lines)) .25)
	(enable-auto-fill)))
  (cond ((or (equal "checklink-args.txt" (buffer-name))
	     (string-match "\.csv$" (buffer-name)))
	 (visual-line-mode 0)
	 (auto-fill-mode 0)))

  ;; Key bindings
  ;; 177 octal is 127 decimal = DEL
  (define-key text-mode-map "\177" 'delete-backward-char-untabify-maybe)

  ;; Variable settings
  ;; Avoid errors when editing RMAIL files.
  (make-local-variable 'require-final-newline)
  (if (string-match "^RMAIL$\\|\\.e?mail$" (buffer-name))
      (setq require-final-newline nil))

  (if (and (buffer-file-name)
	   (string-match "\\.adoc$" (buffer-file-name)))
      (progn
	(make-local-variable 'page-delimiter)
	(setq page-delimiter "\n=")))

  (if (paragraph-buffer-p)
      (setq paragraph-start "\f\\|$"
	    paragraph-separate paragraph-start))
  )
(add-hook 'text-mode-hook 'mde-text-mode-hook)


;;; Experiment with not using this.
;; (with-eval-after-load "text-mode"
;;      (define-key text-mode-map "\t" 'tab-to-tab-stop) ; was 'indent-relative
;;      )
;; ;; Maybe text-mode is preloaded, so do this to be safe
;; (if (boundp 'text-mode-map)
;;     (progn
;;       (define-key text-mode-map "\t" 'tab-to-tab-stop) ; was 'indent-relative
;;       ))

(defun delete-backward-char-untabify-maybe (arg &optional killp)
  "Delete characters backward.
Use `backward-delete-char-untabify' if prefix ARG was specified and preceding
char is a tab; otherwise use `delete-backward-char'.
Interactively, ARG is the prefix arg (default 1)
and KILLP is non-nil if prefix arg was specified."
  (interactive "*p\nP")
  (if (and (equal '(4) killp)
	   (char-equal (preceding-char) ?\t))
      (backward-delete-char-untabify 1)
    (delete-char (- arg) killp)))



(with-eval-after-load "help-mode"
  ;; also bound to "f" in this keymap
  (define-key help-mode-map "\C-cf" 'find-function)	; like M-. (find-tag)
  )


;; (defun isearch-abort-fsf-style ()
;;   "Quit incremental search mode, signaling quit.
;; Like `isearch-abort', but don't go back to previous successful search:
;; only require one invocation to quit isearch."
;;   (interactive)
;;   (let ((isearch-success t))
;;     (isearch-abort)))
;; (put 'isearch-abort-fsf-style			'isearch-command t)

;; Disabled for now.
;; (with-eval-after-load "isearch-mode"
;;   (define-key isearch-mode-map "\C-g" 'isearch-abort-fsf-style)) ; was isearch-abort))



;; Sieve is a mail filtering language
(defun mde-sieve-mode-hook ()
  (make-local-variable 'inleft-string)
  (setq inleft-string "# "))
(add-hook 'sieve-mode-hook 'mde-sieve-mode-hook)


(defvar char-x4240-string (make-string 1 2208))
(defun sentence-end--add-x4240-char (orig-fun &rest args)
  "Add char x4240 wherever \"\u00a0\" appears in character classes \"[...]\"."
  (replace-regexp-in-string
   "\u00a0" (concat "\u00a0" char-x4240-string)
   (apply orig-fun args)))
(advice-add 'sentence-end :around #'sentence-end--add-x4240-char)


(defun mde-change-log-mode-hook ()
  "Use text mode instead of Changelog Mode for specific files."
  (if (or (string-match "checker-framework.*/docs/CHANGELOG.md" buffer-file-name)
	  (string-match "/java/plume-lib/.*/CHANGELOG.md" buffer-file-name))
      (text-mode)))

(add-hook 'change-log-mode-hook 'mde-change-log-mode-hook)


;;;
;;; Auto filling and visual line mode
;;;

(defun enable-auto-fill ()
  "Turn on auto fill and set the fill column."
  (interactive)
  (auto-fill-mode 1)
  (set-fill-column 75)
  )

;; adaptive-wrap has been renamed visual-wrap-prefix-mode.
(defun my-activate-visual-wrap-prefix-mode ()
  "Turn on `visual-wrap-prefix-mode' iff `visual-line-mode' is on.
Intended to run after `visual-line-mode' runs."
  (if visual-line-mode
      (progn
	(visual-wrap-prefix-mode 1)
	(auto-fill-mode 0))
    (visual-wrap-prefix-mode -1)))
;; `visual-line-mode-hook is called when the mode is activated or deactivated.
(add-hook 'visual-line-mode-hook 'my-activate-visual-wrap-prefix-mode)

(setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; This has to be advice because visual-line-mode-hook is not run early enough
;; to affect variable `fringe-indicator-alist` which is set from variable
;; `visual-line-fringe-indicators` within function `visual-line-mode`.
(defun visual-line-mode--set-visual-line-fringe-indicators (&optional _arg)
  "Set `visual-line-fringe-indicators` to be arrows in both fringes."
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))
(advice-add 'visual-line-mode :before #'visual-line-mode--set-visual-line-fringe-indicators)


;;;
;;; Obfuscating writing style
;;;

(defun obfuscate-writing-style ()
  "For referee reports, change certain idiosyncrasies of my writing style."
  (interactive)
  (save-excursion
    ;; One, not two, spaces at end of sentences
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\(\\w[\"`'()]*[:.?!][\"`'():.?!]*\\)  \\([\"`'()]*\\w\\)" "\\1 \\2")
    ;; Change possessives of words ending in s (this introduces a typo).
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\(\\ws'\\)s\\b" "\\1")
    ))

(defun sentence-end-single-space ()
  (make-local-variable 'sentence-end-double-space)
  (setq sentence-end-double-space nil))

(defun referee-maybe-set-write-file-functions ()
  (if (save-match-data (string-match "\\breview_?forms?[-_]?[0-9]*.txt$\\|\\(tse\\|tosem\\).*review.txt$" buffer-file-name))
      (progn
	(add-hook 'write-file-functions 'obfuscate-writing-style nil 'local)
	;; (setq fill-column 69)		; 70 is too large for Paperdyne
	(sentence-end-single-space)
	(make-local-variable 'colon-double-space)
	(setq colon-double-space nil))))

(add-hook 'find-file-hook 'referee-maybe-set-write-file-functions)


;;;
;;; Converting records to and from tab-separated and newline-separated
;;; 

(defvar convert-record-separator "CNTCS_NEWLINE_GOES_HERE_CNTCS")

(defun convert-newlines-to-tab-separated ()
  "Convert newline-separated records to tab-separated records."
  (interactive)
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (replace-string-noninteractive "\n\n" convert-record-separator)
      (goto-char (point-min))
      (replace-string-noninteractive "\n" "\t")
      (goto-char (point-min))
      (replace-string-noninteractive convert-record-separator "\n"))))

(defun convert-newlines-to-comma-separated ()
  "Convert newline-separated records to comma-separated records."
  (interactive)
  (save-restriction
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (insert "\"")
      (insert convert-record-separator)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (insert "\"")
      (replace-string-noninteractive "\n\n"
		                     (concat "\"" convert-record-separator "\""))
      (goto-char (point-min))
      (replace-string-noninteractive "\n" "\",\"")
      (goto-char (point-min))
      (replace-string-noninteractive convert-record-separator "\n"))))

(defun convert-tab-separated-to-newlines ()
  "Convert tab-separated records to newline-separated records."
  (interactive)
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (replace-string-noninteractive "\n" convert-record-separator)
      (goto-char (point-min))
      (replace-string-noninteractive "\t\t" "\t \t")
      (goto-char (point-min))
      (replace-string-noninteractive "\t" "\n")
      (goto-char (point-min))
      (replace-string-noninteractive convert-record-separator "\n\n"))))

(defun convert-comma-separated-to-newlines ()
  "Convert tab-separated records to newline-separated records."
  (interactive)
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (delete-char 1)
      (replace-string-noninteractive "\n" convert-record-separator)
      (goto-char (point-min))
      (replace-string-noninteractive "\",\"" "\n")
      (goto-char (point-min))
      (replace-string-noninteractive convert-record-separator "\n\n")
      (goto-char (point-min))
      (replace-string-noninteractive "\n\"" "\n")
      (goto-char (point-min))
      (replace-string-noninteractive "\"\n" "\n"))))


;;;
;;; Page delimitors and separators
;;;

;; I'm not sure what I really want for page-delimiter.
;; This results in very slow searches; figure out how to make it faster.
;; page-delimiter shouldn't end with + or * if backward-page is to work.
(defun mde-page-delimiter (char)
  "Delimiter for the start of a section heading:
a row of comment characters, then a row with three comment characters plus
a title, then a row with three comment characters.
Comment character is CHAR.
Actually CHAR may be a string as well. CHAR should be regexp-quoted already."
  (concat
   ;; (regexp-opt ... 'output-parens) infinite-loops, so use this instead
   "\\("
   (mapconcat 'identity
	      (list
	       ;; form feed, newline, four comment characters
	       (format "\f\n%c%c%c%c" char char char char)
	       ;; blank line, three+ comment chars, optional hyphens,
	       ;; newline, comment chars
	       (format "[\n\f]\n%c%c%c+-*\n%c+" char char char char)
	       ;; blank line, three+ comment chars, optional hyphens,
	       ;; newline, comment chars, newline, comment chars (Erik Ruf's style)
	       (format "[\n\f]\n%c%c%c+-*\n%c+\n%c+" char char char char char)
	       ;; form feed, newline, three+ comment chars, required space
	       (format "\f\n%c%c%c+ " char char char)
	       ;; beginning of file
	       (format "\\`%c+" char))
	      "\\|")
   "\\)"
   ;; Whitespace and a word beginning
   "\\s *\\<"))
;; (mde-page-delimiter ?;)


;; Cope with my text separator style, which looks like
;;   Section name
;;   ============

(defvar text-page-delimiter-equals "[^\n]\n===+\n[^=]")
(defvar text-page-delimiter-hypens "[^\n]\n---+\n[^-]")
(defvar text-page-delimiter-equals-or-hyphens
  (concat text-page-delimiter-equals
	  "\\|" text-page-delimiter-hypens))

(defun pages-directory--set-page-delimiter (&rest _args)
  "Set `page-delimiter' if in text mode and it doesn't match in the buffer."
  (save-excursion
    (if (and (eq major-mode 'text-mode)
	     (not (save-excursion
		    (goto-char (point-min))
		    (re-search-forward page-delimiter nil t))))
	(cond ((equal ".wiki" (substring (buffer-file-name) -5))
	       (setq page-delimiter "\n---+\n="))
	      (t
	       (let ((equals-count (progn
				     (goto-char (point-min))
				     (how-many text-page-delimiter-equals))))
		 (if (> equals-count 3)
		     (setq page-delimiter text-page-delimiter-equals)
		   (let ((equals-or-hypens-count
			  (progn
			    (goto-char (point-min))
			    (how-many text-page-delimiter-equals-or-hyphens))))
		     (if (> equals-or-hypens-count 3)
			 (setq page-delimiter text-page-delimiter-equals-or-hyphens))))))))))
(advice-add 'pages-directory :before #'pages-directory--set-page-delimiter)

(defun pages-copy-header-and-position--text-mode-adjust-point (orig-fun count-lines-p)
  "Adjust point if using my text mode page delimiter.
That delimiter follows the section name rather than preceding it."
  (save-excursion
    (if (or (eq page-delimiter text-page-delimiter-equals)
	    (eq page-delimiter text-page-delimiter-equals-or-hyphens))
	(forward-line -3))
    (funcall orig-fun count-lines-p))
  (end-of-line 1)			; from original definition
  )
(advice-add 'pages-copy-header-and-position :around
            #'pages-copy-header-and-position--text-mode-adjust-point)

(defun pages-copy-header-and-position--html-mode-adjust-point (count-lines-p)
  "Adjust point if using my text mode page delimiter.
That delimiter follows the section name rather than preceding it."
  (if (eq page-delimiter html-headers-page-delimiter)
      (beginning-of-line)))
(advice-add 'pages-copy-header-and-position :before
            #'pages-copy-header-and-position--html-mode-adjust-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TeX
;;;

;; Emacs's built-in TeX support is named "tex-*"
;; and is loaded by (require 'tex-mode).
;; AucTeX functions are generally named "TeX-*"
;; and are loadod by (require 'tex-site) or (require 'tex) or (require 'latex).
;; I don't use AucTeX any longer.


;; (setq-default tex-main-file nil)
(defvar tex-main-file-defaults
  `(
    ("~/prof/grants/ac/application_communities/design/" "design")
    ("~/research/fcut/theory_case_studies/" "studies")
    ("~/research/types/checker-framework/checker/manual/" "manual")
    ("~/research/db/vldb2010_bu/HaLoop/tex/" "../HaLoop")
    ("~/research/testing/cse503zbw-docs/project-proposal/tex/" "../proposal")
    ("~/research/testing/failuredoc/paper/tex/" "../autodoc")
    ("~/research/vakilian/2015-ICSE-TQI/paper/text/" "2015-icse-tqi.tex")
    ("~/prof/grants/2016-11-nsf-cri-defects4j/sections/" "../proposal")
    ("~/research/testing/fault-localization-paper/tables/" "../evaluating-improving-fl/paper.tex")
    ("~/research/nlp/nl2cmd_paper/" "nl2cmd")
    ("~/research/types/index-checker-paper/" "index-checker")
    ("~/research/types/aws-compliance-paper/" "continuous-compliance")
    ("/homes/gws/clones/types/aws-compliance-paper/" "continuous-compliance")
    ("~/research/neurips-fairness-paper/" "neurips_2020.tex")
    ("~/research/types/resource-leak-paper/" "resource-leak.tex")
    ("/scratch/mernst/clones/types/resource-leak-paper" "resource-leak.tex")
    ("~/research/types/accumulation-paper/" "accumulation.tex")
    ("~/research/types/wpi-paper/" "wpi.tex")
    )
  "Assoc list of directories and tex-main-file settings.
`mde-tex-mode-hook' handles well-known filenames:  main, paper, article,
proposal")
(make-variable-buffer-local 'tex-main-file)
(defun set-tex-main-file ()
  "Set the `tex-main-file' variable appropriately."
  (if (not tex-main-file)
      (progn
	(setq tex-main-file
	      (or (cadr (assoc (abbreviate-file-name default-directory)
			       tex-main-file-defaults))
		  (and (file-exists-p "main.tex") "main")
		  (and (file-exists-p "paper.tex") "paper")
		  (and (file-exists-p "article.tex") "article")
		  (and (file-exists-p "proposal.tex") "proposal")
		  (and (file-exists-p "p.tex") "p") ; gross name used by some people
		  (let ((dir-basename (file-name-nondirectory
				       (directory-file-name default-directory))))
		    (and (file-exists-p (concat dir-basename ".tex")) dir-basename))
		  (and (file-exists-p "../main.tex") "../main")
		  (and (file-exists-p "../paper.tex") "../paper")))
	(setq TeX-master tex-main-file))))

(autoload 'save-buffer-if-modified "util-mde"
  "Save buffer if it exists and is modified." t)

;; tex-compile calls `save-some-buffers', but I don't want questions about the current buffer.
(defun tex-compile--save-buffer-if-modified (_dir _cmd)
  "Save current buffer before executing a TeX command."
  (save-buffer-if-modified))
(advice-add 'tex-compile :before #'tex-compile--save-buffer-if-modified)

;; default 1.1, which means use varying height faces
(setq font-latex-fontify-sectioning 'color)

(eval-when-compile
  (require 'tex nil 'noerror))

(defun mde-tex-mode-hook ()
  "Michael Ernst's TeX mode hook."
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (setq indent-tabs-mode nil)		; never insert tab characters
  (setq fill-column 75)
  (make-local-variable 'inleft-string)
  (setq inleft-string "% ")
  (setq paragraph-start "^[ \t]*$\\|^[\f\\%]\\|^.*[^\\]%\\|^\\$\\$")
  (setq paragraph-separate paragraph-start)

  (set-tex-main-file)

  (if (member (file-name-nondirectory (directory-file-name default-directory))
	      '("control-plane-verification"
		"verified-system-transformers"))
      (let ((local-map (copy-keymap (current-local-map))))
	(define-key local-map "\C-c\C-c" 'compile) ; use make instead of pdflatex
	(use-local-map local-map)))

  ;; Delete trailing whitespace for some files
  (if (string-match "docs/manual$" (directory-file-name default-directory))
      (add-hook 'before-save-hook 'delete-trailing-whitespace))
  )
(add-hook 'tex-mode-hook 'mde-tex-mode-hook)
(add-hook 'latex-mode-hook 'mde-tex-mode-hook)

(add-hook 'latex-mode-hook 'turn-on-reftex)


(with-eval-after-load "ispell"
  (setq ispell-tex-skip-alists
	(list
	 (append
	  '(
	    ("\\\\epsfig"		ispell-tex-arg-end)
	    ("\\\\[cC]ref"		ispell-tex-arg-end)
	    ("\\\\autoref"		ispell-tex-arg-end)
            ;; formatting in my dissertation
	    ("\\\\func"		ispell-tex-arg-end)
	    ("\\\\inv"		ispell-tex-arg-end)
	    ("\\\\prog"		ispell-tex-arg-end)
	    ("\\\\type"		ispell-tex-arg-end)
	    ("\\\\var"		ispell-tex-arg-end)
            ;; Checker Framework manual
	    ("\\\\chapterpageref"	ispell-tex-arg-end)
	    )
	  (car ispell-tex-skip-alists))
	 (append
	  '(
            ;; like "verbatim" rule, but with "Verbatim" (capitalized)
	    ("Verbatim\\*?" . "\\\\end[ 	\n]*{[ 	\n]*Verbatim\\*?[ 	\n]*}")
	    )
	  (car (cdr ispell-tex-skip-alists))))))

(with-eval-after-load "tex-mode"
  (setq tex-verbatim-environments
	(append tex-verbatim-environments
		'("Verbatim"))))

;; should extend this to work only over the current region
(defun remove-tex-comments ()
  "Remove TeX/LaTeX comments; but prefer the `latex-process-inputs' program."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^\\]%" nil t)
      (delete-region (1+ (match-beginning 0)) (progn (end-of-line) (point))))))

(defun cleanup-latex-output ()
  "Remove uninteresting LaTeX errors."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-matching-paragraphs "^\\(Over\\|Under\\)full \\\\hbox")
    (delete-matching-paragraphs "^LaTeX Warning: \\(Citation\\|Reference\\)" nil nil t)
    ))

(defun use-brocket-tt ()
  "Change uses of \\texttt{...} to \\<...>."
  (tags-query-replace "\\\\texttt{\\([^{}<>]+\\)}" "\\\\<\\1>")
  (tags-query-replace "\\\\texttt{\\([^{}]+\\)}" "\\\\codeid{\\1}")
  )


;;;
;;; BibTeX
;;;

(setq tex-bibtex-command "bibtex --min-crossrefs=9999")

(with-eval-after-load "bibtex"
  (setq bibtex-user-optional-fields
	(append bibtex-user-optional-fields
		'(
		  ("doi" "The Digital Object Identifier")
		  ;; Fields for bibtex2web
		  ("abstract" "The abstract, in LaTeX format")
		  ("usesDaikon" "Set if the paper uses the Daikon invariant detector")
		  ("usesDaikonAsTestSubject" "Set if the paper uses Daikon as a test subject")
		  ("omitfromcv" "Set if the paper should not be publicized")
		  ("underreview" "Set if the paper is under review")
		  ("basefilename" "File name for bibtex2web")
		  ("downloads" "List of other downloads (semicolon-separated)")
		  ("downloadsnonlocal" "List of otehr downloads (if the \"basename\" file is not found)")
		  ("supersededby" "Comma-separated list of keys of articles that supersede this one")
		  ("category" "Category/topic for this article, in bibtex2web output")
		  ("summary" "A very brief desciption (3 lines)")))))

(setq bibtex-comment-start "%")
(setq bibtex-comma-after-last-field t)
(setq bibtex-autoadd-commas t)
(setq bibtex-maintain-sorted-entries nil)
(setq bibtex-files '(bibtex-file-path))

(setq-default bibtex-field-delimiters 'braces)

(eval-when-compile (require 'bibtex))
(defun mde-bibtex-mode-hook ()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter (mde-page-delimiter ?%))
  ;; (define-key bibtex-mode-map "\"" 'self-insert-command)
  ;; Done by variable prefix-help-command, bound to describe-prefix-bindings.
  ;; (define-key bibtex-mode-map "\C-c\C-e\C-h" 'prefix-help)
  (setq inleft-string "% ")
  ;;  (setq paragraph-start "^[%} \t\f]*$"
  ;;        paragraph-separate paragraph-start)
  (setq ;; `forward-paragraph' adds leading [ \t]*
   paragraph-start "\\([%}\n]\\|[ \t\f]+\\($\\|[^ \t]\\)\\)"
   ;; NOT the same as paragraph-start, else a one-line paragraph is
   ;; considered merely a separator.
   paragraph-separate "[%} \t\f]*$")
  (setq fill-column 80)
  ;; I want a nil fill-prefix outside BibTeX entries, but the existing one
  ;; is OK inside them...
  (setq fill-prefix nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  )
(add-hook 'bibtex-mode-hook 'mde-bibtex-mode-hook)

(with-eval-after-load "bibtex"
  (define-key bibtex-mode-map [(meta backspace)] 'backward-kill-word)) ; was bibtex-mark-entry

;;; Experimentally commented out, 2025-04-06.
;; (defadvice bibtex-move-outside-of-entry (after move-past-brace activate)
;;   (if (looking-at "}$")
;;       (progn
;; 	(forward-char 2)
;; 	(insert "\n\n"))))

(defun fixup-bibliographies ()
  "Fix common typographical errors in bibliographies. Uses current TAGS table."
  (interactive)
  ;; (tags-query-replace-noerror "\\(pages.*=.*[0-9]\\)-\\([0-9]\\)" "\\1--\\2")
  ;; (tags-query-replace-noerror "\\(month.*=.*[0-9]\\)-\\([0-9]\\)" "\\1--\\2")
  (tags-query-replace-noerror "\\(month.*=.*[0-9]\\)\"" "\\1,\"")

  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"january\"\\(\\s-*,\\)" "\\1jan\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"february\"\\(\\s-*,\\)" "\\1feb\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"march\"\\(\\s-*,\\)" "\\1mar\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"april\"\\(\\s-*,\\)" "\\1apr\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"may\"\\(\\s-*,\\)" "\\1may\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"june\"\\(\\s-*,\\)" "\\1jun\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"july\"\\(\\s-*,\\)" "\\1jul\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"august\"\\(\\s-*,\\)" "\\1aug\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"september\"\\(\\s-*,\\)" "\\1sep\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"october\"\\(\\s-*,\\)" "\\1oct\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"november\"\\(\\s-*,\\)" "\\1nov\\2")
  (tags-query-replace-noerror "\\(month\\s-*=\\s-*\\)\"december\"\\(\\s-*,\\)" "\\1dec\\2")

  ;; Don't try to fix when the day of the month precedes the month, because
  ;; that usually happens when it's in a foreign country (and it should
  ;; remain like that, maybe??)
  ;; (tags-search "#\\s-*\\(jan\\|feb\\|mar\\|apr\\|may\\|jun\\|jul\\|aug\\|sep\\|oct\\|nov\\|dec\\)")

  (message "Done fixing up bibliographies")
  )

(defun find-missing-downloadsnonlocal ()
  "Report missing downloadsnonlocal entries in the current BibTeX buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*add-downloadsnonlocal*"))
  (widen)
  (erase-buffer)
  (insert-file-contents (expand-file-name "ernst.bib" "~/bib/"))
  (text-mode)
  (setq paragraph-separate "[ \t\f]*$"
	paragraph-start "\f\\|[ \t]*$")
  (goto-char (point-min))
  (delete-non-matching-paragraphs "basefilename")
  (goto-char (point-min))
  (delete-matching-paragraphs "^% LocalWords")
  (goto-char (point-min))
  ;;  (delete-matching-paragraphs "\\bsupersededby\\b")
  (goto-char (point-min))
  (delete-matching-paragraphs "\\bomitfromcv = 1")
  (goto-char (point-min))
  (while (not (eobp))
    (let ((start (point))
	  (end (progn (forward-paragraph) (point))))
      (narrow-to-region start end)
      ;; bibtex-mode may provide a cleaner way to iterate and access fields
      (goto-char start)
      (re-search-forward "^[ \t]*basefilename[ \t]*=[ \t]*\"\\(.*\\)\"")
      (let ((basefilename (match-string 1)))
	(let* ((files
		(cl-delete-if
		 #'(lambda (f)
		     (goto-char (point-min))
		     (re-search-forward
		      (concat "downloadsnonlocal[ \t\n]*=[ \t\n]*\"[^\"]*" f " ")
		      nil t)
		     (not (= (point) (point-min))))
		 (cl-delete-if
		  #'(lambda (f) (or (string-match "-abstract\\.html$" f)
				    (string-match "\\.ps$" f)))
		  (directory-files "~/public_html/pubs/" nil basefilename nil))))
	       (entries
		(remove nil
			(mapcar #'(lambda (download-type)
				    (make-downloadsnonlocal-maybe
				     basefilename download-type files))
				download-types))))
	  (goto-char (point-max))
	  (if entries
	      (progn
		(insert "MISSING:\n")
		(insert "  downloadsnonlocal =\n   \"")
		(insert (string-join entries ";\n    "))
		(insert "\",\n"))))))
    (goto-char (point-max))
    (widen))
  (goto-char (point-min))
  (delete-non-matching-paragraphs "MISSING:")
  (goto-char (point-min))
  )

(defvar download-types
  '((".pdf" "PDF")
    (".pdf.gz" "PDF (gzipped)")
    (".doc" "MS Word")
    (".docx" "MS Word")
    (".doc.gz" "MS Word (gzipped)")
    ("-slides.pdf" "slides (PDF)")
    (".ppt" "slides (PowerPoint)")
    (".pptx" "slides (PowerPoint)")
    (".ppt.gz" "slides (PowerPoint (gzipped))")
    (".odp" "slides (ODP)")))

(defun make-downloadsnonlocal-maybe (basename download-type files)
  "Returns nil or a downloadsnonlocal entry."
  (let* ((extension (cl-first download-type))
	 (label (cl-second download-type))
	 (filename (concat basename extension)))
    (if (member filename files)
	(concat "http://homes.cs.washington.edu/~mernst/pubs/" filename " " label))))

;; This was a one-off for my dissertation, probably.
(defun bib-show-properties ()
  "Show the PROPERTIES: paragraph of each BibTeX entry that has one."
  (interactive)
  (with-current-buffer (get-buffer-create "bib-properties")
    (erase-buffer))
  (save-excursion
    (goto-char (point-min))
    (let (properties prop-end bibentry)
      (while (re-search-forward "^[ \t]*PROPERTIES:" nil t)
        (setq properties (buffer-substring (match-beginning 0)
                                           (progn (search-forward "\n\n")
                                                  (point)))
              prop-end (point)
              bibentry (buffer-substring (progn ;; (beginning-of-bibtex-entry)
                                           (bibtex-beginning-of-entry)
                                           (search-forward "{")
                                           (point))
                                         (progn (end-of-line) (point))))
        (goto-char prop-end)
        (with-current-buffer (get-buffer "bib-properties")
          (insert bibentry "\n" properties)))))
  (switch-to-buffer "bib-properties")
  (set-buffer-modified-p nil))

;;;
;;; Texinfo
;;;

(defun mde-texinfo-mode-hook ()
  (auto-fill-mode 1)
  (setq fill-column 75)
  (make-local-variable 'inleft-string)
  (setq inleft-string "@c "))
(add-hook 'texinfo-mode-hook 'mde-texinfo-mode-hook)

(with-eval-after-load "info"
  (define-key Info-mode-map [(meta s)] 'Info-search) ; was unbound
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML
;;;

;; For "It's All Text" Chrome plugin
(add-to-list 'auto-mode-alist
	     '("/www\\.blogger\\.com\\.[^/]+\\.txt\\'" . html-mode))

(defvar html-headers-page-delimiter "\n *<h[1-6]")
(defun mde-html-mode-hook ()
  (mde-text-mode-hook)
  ;; Try using the default.
  ;; (setq paragraph-start "^[ \t]*$\\|^[\f<]")
  ;; (setq paragraph-separate paragraph-start)
  (setq indent-tabs-mode nil)		; never insert tab characters

  (make-local-variable 'page-delimiter)
  (setq page-delimiter html-headers-page-delimiter)
  )
(add-hook 'html-mode-hook 'mde-html-mode-hook)

;; Make Emacs automatically run html-update-toc on HTML files as you save them.
(load-library "html-update.el")

(defun html-add-heading-anchors ()
  "Add anchors to section headings in an HTML document."
  (interactive)
  (require 'dired)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(<h[1-6]\\)\\([^>]*>\\)\\([^<>\n]+\\)\\(</h[1-6]>\\)[ \t]*" nil t)
      (let ((hextra (match-string 2))
            (hname (match-string 3)))
        (if (not (save-match-data (string-match " id=" hextra)))
            (let ((anchor-name
                   (save-match-data (replace-regexp-in-string " " "_"
                                                              (replace-regexp-in-string "'" "" hname)))))
              (replace-match (concat " id=\"" anchor-name "\"" hextra) nil t nil 2)))))))

(defun html-fixup-paragraphs ()
  "Make sure paragraphs in HTML source start with <p> and end with </p>."
  (interactive)
  (save-excursion
    (let* ((list-start-or-newline-nomatch "\\(?: *\n\\| *<\\(?:[dou]l\\|li\\)>\\)")
	   (list-start-or-newline (concat "\\(" list-start-or-newline-nomatch "\\)"))
	   (parbreak-re (concat "\\(\n" list-start-or-newline-nomatch "\\)")))
      (goto-char (point-min))
      ;; Add initial <p>
      (query-replace-regexp
       (concat list-start-or-newline "\n\\([ \t]*\\(?:[^<\n \t]\\|<a \\|<b>\\)\\)")
       "\\1\n<p>\n\\2" nil nil nil)
      (goto-char (point-min))
      ;; Add final </p>
      (query-replace-regexp (concat "\\([^>\n \t]\\)[ \t]*\n" list-start-or-newline) "\\1\n</p>\n\\2" nil nil nil)
      ;; Convert final <p> into </p>
      (goto-char (point-min))
      (query-replace-regexp (concat "<p>" parbreak-re) "</p>\\1" nil nil nil)
      ;; Remove unneeded </p>
      (goto-char (point-min))
      (query-replace-regexp "\\(</[dou]l>\n?\\)</p>" "\\1" nil nil nil)
      ;; Fix double-quote marks
      (goto-char (point-min))
      (search-forward "/head" nil t)	; don't replace in header
      (query-replace-regexp "\\([\[\(> \t\n]\\)\"" "\\1&ldquo;" nil nil nil) ;
      (goto-char (point-min))
      (search-forward "/head" nil t)	; don't replace in header
      ;; avoid matching: =".
      (query-replace-regexp "\\([^=]\\)\"\\([\]\).?!,;:< \t\n]\\)" "\\1&rdquo;\\2" nil nil nil)
      ;; Fix em dashes
      (goto-char (point-min))
      (query-replace-regexp "\\([ \n]\\)--\\([ \n]\\)" "\\1&mdash;\\2" nil nil nil)
      ;; Itemized lists
      (goto-char (point-min))
      (query-replace-regexp "^ \\* " "  </li>\n  <li>" nil nil nil)
      (goto-char (point-min))
      (query-replace-regexp "\\(<p>[^<]*\n\\) *</li>" "\\1</p>\n<ul>" nil nil nil)
      (goto-char (point-min))
      (query-replace-regexp "\\(^\\( *\\)<li>[^<]*\\)</p>" "\\1\\2</li>\n</ul>" nil nil nil)
      (goto-char (point-min))
      (query-replace-regexp "^[ \t]*<p>\n[ \t]*</p>\n" "" nil nil nil)
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;

;; I could modify find-file-noselect to run dired either if the argument is
;; a directory or if it contains a wildcard. But I can always just invoke
;; dired with a wildcard argument.

(with-eval-after-load "dired"
  (load "dired-mde" nil t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell
;;;

(if (fboundp 'coterm-mode)
    (coterm-mode))
;; Optional: bind `coterm-char-mode-cycle' to C-; in comint
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-;") #'coterm-char-mode-cycle))
;; If your process repeats what you have already typed, try customizing
;; `comint-process-echoes':
;;   (setq-default comint-process-echoes t)

(require 'mouse-goto-error)

;; I wish to be able to use aliases in `shell-command' and `dired-do-shell-command'.
;; The recommendation for accomplishing this is
;;   (setq shell-command-switch "-ic")
;; which reads the .aliases file but has a major disadvantage:
;; On my home machine (but not at CSE?) it leads to the following error on every shell-command:
;;    bash: cannot set terminal process group (-1): Inappropriate ioctl for device
;;    bash: no job control in this shell
;; Running "set +m" or adding "+m" as a command-line argument
;; might fix the problem, but shell-command-switch must be a single argument.
;; Instead, ensure that .bashrc contains "shopt -s expand_aliases" and use this:
;; I tried (setq shell-command-switch "-lc")
;; but am currently experimenting with undoing it, because I'm having other
;; problems that may or may not be related to it.

;; These next three don't work unless comint-prompt-regexp is set right.
(defun mde-shell-mode-hook ()
  (auto-fill-mode 0)
  ;; $HOST and (system-name) may be the same (both long), or $HOST may be
  ;; short while system-name is long.
  (let* ((hostname (or (getenv "HOST") ; not set when running emacs directly via ssh, apparently
		       (system-name)))
	 (name-regexp
	  (if (string-match "^\\([^.]*\\)\\(\\..*\\)$" hostname)
	      ;; hostname contains a period
	      (let ((hostname-base (match-string 1 hostname))
		    (hostname-extra (match-string 2 hostname)))
		(concat (regexp-quote hostname-base)
			"\\("
			(regexp-quote hostname-extra)
			"\\)?"))
	    ;; hostname contains no period
	    (regexp-quote hostname))))
    (setq comint-prompt-regexp
	  (concat
	   ;; "\r?" because sometimes I get extranous ^M at beginning of line.
	   ;; (Is that still necessary?)
	   "^\r?"
	   ;; for Python virtual environments
	   "\\(([.a-z]+) \\)?"
	   name-regexp
	   ;; "?" because when running zsh, no prompt number
	   "\\( [0-9]+\\)?% "))))
(add-hook 'shell-mode-hook 'mde-shell-mode-hook)

;; `comint-delete-output', bound to C-c C-o, removes the output of the
;; preceding command line. This command is slightly different in that it
;; can be used repeatedly whereas subsequent invocations of
;; comint-delete-output have no effect.
;; (defun shell-remove-last-command ()
;;   "Eliminate preceding command line and its output."
;;   (interactive)
;;   (goto-char (point-max))
;;   (beginning-of-line)
;;   ;; special case for me: "\\([a-z]* \\)\\([0-9]+\\)% "
;;   (if (not (looking-at comint-prompt-regexp))
;;       (error "Didn't find prompt at beginning of last line"))
;;   (if (not (re-search-backward comint-prompt-regexp))
;;       (error "Didn't find previous prompt"))
;;   (delete-region (match-end 0) (point-max))
;;   (goto-char (point-max)))

(defun comint-next-input--go-to-command-line (_arg)
  "When invoked not at a command line, go to next command line."
  (if (not (comint-after-pmark-p))
      (if (not (re-search-forward comint-prompt-regexp nil t))
	  (error "Can't find next command line"))))
(advice-add 'comint-next-input :before #'comint-next-input--go-to-command-line)

(defun comint-previous-input--go-to-command-line (_arg)
  "When invoked not at a command line, go to previous command line."
  (if (not (comint-after-pmark-p))
      (if (progn
	    ;; like (beginning-of-line 1), but ignore field boundaries
	    (forward-line 0)
	    (re-search-backward comint-prompt-regexp nil t))
	  (goto-char (match-end 0))
	(error "Can't find previous command line"))))
(advice-add 'comint-previous-input :before #'comint-previous-input--go-to-command-line)

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil nil)

;; ;; Like the above, but for ACL
;; (defadvice fi:push-input (around next-command-line activate)
;;   "When invoked not at a command line, go to next command line."
;;   (if (not (comint-after-pmark-p))
;;       (if (not (re-search-forward comint-prompt-regexp nil t))
;; 	  (error "Can't find previous command line"))
;;     ad-do-it))
;; (defadvice fi:comint-previous-input (around previous-command-line activate)
;;   "When invoked not at a command line, go to previous command line."
;;   (if (not (comint-after-pmark-p))
;;       (if (progn
;; 	    ;; like (beginning-of-line 1), but ignore field boundaries
;; 	    (forward-line 0)
;; 	    (re-search-backward comint-prompt-regexp nil t))
;; 	  (goto-char (match-end 0))
;; 	(error "Can't find previous command line"))
;;     ad-do-it))

(defun mde-comint-mode-hook ()
  (auto-fill-mode 0))
(add-hook 'comint-mode-hook 'mde-comint-mode-hook)

(defun shell-command-buffer (&optional output-buffer)
  "Return the output buffer for a shell command."
  (let ((output-buffer-buffer (and output-buffer
                                   (if (eq output-buffer t)
                                       (current-buffer)
                                     (or (get-buffer output-buffer)
                                         (current-buffer))))))
    (or output-buffer-buffer (get-buffer shell-command-buffer-name))))

(defun shell-command--set-diff-mode (_command &optional output-buffer _error-buffer)
  "Set mode of buffer *Shell Command Output* to diff-mode if appropriate."
  (let ((buffer (shell-command-buffer output-buffer)))
    (if buffer
	(with-current-buffer buffer
	  (save-excursion
	    (goto-char (point-min))
	    (cond ((looking-at "diff ")
		   (diff-mode))
		  ((and (looking-at "commit ")
			(save-excursion
			  (re-search-forward "\n\ndiff --git a/" nil t)))
		   (diff-mode))
		  (t (fundamental-mode))))))))
(advice-add 'shell-command :after #'shell-command--set-diff-mode)


(defun shell-command--auto-browse (_command &optional output-buffer _error-buffer)
  (let ((buffer (shell-command-buffer output-buffer)))
    (if buffer
	(with-current-buffer buffer
	  (save-excursion
	    (goto-char (point-min))
            (browse-url-once-if-matched "You can view, comment on, or merge this pull request online at:\n\n *\\(https://github.com/.*/pull/[0-9]+\\)" 1)
            (browse-url-once-if-matched "remote: Create a pull request for .*: *\nremote: *\\(https://github.com/.*/pull/new/[^ \n]*\\)" 1)
            (browse-url-once-if-matched "remote: Create pull request for .*: *\nremote:   \\(https://bitbucket.org/.*/pull-requests/new?source=.*&t=1\\)" 1)
            (browse-url-once-if-matched "remote: To create a merge request for .*, visit: *\nremote:   \\(https://gitlab\\..*/merge_requests/new.*?\\) " 1))))))
(advice-add 'shell-command :after #'shell-command--auto-browse)

(with-eval-after-load "diff-mode"
  (define-key diff-mode-map (kbd "<C-return>") 'ediff-hunk))

;; from https://stackoverflow.com/questions/4726220/how-to-have-colors-in-the-output-of-emacs-shell-command

(defun xterm-color-colorize-shell-command-output ()
  "Colorize `shell-command' output."
  (let ((bufs
         (seq-remove
          (lambda (x)
            (not (or (string-prefix-p " *Echo Area" (buffer-name x))
                     (string-prefix-p "*Shell Command" (buffer-name x)))))
          (buffer-list))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (xterm-color-colorize-buffer)))))

;; (defun xterm-color-colorize-shell-command-output-advice (proc &rest rest)
;;   (xterm-color-colorize-shell-command-output))
;; 
;; (advice-add 'shell-command :after #'xterm-color-colorize-shell-command-output-advice)
;; ;; (advice-remove 'shell-command #'xterm-color-colorize-shell-command-output-advice)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Etc.
;;;

;; (load "server")
;; (add-hook 'server-visit-hook
;;           (function
;;            (lambda ()
;;              (add-hook 'kill-buffer-hook
;;                        (function
;;                         (lambda ()
;;                           (server-buffer-done (current-buffer))))))))
;; (server-start)

(with-eval-after-load "hexl"
  (defun hexl-print-buffer ()
    "Print the current buffer, double-spaced, with file name at the top."
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     (concat "enscript -1R -s11 -b "
	     (file-name-nondirectory (buffer-file-name (current-buffer)))))))

(provide 'mode-hooks-mde)

;;; mode-hooks-mde.el ends here
