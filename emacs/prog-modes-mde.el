;;; -*- lexical-binding: t -*-

;;; prog-modes-mde.el --- Michael Ernst's Emacs mode hooks for programming languages

;;; Commentary:

;; Much of the hook stuff should use add-hook instead of just setq.

;;; Code:


;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)
(use-package treesit-auto
  ;; :custom
  ;; (treesit-auto-install 'prompt)
  :config
  ;; (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(eval-when-compile
  (require 'apheleia)
  (require 'cl-lib)			; for `cl-assert'
  (require 'compile)
  (require 'cc-cmds)
  (require 'pylookup nil 'noerror)
  (require 'groovy-mode nil 'noerror)
  (require 'util-mde)                   ; for `save-buffer-if-modified', `replace-string-noninteractive', etc.
  (require 'mode-hooks-mde)
  (require 'javadoc-lookup)
  (require 'dtrt-indent nil 'noerror)
  (require 'string-inflection)
  (require 'xscheme)
  )

(autoload 'save-buffer-if-modified "util-mde"
  "Save buffer if it exists and is modified." t)
(autoload 'replace-string-noninteractive "util-mde"
  "Like `replace-string', but doesn't modify mark or the mark ring.")


(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(defvar dont-check-parens nil
  "A user should set this to non-nil to avoid errors \"Unmatched bracket or quote\".
For instance, set it when editing files with version control merge conflicts.")
(defvar check-parens-previous-try nil
  "A buffer name if the previous call to check-parens failed.
Nil if the previous call to check-parens succeeded.
There might or might not have been edits between the two attempts.")
;; This should probably check that the buffer was not edited in between...
(defun check-parens-ignore-on-retry ()
  "Like `check-parens' (which see), but a second retry in a row causes success.
This is good for modes like Perl, where the parser can get confused."
  (if (and (not dont-check-parens)
           (not (equal check-parens-previous-try (buffer-name))))
      (progn
        (setq check-parens-previous-try (buffer-name))
        (check-parens)
	;; If check-parens finds a problem, it throws an exception
	;; and check-parens-previous-try does not get set to nil.
        (setq check-parens-previous-try nil))))


;; This causes asynchronous behavior.  I need to decide whether I like that.
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)

;; Automatically format buffers when saving them.
(use-package apheleia)
;; For debugging:
(setq apheleia-log-only-errors nil)
(setq apheleia-log-debug-info t)
(with-eval-after-load "apheleia"
  (setf (alist-get 'python-mode apheleia-mode-alist)
        'ruff)
  ;; out of the box, apheleia uses the script name "google-java-format" which doesn't exist
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("run-google-java-format.py" inplace))
  ;; out of the box, apheleia doesn't format shell scripts
  (setf (alist-get 'sh-mode apheleia-mode-alist)
        'shfmt)
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-filename" filepath "-ln"
          (cl-case (bound-and-true-p sh-shell) (sh "posix") (t "bash"))
          "-ci" "-bn" "-sr"
          (when apheleia-formatters-respect-indent-level
            (list "-i"
                  (number-to-string
                   (cond (indent-tabs-mode 0)
                         ((boundp 'sh-basic-offset) sh-basic-offset)
                         (t 2)))))
          "-"))
  )


(setq-default indent-tabs-mode nil)

;; sideline is too distracting.
(setq lsp-ui-sideline-enable nil)


(defun run-command (buffer command &rest args)
  "Run `command' and show output if it fails.  BUFFER is optional.
Intended for use in after-save-hook."
  (if (not buffer)
      (setq buffer (concat "*run-" command "*")))
  (if (get-buffer buffer)
      (with-current-buffer (get-buffer buffer)
	(erase-buffer)))
  (let* ((compilation-ask-about-save nil)
         (process-status (save-excursion (apply #'call-process command nil buffer nil args))))
    (if (not (equal process-status 0))
	(progn
	  (pop-to-buffer buffer)
	  (error "Command failed in %s: %s %s" default-directory command args)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key maps
;;;

(defun swap-return-and-linefeed ()
  "Swap the return and linefeed keys."
  ;; This doesn't use keyboard-translate because it should apply only in
  ;; certain situations.
  ;; Values are in octal.
  (local-set-key "\15" 'newline-and-indent)
  (local-set-key "\12" 'newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C and C++
;;;

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq-default c-recognize-knr-p nil)

(defun mde-c-mode-hook ()
  "Michael Ernst's C mode hook."
  (if (featurep 'elide-head-mode)
      (elide-head-mode))
  (swap-return-and-linefeed)
  (local-set-key "\C-c\C-c" 'compile)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter
        (concat "^\f\\|"
                (regexp-quote "/* ***************************************************************************")
                "\\|// //////////////////////////////////////////////////////////////////////"))
  (c-set-compile-command)

  (dtrt-indent-mode t)

  (setq indent-tabs-mode nil)		; never insert tab characters

  (turn-on-font-lock)
  )
(add-hook 'c-mode-hook 'mde-c-mode-hook)


;; dtrt-indent is the successor to guess-offset.
;; Homepage:  https://github.com/jscheid/dtrt-indent
;; To debug dtrt-indent, execute this in the buffer with the bad guess:
;;   (dtrt-indent-diagnosis)

(if (not (locate-library "dtrt-indent"))
    (message "Could not find dtrt-indent")
  (progn
    (require 'dtrt-indent)
    (dtrt-indent-mode 1)
    (setq dtrt-indent-min-indent-superiority 50.0) ; default 100.0
    (setq dtrt-indent-max-merge-deviation 30.0) ; default 20.0; 40.0 didn't work for me
    ;; (setq dtrt-indent-min-indent-superiority-double 40.0) ; default 100.0
    ))

(defun mde-yaml-mode-hook ()
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  (make-local-variable 'inleft-string)
  (setq inleft-string "# ")
  (add-hook 'after-save-hook 'run-circleci-config-validate nil 'local))


(if (not (locate-library "yaml-mode"))
    (message "Could not find yaml-mode")
  (progn
    (require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
    (add-hook 'yaml-mode-hook 'mde-yaml-mode-hook)))


(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun run-circleci-config-validate ()
  "Run `circleci config validate` if this file is .circleci/config.yml."
  (if (string-suffix-p "/.circleci/config.yml" (buffer-file-name))
      (progn
	(if (get-buffer "*circleci-config-validate*")
	    (with-current-buffer (get-buffer "*circleci-config-validate*")
	      (erase-buffer)))
	(let* ((default-directory (parent-directory default-directory))
	       (process-status (call-process "circleci" nil "*circleci-config-validate*" nil
					     "config" "validate")))
	  (if (not (equal process-status 0))
	      (progn
		(pop-to-buffer "*circleci-config-validate*")
		(error "Invalid .circleci/config.yml file")))))))


;; ;; I was afraid this would blow away prefix argument info; maybe it doesn't.
;; (defadvice c-electric-slash (around not-at-left-margin activate)
;;   "Don't indent if at left column."
;;   (interactive "P")
;;   (if (and (eq ?/ (char-after (- (point) 1)))
;;            (eq ?\n (char-after (- (point) 2))))
;;       (self-insert-command (prefix-numeric-value arg))
;;     ad-do-it))

(defun mde-c++-mode-hook ()
  "Michael Ernst's C++ mode hook."
  (mde-c-mode-hook)
  (make-local-variable 'inleft-string)
  (setq inleft-string "// ")
  (c++-set-compile-command))
(add-hook 'c++-mode-hook 'mde-c++-mode-hook)

;; ;; Infinite loop in c-fill-paragraph if fill-prefix is empty string.
;; (defadvice c-fill-paragraph (before avoid-empty-string-fill-prefix activate)
;;   (if (equal fill-prefix "")
;;       (setq fill-prefix nil)))


;; This is generally useful, but not currently used.

;; Arguments to the c-*-of-* functions seem to be required by Emacs 20.
(defun c-name-of-enclosing-function ()
  "Return the name of the function containing point, or nil
if point is not in a function."
  (save-excursion
    (beginning-of-line)
    (let ((orig-point (point)))
      (c-end-of-defun 1)
      (c-beginning-of-defun 1)
      (if (= (point) (point-min))
          nil
        (let ((bod (point)))            ; beginning of defun
          (c-beginning-of-statement 1)
          (if (< orig-point (point))
              nil
            (if (re-search-forward "\\b\\(\\w+\\)\\s-*(" bod t)
                (match-string 1)
              (progn
                (message "c-name-of-enclosing-function got confused")
                nil))))))))
;; Here is some test code.
(defun message-c-name-of-enclosing-function ()
  (if (eq major-mode 'c-mode)
      (message "%s" (c-name-of-enclosing-function))))
;; (add-hook 'post-command-hook 'message-c-name-of-enclosing-function)
;; ;; To undo:
;; (remove-hook 'post-command-hook 'message-c-name-of-enclosing-function)


;; Also works for Java.
(autoload 'google-set-c-style "google-c-style")
(autoload 'google-make-newline-indent "google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java
;;;

;; (require 'lsp-java nil 'noerror)
;; (add-hook 'java-mode-hook #'lsp)

(require 'use-package)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

;; TODO: Why is all this at the top level?
(use-package flycheck)
;; (use-package yasnippet :config (yas-global-mode))
;; (use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))
;; (use-package lsp-ui)
(use-package which-key :config (which-key-mode))
;; (use-package lsp-treemacs)
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (use-package dap-java :ensure nil)

;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;; (with-eval-after-load "lsp-mode"
;;   (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map))


(use-package apheleia)          ; auto-format code on save
;; For debugging:
;; (setq apheleia-log-only-errors nil)
;; (setq apheleia-log-debug-info t)



;; TODO: Add autoloads for functions that are defined in java-style.el.
(autoload 'improve-javadoc-style "java-style"
  "Improve Javadoc style in Java files, in the current TAGS table."
  t)

;; Appears to mean go to beginning of class rather than method.
(defvar c-beginning-of-defun-prefer-second nil)


(setq auto-mode-alist
      (append '(("\\.javax\\'" . java-mode) ; ConstJava uses ".javax" extension
                ("\\.jpp\\'" . java-mode) ; for preprocessed files; can't specify ".java.jpp"
		("\\.astub\\'" . java-mode) ; Checker Framework annotated libraries
		("\\.java-ORIG\\'" . java-mode)
		("\\.java-SAVE\\'" . java-mode)
		)
              auto-mode-alist))
(defun java-beginning-of-defun (&optional arg)
  "See `c-beginning-of-defun'.
With prefix arg, goes to beginning of class; otherwise to beginning of method."
  (interactive "P")
  (let ((c-beginning-of-defun-prefer-second t))
    (if (equal arg '(4))
        (setq arg 1
              c-beginning-of-defun-prefer-second nil))
    (c-beginning-of-defun (prefix-numeric-value arg))))

(defun java-end-of-defun (&optional arg)
  "See `c-end-of-defun'.
With prefix arg, goes to end of class; otherwise to end of method."
  (interactive "P")
  (let ((c-beginning-of-defun-prefer-second t))
    (if (equal arg '(4))
        (setq arg 1
              c-beginning-of-defun-prefer-second nil))
    (c-end-of-defun (prefix-numeric-value arg))))

;;; Experimentally commenting out, 2025-04-07.
;; ;; This is my enhancement
;; (with-eval-after-load "cc-cmds"
;;   (defun c-beginning-of-defun (&optional arg)
;;     "Move backward to the beginning of a defun.
;;   With argument, do it that many times.  Negative arg -N
;;   means move forward to Nth following beginning of defun.
;;   Returns t unless search stops due to beginning or end of buffer.
;; 
;;   Unlike the built-in `beginning-of-defun' this tries to be smarter
;;   about finding the char with open-parenthesis syntax that starts the
;;   defun.
;; 
;;   This also respects `c-beginning-of-defun-prefer-second'."
;;     (interactive "p")
;;     (unless arg (setq arg 1))
;;     (if (< arg 0)
;;         (c-end-of-defun (- arg))
;;       (while (> arg 0)
;;         (let ((state (nreverse (c-parse-state)))
;;               prevbod bod
;;               prevbod2 bod2)
;;           (while (and state (not bod))
;;             (setq bod (car state)
;;                   state (cdr state))
;;             (if (consp bod)
;;                 (setq prevbod (car bod)
;;                       bod nil)))
;;           (if c-beginning-of-defun-prefer-second
;;               (while (and state (not bod2))
;;                 (setq bod2 (car state)
;;                       state (cdr state))
;;                 (if (consp bod2)
;;                     (setq prevbod2 (car bod2)
;;                           bod2 nil))))
;;           (cond
;;            (bod2 (goto-char bod2))
;;            (prevbod2 (goto-char prevbod2))
;;            (bod (goto-char bod))
;;            (prevbod (goto-char prevbod))
;;            (t (goto-char (point-min))
;;               (setq arg 0)))
;;           (setq arg (1- arg))))
;;       (c-keep-region-active)
;;       (= arg 0))))


(make-variable-buffer-local 'before-save-hook)

(defun mde-java-mode-hook ()
  "Michael Ernst's Java mode hook."
  (eval-when-compile (require 'cc-mode)) ; defines java-mode
  (save-match-data
    (mde-c-mode-hook)
    (make-local-variable 'inleft-string)
    (setq inleft-string "// ")
    (setq paragraph-start (concat " */* *<p>\\|" paragraph-separate))
    (setq paragraph-separate (concat ".*<p>\\|" paragraph-separate))
    (if (boundp 'java-mode-map)
        (define-key java-mode-map "\C-hf" 'javadoc-lookup))
    (if (boundp 'java-ts-mode-map)
        (define-key java-ts-mode-map "\C-hf" 'javadoc-lookup))
    (add-hook 'before-save-hook 'change-returns-to-return)
    (add-hook 'before-save-hook 'sort-checkerframework-code)
    (if (string-match "/\\(checker-framework\\|plume-lib\\|randoop\\)/"
		      (directory-file-name default-directory))
	(progn
	  (add-hook 'before-save-hook 'delete-trailing-whitespace)
	  ))
    (java-set-compile-command)

    ;; (subword-mode t)     ; handle CamelCase
    ;; (hs-minor-mode 1) ; hide/show code and comment blocks

    (make-local-variable 'write-contents-functions)
    ;; Behavior for specific file names.
    (if (enable-java-formatting-p)
        (apheleia-mode +1))
    (let ((file-name (buffer-file-name (current-buffer))))
      (if file-name
	  (progn
	    (if (and
		 (not (string-match "\\.jpp$" file-name))
		 (not (string-match "/AnnotatedTypeFactory.java$" file-name)))
		;; TODO: This is not necessary for projects that use spotless or run-google-java-format.py
		(add-hook 'write-contents-functions 'check-parens-ignore-on-retry))
	    (if (not (string-match "share/classes" file-name))
		(add-hook 'write-contents-functions 'check-for-string-equality))
	    (if (string-match "/checker-framework\\|/daikon\\|/plume-lib\\|/randoop\\|/mernst/bin/src/" file-name)
		(progn
		  ;; Google Java style sets fill column to 100
		  (setq fill-column 100)
		  ))
	    )))))

(add-hook 'java-mode-hook 'mde-java-mode-hook)
(add-hook 'java-ts-mode-hook 'mde-java-mode-hook)


(defun change-returns-to-return ()
  "Change Javadoc occurrences of @returns to @return."
  (save-excursion
    (goto-char (point-min))
    (replace-string-noninteractive "^\\( *\\(\\* *\\)\\)@returns " "\\1@return ")))

(defun sort-checkerframework-code ()
  "Improve style by sorting Checker Framework code."
  (sort-checkerframework-imports)
  (sort-checkerframework-polyannotations))

(defun sort-checkerframework-imports ()
  "Sort adjacent instances of \"import org.checkerframework...\"."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\nimport org.checkerframework" nil t)
	(let ((begin (1+ (match-beginning 0)))
	      (previous-line "dummy")
	      (unsorted nil))
	  (goto-char begin)
	  (while (looking-at "import org.checkerframework")
	    (let ((current-line (buffer-substring-no-properties
				 (line-beginning-position)
				 (line-end-position))))
	      (if (and (not unsorted) (not (import< previous-line current-line)))
		  (setq unsorted t))
	      (setq previous-line current-line))
	    (forward-line 1))
	  (if unsorted
	      (save-restriction
		(narrow-to-region begin (point))
		(goto-char (point-min))
		(let ;; To make `end-of-line' and etc. to ignore fields.
		    ((inhibit-field-text-motion t))
		  (sort-subr nil 'forward-line 'end-of-line nil nil 'import<)
		  (delete-duplicate-lines (point-min) (point-max)))))))))

(defun sort-checkerframework-polyannotations ()
  "Sort adjacent instances of \"@PolyXXX\"."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "@Poly[A-Za-z]+\\([ \t]+@Poly[A-Za-z]+\\)+" nil t)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(if (polyannos-are-unsorted beg end)
	    (let ((polyannos (buffer-substring-no-properties beg end)))
	      (delete-region beg end)
	      (insert (sorted-polyannotations polyannos))))))))

;; This function exists for efficiency, to reduce the creation of temporary strings.
(defun polyannos-are-unsorted (beg end)
  "Returns t if the @PolyXXX annotations in the given range are sorted."
  (save-excursion
    (save-match-data
      (goto-char beg)
      (let ((unsorted nil)
	    (prevpoly "@Poly"))
	(while (and (not unsorted) (re-search-forward "@Poly[A-Za-z]+" end t))
	  (let ((thispoly (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
	    (if (string< prevpoly thispoly)
		(setq prevpoly thispoly)
	      (setq unsorted t))))
	unsorted))))

(defun sorted-polyannotations (polyannos)
  "Returns a sorted version of the given space-separated @PolyXXX annotations."
  (save-match-data
    (string-join
     (sort (split-string polyannos))
     " ")))
;; (string= "@PolyA @PolyB @PolyC" (sorted-polyannotations "@PolyA @PolyB @PolyC"))
;; (string= "@PolyA @PolyB @PolyC" (sorted-polyannotations "@PolyA	@PolyB  @PolyC"))
;; (string= "@PolyA @PolyB @PolyC" (sorted-polyannotations "@PolyB @PolyA @PolyC"))
;; (string= "@PolyA @PolyB @PolyC" (sorted-polyannotations "@PolyC @PolyB @PolyA"))


(defun import< (s1 s2)
  "Compares two import strings, in the order used by google-java-format."
  (cond ((and (numberp s1) (numberp s2))
	 (< s1 s2))
	((and (consp s1) (consp s2))
	 (< (compare-buffer-substrings nil (car s1) (1- (cdr s1)) nil (car s2) (1- (cdr s2))) 0))
	(t
	 (string< (replace-regexp-in-string ";$" "" s1) (replace-regexp-in-string ";$" "" s2)))))
(if nil
    (progn
      (cl-assert (import< "dummy" "import foo.bar.Baz;"))
      (cl-assert (import< "import foo.bar.baz;" "import foo.bar.baz.quux;"))
      (cl-assert (not (import< "import foo.bar.baz.quux;" "import foo.bar.baz;")))
      (cl-assert (import< "dummy" "import foo.bar.baz;"))))

(defun set-tab-stop-list-width (n)
  "Set tab width in current buffer to N.
Interactively, it's probably better to just set variable `tab-width'."
  (interactive "P")
  (if (not (numberp n))
      (error "Supply explicit numeric prefix argument to `set-tab-stop-list-width'"))
  (setq tab-stop-list '())
  ;; was 120, which was not big enough
  (let ((i (* (/ 500 n) n)))
    (while (> i 0)
      (setq tab-stop-list (cons i tab-stop-list)
            i (- i n)))))

(defun set-tab-width (n)
  (interactive "P")
  (while (or (not (numberp n)) (zerop n))
    (setq n (string-to-number (read-string "Set tab width: "))))
  (setq tab-width n))

(defun java-equals-method-template ()
  "Insert template for a Java `equals' method."
  (interactive)
  (if (not (and buffer-file-name
                (string-match "/\\([^/]+\\)\\.java$" buffer-file-name)))
      (error "Not editing a Java file"))
  (let ((class-name (match-string 1 buffer-file-name)))
    (if (not (bolp))
        (insert "\n"))
    (insert "  public boolean equals(Object obj)
    {
      if (!(other instanceof " class-name ")) {
        return false;
      }
      " class-name " o = (" class-name ") other;
      ")))


(defun check-for-string-equality ()
  "Complain if Java strings are being compared for pointer equality.
This is disabled on lines with a comment containing the string \"interned\"."
  (let ((error-point nil))
    (save-excursion
      (goto-char (point-min))
      ;; Look for `=="' or `"==' (likewise for `!=')
      ;; The "[^+]" is to avoid complaining about:   foo + " != " + bar
      (while
          ;; (re-search-forward "[^=][=!]= *\".[^+]\\|[^+].\" *[=!]=[^=]" nil t)
          (condition-case err
              (re-search-forward (concat "[^=\n][=!]= *\"\\(.?\"\\|.[^+\n].*\"\\)"
                                         "\\|"
                                         "\\(\".?\\|\".*[^+\n].\\)\" *[=!]=[^=\n].*\"")
                                 nil t)
            (error
             (let ((error-message (cl-second err)))
               (if (equal error-message "Stack overflow in regexp matcher")
                   nil
                 (throw 'error error-message)))))
        (if (let ((bol-point (save-excursion (beginning-of-line) (point))))
	      (not (or (looking-at ".*//.*interned")
		       ;; line ends with string ending with "=="
		       (and (looking-back "=?= *\"" bol-point)
			    (looking-at ";\n"))
		       ;; if already in comment, suppress warning
		       (looking-back "/[/*].*" bol-point)
		       (looking-back "^[ \t]*\\*.*" bol-point) ; Javadoc comment
		       ;; entire string appears to be "==" or "!=" (as an arg)
		       (looking-back "\(\"[=!]=\"\).*" bol-point)
		       )))
            (progn
              (redisplay)
              (if (not (y-or-n-p "Strings being compared with pointer equality; save anyway? "))
                  (progn
                    (message "\"// interned\" comment suppresses warning")
                    (setq error-point (point))
                    (goto-char (point-max)))
                (message "\"// interned\" comment suppresses warning"))))))
    (if error-point
        (progn
          (goto-char error-point)
          (error "Strings being compared with pointer equality"))))
  ;; return nil so this can be used as a write-{file,contents}-hook
  nil)

(autoload 'bdiff-revert-buffer-maybe "bdiff")

;; I am trying a different approach for auto-formatting on save, apheleia-mode.
;; (defun update-java-mode-hook-for-gjf ()
;;   (add-hook 'after-save-hook 'run-google-java-format nil 'local))
;; (add-hook 'java-mode-hook 'update-java-mode-hook-for-gjf)

;; Also see enable-python-formatting-p
(defun enable-java-formatting-p ()
  "Returns true if the file matches a hard-coded list of directories."
  (let ((filename (buffer-file-name)))

    (cond

     ;; Not visiting a file.
     ((not filename)
      nil)

     ;; No formatting

     ;; The buffer contains Git merge conflict markers
     ((save-excursion
        (save-match-data
	  (goto-char (point-min))
	  (search-forward "\n<<<<<<<" nil t)))
      nil)
     ;; This file is formatted differently by Spotless and run-google-java-format.py
     ;; ((string-match-p "CheckerFrameworkWPIPerDirectoryTest.java" filename)
     ;;  nil)
     ;; Files copied from elsewhere that should not be reformatted
     ((or (string-suffix-p "WeakIdentityHashMap.java" filename)
          (string-suffix-p "WeakHasherMap.java" filename)
	  (string-suffix-p "diff_match_patch.java" filename))
      nil)

     ;; Perform formatting

     ((or

       ;; Checker Framework
       (and (string-match-p "/checker-framework" filename)
	    (not (string-match-p "/checker-framework-demo" filename))
	    (not (string-match-p "/checker-framework-inference" filename))
	    (not (string-match-p "/checker/jdk/" filename))
	    (not (string-match-p "\\.astub$" filename))
	    (not (string-match-p "/dataflow/manual/examples/" filename))
	    (not (string-match-p "/nullness-jspecify-samples/" filename))
	    )
       (and (or (string-match-p "/object-construction-checker" filename)
	        (string-match-p "/no-literal-checker" filename))
	    (not (string-match-p "/tests/\\|/test-lib-java/" filename)))

       ;; Randoop
       (and (string-match-p "/\\(randoop\\)" filename)
	    (not (string-match-p "CloneVisitor\\.java$" filename))
	    (not (string-match-p "/src/testinput/" filename)))

       ;; Daikon
       (and (string-match-p "/daikon" filename)
	    (not (string-match-p "/daikon-gradle-plugin" filename))
	    (not (string-match-p "\\.jpp$" filename))
	    (not (string-match-p "nonnull-interned-demo" filename))
            (not (string-match-p "/tests/daikon-tests/" filename))
            (not (string-match-p "/tests/sources/" filename))
            (not (string-match-p "/java/jtb/" filename))
            )

       ;; Toradocu
       (and (string-match-p "/toradocu" filename)
	    (not (string-match-p "/src/test/resources/" filename)))

       ;; Plume-lib
       (and
        (or
         (and (string-match-p "/plume-lib" filename)
	      (not (string-match-p "src/test/resources" filename)))
         (string-match-p "/org/plumelib/" filename))
        (not (string-match-p "/wpi-many-tests" filename))
        (not (string-match-p "fork-t-rasmud" filename)))
       
       ;; Merging
       (string-match-p "/AST-Merging-Evaluation[^/]*/src/main/java/" filename)
       (string-match-p "/ast-merge-driver[^/]*/src/main/java/" filename)

       ;; Teaching
       (and (string-match-p "/class/331/" filename)
            ;; Not student directories
	    (not (string-match-p "/class/331/cse331-[0-9][0-9]\\(au\\|sp\\|su\\|wi\\)-" filename)))
       )
      t)

     ;; No formatting for all other projects
     (t
      nil))))

(with-eval-after-load "apheleia"
  (setf (alist-get 'python-mode apheleia-mode-alist)
        'ruff)
  ;; out of the box, apheleia uses the script name "google-java-format" which doesn't exist
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("run-google-java-format.py" inplace)))

(defun enable-python-formatting-p ()
  "Returns true if the file matches a hard-coded list of directories."
  (let* ((filename (buffer-file-name))
         (dirpath (and filename (file-name-directory filename)))
         (dir-simple-name (and dirpath (file-name-nondirectory dirpath))))

    (cond

     ;; Not visiting a file.
     ((not filename)
      nil)

     ;; No formatting

     ;; ...

     ;; Perform formatting

     ((or (string-match-p "/untangling-tools-benchmark" filename)
	  (and (string-match-p "/AST-Merging-Evaluation" filename)
	       (not (string-match-p "git-hires-merge" filename)))
	  (string-match-p "/git-scripts" filename)
	  (string-match-p "/plume-scripts" filename)
          (string-match-p "/prompt-mutation-experiments" filename)
          (string-match-p "/rust_verification" filename)
          (and (string-match-p "/grt-testing" filename)
               (or (starts-with "grt-testing" dir-simple-name)
                   (equal "subject-programs" dir-simple-name)))
          
          )
      t)

     ;; No formatting for all other projects
     (t
      nil))))

(custom-set-variables
 '(jdee-server-dir (expand-file-name "~/.emacs.d/jdee-server"))
 )


;; (defadvice jdb (after set-gud-jdb-sourcepath activate)
;;   "Hard-code some directories whose bin/jar is on my classpath."
;;   (setq gud-jdb-sourcepath
;; 	(append
;; 	 gud-jdb-sourcepath
;; 	 (mapcar #'expand-file-name
;; 		 '(
;; 		   "~/research/types/annotation-tools/annotation-file-utilities/src"
;; 		   "~/research/types/annotation-tools/scene-lib/src"
;; 		   "~/research/types/annotation-tools/scene-lib/src-devel"
;; 		   "~/research/types/annotation-tools/asmx/src"
;; 		   "~/research/types/checker-framework/checker/src"
;; 		   "~/research/types/jsr308-langtools/src/share/classes"
;; 		   "~/java/java-6-src"
;; 		   "~/java/junit-4.5-src"
;; 		   "~/java/iCal4j/source"
;; 		   )))))



(add-hook 'groovy-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)
	      ;; TODO: maybe set indent offset depending on buffer-file-name
	      (setq groovy-indent-offset 2)
	      (make-local-variable 'inleft-string)
	      (setq inleft-string "// ")
	      ))


;;; This disables highlighting in conflicted files (which might be done by SMerge mode?).
;;; Investigate and fix before re-enabling this.
;; ;; Highlight multi-line strings ("text blocks")
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (mmm-add-classes
;;  '((java-text-block
;;     :submode fundamental-mode
;;     :front ".+\"\"\"$"
;;     :back ".*\"\"\".*"
;;     :face mmm-code-submode-face
;;     )))
;; (mmm-add-mode-ext-class 'java-mode "\\.java$" 'java-text-block)


(defun compilation-fix-java-error-prone ()
  "Apply the suggestions made by Error Prone in the *compilation* buffer.
Returns t if any change was made, nil otherwise."
  (interactive)
  (let ((result nil))
    (with-current-buffer "*compilation*"
      (goto-char (point-min))
      (while (re-search-forward "^  Did you mean '\\(.*\\)'\\?$" nil t)
        (let ((replacement (match-string 1)))
          (save-excursion
            (re-search-backward "^[^ :]+:[0-9]+:")
            (compile-goto-error)
            (beginning-of-line)
            (skip-chars-forward " \t")
            (delete-region (point) (progn (end-of-line) (point)))
            (insert replacement)
            (save-buffer)))
        (setq result t)))
    result))

(defun fix-compilation ()
  "Fix problems found in the *compilation* buffer."
  (interactive)
  (let ((result nil))
    (if (compilation-fix-java-imports)
        (setq result t))
    (if (compilation-fix-java-error-prone)
        (setq result t))
    (if (compilation-fix-python-ruff)
        (setq result t))
    (if result
        (with-current-buffer "*compilation*"
          (recompile)
          (fix-compilation)))))

;; Don't spell-check symbols in Javadoc
(add-to-list 'ispell-skip-region-alist '("\\*[ \t]+@param[ \t]+[a-zA-Z0-9_]+"))
(add-to-list 'ispell-skip-region-alist '("{@link [^{}]*}"))
(add-to-list 'ispell-skip-region-alist '("{@code [^{}]*}"))
(add-to-list 'ispell-skip-region-alist '("\\*[ \t]+@see.*"))
(add-to-list 'ispell-skip-region-alist '("</?ul>"))
(add-to-list 'ispell-skip-region-alist '("</?ol>"))
(add-to-list 'ispell-skip-region-alist '("</?li>"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Makefiles and shell scripts
;;;

(defun mde-makefile-mode-hook ()
  "Michael Ernst's Makefile mode hook."
  (local-set-key "\C-c\C-c" 'compile)
  (setq inleft-string "# "))
(add-hook 'makefile-mode-hook 'mde-makefile-mode-hook)

(defun mde-xml-mode-hook ()
  "Michael Ernst's XML mode hook."
  (if (and (buffer-file-name)
           (string-equal "build.xml" (file-name-nondirectory (buffer-file-name))))
      (local-set-key "\C-c\C-c" 'compile)))
(add-hook 'sgml-mode-hook 'mde-xml-mode-hook)

(defun enable-shell-script-formatting-p ()
  "Returns true if the file matches a hard-coded list of directories."
  (let ((filename (buffer-file-name)))

    (cond

     ((not filename)
      nil)



     ;; No formatting

     ;; ...

     ;; Perform formatting

     ;; Randoop
     ((and (string-match-p "/\\(randoop\\)" filename)
	   (not (string-match-p "CloneVisitor\\.java$" filename))
	   (not (string-match-p "/src/testinput/" filename)))
      t)



     ;; No formatting for all other projects
     (t
      nil))))

(eval-when-compile (require 'sh-script))
(defun mde-sh-mode-hook ()
  "Michael Ernst's shell mode hook."
  (setq inleft-string "# ")
  (setq indent-tabs-mode nil)
  (if (enable-shell-formatting-p)
      (progn
        ;; TODO: change for certain files (but I use 2 for my own projects).
        (setq sh-basic-offset 2)
        (apheleia-mode +1)))
  (add-hook 'after-save-hook 'shell-script-validate nil 'local)
  (if (enable-shell-script-formatting-p)
      (apheleia-mode +1))
  )
(add-hook 'sh-mode-hook 'mde-sh-mode-hook)

;; Use apheleia-mode instead.
;; (add-hook 'sh-mode-hook 'shfmt-on-save-mode)

(defun shell-script-validate ()
  "Validate this shell script."
  (let* ((bfilename (buffer-file-name))
         (filename (and bfilename (file-name-nondirectory bfilename))))
    (if filename
        (progn
          (run-command nil (symbol-name sh-shell) "-n" filename)
          (if (equal sh-shell 'sh)
              (run-command nil "checkbashisms" filename))
          ;; TODO: Add -P that includes both "." and the top-level of the git repository (if in a git repository).
          (run-command nil "shellcheck" "-x" "--format=gcc" "-P" "SCRIPTDIR" filename)
          ))))

(defun buffer-validate (validator &rest args)
  "Runs a validation validation on the current buffer's file.
Use this in an after-save-hook.
VALIDATOR is a program to run.
ARGS are args to pass it.  Buffer file name is provided as last arg."
  (if (get-buffer "*validate*")
      (kill-buffer "*validate*"))
  (let ((process-status (apply #'call-process validator nil "*validate*" nil
			       (append args (list (buffer-file-name))))))
    (if (not (equal process-status 0))
	(progn
	  (pop-to-buffer "*validate*")
	  (error "Invalid shell script")))))

(defun enable-shell-formatting-p ()
  "Returns true if the file matches a hard-coded list of directories."
  (let ((filename (buffer-file-name)))

    (cond

     ;; Not visiting a file.
     ((not filename)
      nil)

     ;; No formatting

     ;; ...

     ;; Perform formatting

     ((or (string-match-p "/untangling-tools-benchmark" filename)
	  (and (string-match-p "/AST-Merging-Evaluation" filename)
	       (not (string-match-p "git-hires-merge" filename)))
	  (string-match-p "/git-scripts" filename)
          (string-match-p "/grt-testing" filename)
	  (string-match-p "/plume-scripts" filename)
	  (string-match-p "/html-tools" filename)
          (string-match-p "/prompt-mutation-experiments" filename)
          )
      t)

     ;; No formatting for all other projects
     (t
      nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl
;;;

(defvar perl-major-modes '(perl cperl))

(eval-when-compile (require 'perl-mode))

(defun mde-perl-mode-hook ()
  "Michael Ernst's Perl mode hook."
  (swap-return-and-linefeed)
  (setq inleft-string "# ")
  ;; GNU indentation style for Perl
  (setq perl-indent-level                2
        perl-continued-statement-offset  2
        perl-continued-brace-offset      0
        perl-brace-offset                0
        perl-brace-imaginary-offset      0
        perl-label-offset               -2)
  ;; Why is this necessary?
  (setq perl-brace-offset -2)
  (make-local-variable 'compile-command)
  (if buffer-file-name
      (if (looking-at ".* -[^ ]T")
          ;; If shebang line has -T, command line must also
          (setq compile-command (concat "perl -cT " buffer-file-name))
        (setq compile-command (concat "perl -c " buffer-file-name))))
  (local-set-key "\C-c\C-c" 'compile)
  ;; (local-set-key "\C-hf" 'cperl-info-on-command)
  (local-set-key "\C-hf" 'perldoc)
  (make-local-variable 'write-contents-functions)
  ;; (add-hook 'write-contents-functions 'maybe-delete-trailing-whitespace)
  (add-hook 'write-contents-functions 'shadowed-variables-perl-write-hook)
  (add-hook 'write-contents-functions 'perl-backup-file-check)
  (add-hook 'write-contents-functions 'check-parens-ignore-on-retry)
  )
;; In Emacs 19.34, this doesn't appear to work.  But once I do (run-hooks
;; 'perl-mode-hook) by hand, it appears to take for future Perl buffers.
(add-hook 'perl-mode-hook 'mde-perl-mode-hook)

(autoload 'perldoc "perldoc" "Run perldoc on the given STRING." t)
;; (defadvice perldoc (before supply-default activate)
;;   "Provide a default of the thing at point."
;;   (interactive
;;    (list (let* ((default (or (thing-at-point 'word)
;;                              (thing-at-point 'filename)))
;;                 (default-prompt (and default (concat " (default " default ")"))))
;;            (completing-read (concat "Perl function or module" default-prompt ": ")
;;                             (perldoc-functions-alist) nil nil
;;                             nil nil default)))))
;; (defadvice perldoc-start-process (after bind-perldoc activate)
;;   "Set `C-h f' key to run `perldoc'."
;;   (local-set-key "\C-hf" 'perldoc))
;; 
;; (defadvice indent-perl-exp (after unspace-brace-hash activate)
;;   "Insert no space before a hash (#) immediately following an open brace."
;;   (let* ((eol (save-excursion (end-of-line) (point)))
;; 	 (end (save-excursion
;; 		(while (<= (point) eol)
;; 		  (forward-sexp 1))
;; 		(point))))
;;     (save-excursion
;;       (while (re-search-forward "[\{\(]\\(\\s-+\\)#" end t)
;; 	;; replace first submatch by a single space
;; 	(replace-match " " t t nil 1)))))

(defun perl-in-comment ()
  "Return non-nil if in a Perl comment."
  (save-excursion
    (let ((here (point)))
      (beginning-of-line 1)
      (save-match-data
        (re-search-forward "\\(^\\|[^\\\$]\\)#" here t)))))


(defun perl-backup-file-check ()
  "Check that Perl in-place editing isn't done without a backup."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at ".*perl.*-[^ \n]*i[^.]")
        (if (not (y-or-n-p "Perl script does in-place editing with no backup file; save anyway? "))
            (error "Perl script does in-place editing with no backup file")))))


(defun shadowed-variables-perl-write-hook ()
  "When in Perl mode, check for shadowed variables before writing file."
  (if (member major-mode '(perl-mode cperl-mode))
      (let ((result (shadowed-variables-perl t)))
        (while result
          (if (not (y-or-n-p (format "%s; save anyway? " (car result))))
              (error "%s" (car result))
            ;; put it in the *Messages* buffer for later reference
            (message "%s" (car result)))
          (setq result (cdr result))))))

(defun shadowed-variables-perl (&optional no-err)
  "Look for Perl variables with nested \(\"my\"\) scope.
If NO-ERR is non-nil return a list of error messages;
otherwise, raise an error after the first problem is encountered."
  (interactive)
  (let ((opoint (point))
        (result '()))
    (goto-char (point-min))
    (search-forward "\n=cut\n" nil t)
    (while (re-search-forward "\\b\\(for\\(each\\)?\\s-*\\(\(\\s-*\\)?\\)?\\bmy\\b\\|use\\s-+vars\\s-+\\('\\|qw\(\\)" nil t)
      (if (perl-in-comment)
          (forward-line 1)
        (let* ((is-my (equal "my" (buffer-substring (- (point) 2) (point))))
               (is-for (equal "for" (buffer-substring (match-beginning 0)
                                                      (+ (match-beginning 0) 3))))
               (vars-string (buffer-substring
                             (point)
                             (progn
                               ;; "foreach my $arg (@args) doesn't declare "@args"
                               (save-match-data
                                 ;; The re-search-forward forms can fail if
                                 ;; there is a partial statement near file end.
                                 (or (if (looking-at "\\s-*\(")
                                         (re-search-forward "\)" nil t)
                                       (re-search-forward "[;=(]" nil t))
                                     (error "Partial statement"))
                                 (point)))))
               (decl-end (point))
               (scope-end (let ((end (1- decl-end)))
                            (if is-for
                                (let ((for-open-paren-pos
                                       (cond ((= (char-after end) ?\()
                                              end)
                                             ((let ((str-3 (match-string 3)))
                                                (and str-3
                                                     (= ?\( (elt str-3 0))))
                                              (1- (match-end 3))))))
                                  (if for-open-paren-pos
                                      (setq end
                                            (save-excursion
                                              (goto-char for-open-paren-pos)
                                              (ignore-errors (forward-sexp 2))
                                              (point))))))
                            (while (<= end decl-end)
                              (setq end
                                    (if (re-search-backward "{" nil t)
                                        (if (and (not (bobp))
                                                 (or (= ?\\ (char-after (1- (point))))
                                                     (= ?$ (char-after (1- (point))))
                                                     (perl-in-comment)))
                                            end ; don't change value of end
                                          (condition-case nil
                                              (save-excursion
                                                (forward-sexp 1)
                                                (point))
                                            (error (point-max))))
                                      (point-max))))
                            end)))
          (goto-char decl-end)
          ;; we're now at the end of this declaration
          ;; not \\w+\\b because '\w' doesn't include _
          (while (string-match "[$@%][a-zA-Z_][a-zA-Z_0-9]*" vars-string)
            (let* ((this-var (match-string 0 vars-string))
                   ;; look for "${i}" as well as "$i"
                   (this-var-regexp (concat
                                     (regexp-quote (substring this-var 0 1))
                                     "{?"
                                     (regexp-quote (substring this-var 1))))
                   (this-var-regexp (concat this-var-regexp
                                            "\\($\\|[^a-zA-Z0-9_]\\)")))
              (setq vars-string (substring vars-string (match-end 0)))
              (goto-char decl-end)
              ;; These tests aren't quite right; if they find something in
              ;; a comment, they should iterate and look again instead of
              ;; just giving up, as they do now.
              (let ((msg
                     (if (and is-my
                              (save-excursion
                                (and
                                 (not (re-search-forward this-var-regexp scope-end t))
                                 (not (and (= ?\@ (elt this-var 0))
                                           (or
                                            (re-search-forward
                                             (concat "\\$" (substring this-var 1) "\\[")
                                             nil t)
                                            (re-search-forward
                                             (concat "\\$#" (substring this-var 1) "\\b")
                                             nil t))))
                                 (not (and (= ?\% (elt this-var 0))
                                           (re-search-forward
                                            (concat "\\$" (substring this-var 1) "{")
                                            nil t))))))
                         (format "Local variable %s at line %d is never used"
                                 this-var (count-lines (point-min) decl-end))
                       (if (let ((case-fold-search nil))
                             (and (re-search-forward
                                   ;; "{" is for body of "for my ..."
                                   ;; Problem: "# My hack\n}elsif ($tag eq "p")"
                                   ;; looks like a definition of $tag to this.
                                   (concat "\\bmy\\b\\s-*\\(\([^;={)]*\\|[^;={]*\\)"
                                           this-var-regexp)
                                   scope-end t)
                                  (not (perl-in-comment))
                                  ;; Make sure the variable doesn't appear in the ()
                                  ;; section of  foreach my $var (...)
                                  (not (string-match (concat "^my\\s-*[$@%][^;=]*\([^;=()]*"
                                                             this-var-regexp)
                                                     (match-string 0)))))
                           (format "Redefinition of variable %s at lines %d and %d"
                                   this-var
                                   (count-lines (point-min) decl-end)
                                   (count-lines (point-min) (point)))))))
                (if msg
                    (if no-err
                        (setq result (cons msg result))
                      (error "%s" msg)))))))))
    (goto-char opoint)
    (if (called-interactively-p 'interactive)
        (message "No shadowed variables.")
      (nreverse result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python
;;;

;; There are two modes for editing Python code in Emacs:
;;  * python.el is from the Emacs community
;;    Its varables/routines start with "python-".
;;  * python-mode.el is from the Python community
;;    Its varables/routines start with "py-".
;; As of Emacs 23 (and even more so as of Emacs 24), python.el is better:
;; it comes with Emacs, has a few extra features, and works out of the box.


;; Avoid errors if various Python support is not available.
;; TODO: reinstate
;; (eval-when-compile (if (locate-library "python-mode") (require 'python-mode)))

(autoload 'python-shell "python" "Start an interactive Python interpreter" t)

(defun mde-python-mode-hook ()
  "Michael Ernst's Python mode hook."
  (swap-return-and-linefeed)
  (setq inleft-string "# ")
  (setq comment-indent-function 'python-comment-indent)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter (mde-page-delimiter ?#))
  ;; Not needed if my patch is accepted.
  ;; (setq comint-prompt-regexp "^\\(>>>\\|(pdb)\\) ")
  ;; This variable only ever honors comments starting with exactly one #,
  ;; never those starting with "##".  I hate that behavior, so I hacked
  ;; my version of python-mode.el.
  (setq python-honour-comment-indentation t)
  (define-key python-mode-map "\C-hf" 'pylookup-lookup)
  (define-key python-mode-map "\C-x-" 'kill-buffer-and-window)
  (make-local-variable 'write-contents-functions)
  ;; (add-hook 'write-contents-functions 'maybe-delete-trailing-whitespace)
  ;; (add-hook 'write-contents-functions 'pyflakes-this-file)

  (if (enable-python-formatting-p)
      (apheleia-mode +1))

  (setq indent-tabs-mode nil)
  )
(add-hook 'python-mode-hook 'mde-python-mode-hook)

;; (defadvice beginning-of-defun (around python-bod activate)
;;   "Extension to work in Python mode."
;;   (if (eq major-mode 'python-mode)
;;       (beginning-of-python-def-or-class 'either (ad-get-arg 0))
;;     ad-do-it))
;; 
;; (defadvice end-of-defun (around python-eod activate)
;;   "Extension to work in Python mode."
;;   (if (eq major-mode 'python-mode)
;;       (end-of-python-def-or-class 'either (ad-get-arg 0))
;;     ad-do-it))

;; It would be cleaner to do this kill-buffer-and-window hacking with advice instead.
(defun python-override-kill-buffer-and-window ()
  "Avoid accidental killing of Python shell buffers."
  (interactive)
  (if (string-match "python" (buffer-name))
      (error "You probably meant to hit \"C-c -\", not \"C-x -\"")
    (kill-buffer-and-window)))
(defun shell-override-kill-buffer-and-window ()
  "Avoid accidental killing of shell buffers."
  (interactive)
  (error "Kill shell buffers with C-x k  (M-x kill-buffer)"))
;; Doing this in all shell buffers seems overkill; but on the other hand,
;; I do hate to lose a lot of work in a shell buffer.
;; (defadvice shell (after set-keys activate)
;;   ;; It's too easy to kill a shell buffer, especially a python-shell
;;   ;; in which "C-c -" is bound to a useful keystroke.
;;   (local-set-key "\C-x-" 'shell-override-kill-buffer-and-window))



(defun python-symbol-around-point ()
  "Return a string consisting of the symbol that point is within (or near)."
  (or (symbol-at-point)                 ; defined in "thingatpt.el"
      (save-excursion
        ;; skip backward over open-parentheses and spaces, then try again
        (while (and (not (bobp))
                    (memq (char-syntax (preceding-char)) '(?\( ?\ )))
          (forward-char -1))
        ;; also, (symbol-at-point) fails if just after final character.
        (if (and (not (bobp))
                 (eq ?\w (char-syntax (preceding-char))))
            (forward-char -1))
        (symbol-at-point))))

(defun python-comment-indent ()
  "Choose comment column for Python comments.  Lifted from `lisp-comment-indent'."
  (if (looking-at "\\s<\\s<\\s<")
      (current-column)
    (progn
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

;; (defadvice indent-for-tab-command (around move-to-text activate)
;;   "In Python mode, if at first column, then move to first non-space character."
;;   (if (and (eq major-mode 'python-mode)
;;            (zerop (current-column))
;;            (looking-at "[ \t]+[^ \t\n]"))
;;       (goto-char (1- (match-end 0)))
;;     ad-do-it))

;; To fix later.  Executing this causes the error
;;   "Info-insert-dir: Can't find the Info directory node"
;; but is that because the directory doesn't exist?
;; (with-eval-after-load "info"
;;      (setq Info-directory-list
;;         (cons (substitute-in-file-name "$HOME/emacs/auctex-11.85/doc")
;;               Info-directory-list)))



(defun compilation-fix-python-ruff ()
  "Apply the suggestions made by ruff in the *compilation* buffer.
Returns t if any change was made, nil otherwise."
  (interactive)
  (let ((result nil)
        (roper-buffer (get-buffer-create "*roper output*")))
    (with-current-buffer "*compilation*"
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\):[0-9]+:[0-9]+: \\(N802 Function name\\|N806 Variable\\|N816 Variable\\) `\\([a-zA-Z0-9_]+\\)`" nil t)
        (let* ((filename (match-string 1))
               (mixedCase (match-string 3))
               (replacement (string-inflection-underscore-function mixedCase))
               (roper-args (list "rename-by-name" "--do" filename mixedCase replacement)))
          (with-current-buffer roper-buffer
            (insert (format "\nCalling roper on %s\n"  roper-args)))
          (condition-case _err
              (progn
                (apply #'call-process "roper" nil roper-buffer nil roper-args)
                (setq result t))
            (error
             ;; do nothing
             ))))
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\):[0-9]+:[0-9]+: N816 Variable `\\([a-zA-Z0-9_]+\\)` in global scope should not be mixedCase" nil t)


        (let* ((filename (match-string 1))
               (mixedCase (match-string 2))
               (replacement (string-inflection-underscore-function mixedCase))
               (roper-args (list "rename-by-name" "--do" filename mixedCase replacement)))
          (with-current-buffer roper-buffer
            (insert (format "\nCalling roper on %s\n"  roper-args)))
          (condition-case _err
              (progn
                (apply #'call-process "roper" nil roper-buffer nil roper-args)
                (setq result t))
            (error
             ;; do nothing
             )))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lisp/Scheme programming
;;;

(defvar lisp-major-modes
  '(emacs-lisp-mode lisp-mode fi:common-lisp-mode scheme-mode))

;;;
;;; Lisp
;;;

;; Should also deal with fi:emacs-lisp-mode (which replaces emacs-lisp-mode
;; when ACL extensions are loaded).

(make-variable-buffer-local 'before-save-hook)

(defun mde-lisp-mode-hook ()
  "Michael Ernst's Lisp mode hook."
  (if (featurep 'elide-head)
      (elide-head-mode))
  (swap-return-and-linefeed)
  (make-local-variable 'inleft-string)
  (setq inleft-string ";; ")
  (make-local-variable 'page-delimiter)
  (setq page-delimiter (mde-page-delimiter ?\;))
  ;; Permit fill-paragraph to work
  (setq paragraph-ignore-fill-prefix nil)
  ;; Not write-file-hooks, as this is about the contents, not the file.
  (make-local-variable 'write-contents-functions)
  ;; Is this what I really want?
  ;; (add-hook 'write-contents-functions 'maybe-delete-trailing-whitespace)
  (add-hook 'write-contents-functions 'check-parens-ignore-on-retry)

  (if (string-match "/plume-lib/"
		    (directory-file-name default-directory))
      (add-hook 'before-save-hook 'delete-trailing-whitespace))
  (if (eq major-mode 'lisp-mode)
      ;; Not emacs-lisp-mode, fi::*-mode, etc.
      (progn
        (if (fboundp 'fi:clman)
            (progn
              (define-key lisp-mode-map "\C-hf" 'fi:clman)
              (define-key lisp-mode-map "\C-ha" 'fi:clman-apropos)))
        (define-key lisp-mode-map "\C-cl" 'run-lisp)))

  )
(add-hook 'lisp-mode-hook 'mde-lisp-mode-hook)

(put 'with-open-file 'lisp-indent-function 1)
(put 'with-input-from-string 'lisp-indent-function 1)


;;;
;;; Emacs Lisp
;;;

;; Checkdoc is already autoloaded in Emacs 20.

;;; Documentation checking
;; Change this when I trust it more.
(setq checkdoc-autofix-flag 'query)     ; default 'semiautomatic
;; May need to do ispell-check-ispell
(setq checkdoc-spellcheck-documentation-flag 'buffer) ; default nil
;; not useful for variable docstrings, though good for function docstrings
(setq checkdoc-verb-check-experimental-flag nil)
(setq checkdoc-permit-comma-termination-flag t)
(setq checkdoc-triple-semi-comment-check-flag nil)
(setq checkdoc-force-history-flag nil)

;; It's a bit too annoying for this to happen every time.
;; (Maybe t would be less annoying than 'ask.)
(setq maybe-checkdoc-flag nil)

(defvar maybe-checkdoc-flag 'ask
  "Non-nil means `maybe-checkdoc-current-buffer' checks for style;
nil means don't check.
If the value is neither nil nor t, then the user is queried first.")
(make-variable-buffer-local 'maybe-checkdoc-flag)
(defun maybe-checkdoc-current-buffer ()
  "Check documentation/style for current buffer, if not in distribution directory."
  (if (and (or (not (buffer-file-name))
               (not (string-match "/emacs[-0-9./]*/lisp" (buffer-file-name))))
           (or (eq t maybe-checkdoc-flag)
               (and maybe-checkdoc-flag
                    (y-or-n-p "Check style of buffer? "))))
      (let ((checkdoc-spellcheck-documentation-flag nil)
            (checkdoc-autofix-flag 'never))
        ;; Don't save-window-excursion, as checkdoc will change it when it
        ;; shows the "*Style Warnings*" buffer.
        (pop-to-buffer (current-buffer))
        (checkdoc-current-buffer 'take-notes)))
  ;; Always return nil; returning t means the buffer has been written
  nil)


(add-hook 'lisp-mode-hook 'mde-lisp-mode-hook)

(setq auto-mode-alist (append '(("\\.elc$"  . emacs-lisp-mode))
                              auto-mode-alist))
(defun mde-emacs-lisp-mode-hook ()
  "Michael Ernst's Emacs Lisp mode hook."
  (run-hooks 'lisp-mode-hook)
  (if (enable-elisp-formatting-p)
      (apheleia-mode +1))
  (apheleia-mode +1)

  (add-hook 'write-contents-functions 'maybe-checkdoc-current-buffer 'append) ; execute last
  )
(add-hook 'emacs-lisp-mode-hook 'mde-emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'mde-emacs-lisp-mode-hook)
;; These define-key's needn't be in the hook, as the map is already defined.
;; (Uh, why is it already defined?)
(define-key emacs-lisp-mode-map "\M-\C-x" 'eval-or-compile-defun) ; was eval-defun
(define-key emacs-lisp-mode-map "\C-xx" 'edebug-defun)

;; This is useful enough to define everywhere.
;; (define-key emacs-lisp-mode-map "\C-cf" 'find-function)      ; like M-. (find-tag)
(global-set-key "\C-cf" 'find-function) ; like M-. (find-tag)


(with-eval-after-load "edebug"
  ;; largely lifted from "let*"
  (def-edebug-spec elib-set-buffer-bind-dll-let*
    (form
     (&rest
      &or symbolp (gate symbolp &optional form))
     body))
  (def-edebug-spec crypt-save-point
    (body)))


(defun orphaned-elc-files ()
  "List .elc files on `load-path' for which no .el file exists in the directory."
  (let ((dirs (delete-dups load-path))
        (result '()))
    (while dirs
      (let* ((all-files (and (file-readable-p (car dirs))
                             (directory-files (car dirs) nil "\\.elc?")))
             (files all-files))
        (while files
          (let ((file (car files)))
            (if (string-match "\\.elc$" file)
                (if (not (member (substring file 0 -1) all-files))
                    (setq result (cons (concat (car dirs) "/" file) result)))))
          (setq files (cdr files))))
      (setq dirs (cdr dirs)))
    result))
;; (orphaned-elc-files)


(defun enable-elisp-formatting-p ()
  "Returns true if the file matches a hard-coded list of directories."
  (let ((filename (buffer-file-name)))

    (cond

     ;; Not visiting a file.
     ((not filename)
      nil)

     ;; No formatting

     ((or (string-match-p "/resources/projects-source/" filename)
          )
      nil)

     ;; Perform formatting

     ((or (string-match-p "/mdedots.*/emacs/" filename)
          (string-match-p "/run-google-java-format" filename)
          (string-match-p "/plume-lib" filename)
          (string-match-p "mewmde\\.el$" filename)
          (string-match-p "/nsf-grant-extras" filename)
          )
      t)

     ;; No formatting for all other projects
     (t
      nil))))

;;; Experimentally commenting out, 2025-04-07.
;; ;; This is most important for systems where my quota is tight.
;; ;; (Probably javadoc-index shouldn't be under revision control anyway...)
;; (defvar non-byte-compiled-files
;;   '("~/.javadoc-index.el"))
;; (defun purge-undesired-elc-files ()
;;   "Remove .elc files that should not have been made in the first place."
;;   (let ((els non-byte-compiled-files))
;;     (while els
;;       (let ((elc (concat (car els) "c")))
;;         (if (file-exists-p elc)
;;             (delete-file elc)))
;;       (setq els (cdr els)))))
;; (run-with-idle-timer 10 nil 'purge-undesired-elc-files)


;;;
;;; Scheme
;;;

(add-hook 'scheme-mode-hook (function (lambda ()
                                        (run-hooks 'lisp-mode-hook)
                                        (require 'scheme-mde))))
(setq auto-mode-alist (append '(("\\.ss$"  . scheme-mode))
                              auto-mode-alist))
;; Emacs 19 and 20 use 'scheme-indent-function, not -hook
(put 'local 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'make-class 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(add-hook 'xscheme-start-hook 'mde-xscheme-start-hook)
;; For some reason, this doesn't seem to work.
(add-hook 'chez-scheme-mode-hook (function (lambda ()
                                             (run-hooks 'scheme-mode-hook))))

;;;
;;; Common Lisp
;;;

;; (defadvice fi::get-lisp-interactive-arguments (before dont-ask activate)
;;   "Set `first-time' to nil, so that I'm not prompted for ACL arguments."
;;   (ad-set-arg 0 nil))
(defun mde-fi:common-lisp-mode-hook ()
  "Michael Ernst's \"fi:\" Common Lisp mode hook."
  (run-hooks 'lisp-mode-hook)
  (setq inleft-string ";; "))


(with-eval-after-load "fi-clman"
  (define-key fi:clman-mode-map "\C-hf" 'fi:clman))
(with-eval-after-load "fi-site-init"
  (global-set-key "\C-c\C-m" 'fi:clman)
  (fset 'clman 'fi:clman)
  (fset 'clman-apropos 'fi:clman-apropos)
  ;; this add-hook wasn't working outside the eval-after-load; how about now?
  (add-hook 'fi:common-lisp-mode-hook 'mde-fi:common-lisp-mode-hook)
  ;; I don't just do the define-key in (eval-after-load "fi-modes.elc" ...)
  ;; because that file defines the variables but leaves them set to nil
  (add-hook
   'fi:inferior-common-lisp-mode-hook
   #'(lambda ()
       (define-key fi:inferior-common-lisp-mode-map "\C-hf" 'fi:clman)
       (define-key fi:inferior-common-lisp-mode-map "\ep" 'fi:pop-input)
       (define-key fi:inferior-common-lisp-mode-map "\en" 'fi:push-input)
       (add-hook 'fi::subprocess-filter-output-preprocess-hook
                 'mde-fi::subprocess-filter-output-preprocess-hook)
       (setq comint-prompt-regexp "^\\(\\[[0-9]+\\] \\)?[-a-zA-Z]+([0-9]+): ")))
  (add-hook
   'fi:common-lisp-mode-hook
   #'(lambda ()
       (define-key fi:common-lisp-mode-map "\C-hf" 'fi:clman)
       (define-key fi:common-lisp-mode-map "\C-ha" 'fi:clman-apropos)
       ;; Not eval-or-compile-last-sexp, because I want to see the result.
       (define-key fi:common-lisp-mode-map "\C-x\C-e" 'fi:lisp-eval-last-sexp))))


(defvar mde-fi::subprocess-redefinition-regexp
  (mapconcat (function identity)
             '("\\(^\\|\n\\)Warning: .*"
               "was defined in"
               ".*\\.lisp"
               "and is now" "being"
               "defined \\(at the top level\\|in" ".*\\.lisp\\)\n")
             "\\( \\|\n +\\)")
  "A separate variable to avoid the `mapconcat' on every execution.
Output that matches this is swallowed by the filter.")

(defun mde-fi::subprocess-filter-output-preprocess-hook (string)
  "Filter out uninteresting errors out of STRING, then return the result."
  ;; not concat, as string might contain "%".
  ;; (message "fi:output[1] = <<%s>>" string)
  (while (string-match mde-fi::subprocess-redefinition-regexp string)
    (setq string (concat (substring string 0 (match-end 1))
                         (substring string (match-end 0)))))
  (if (string-match "\\(^\\|\n\\)Warning: .* was defined in" string)
      (message "Why didn't fi::... match?\n<<<%s>>>" string))
  (if (string-match "was defined in" string)
      (message "WHY didn't fi::... match?\n<<<%s>>>" string))
  (while (string-match "\\(^\\|\n\\);  Note: doing tail merge\n" string)
    (setq string (concat (substring string 0 (match-end 1))
                         (substring string (match-end 0)))))
  (if (string-match "\\(^\\|\n\\); Autoloading for EXCL::COMPLEX-LOOP-EXPANDER:\n" string)
      (setq string (concat (substring string 0 (match-end 1))
                           (substring string (match-end 0)))))
  (if (string-match "\\(^\\|\n\\); Fast loading /projects/null/ai.IRIX/acl4.2/lib/code/loop.fasl.\n" string)
      (setq string (concat (substring string 0 (match-end 1))
                           (substring string (match-end 0)))))
  ;; not concat, as string might contain "%".
  ;; (message "fi:output[2] = <<%s>>" string)
  string)

;; Defaults to eval to maintain emacs bindings and because usually when I'm
;; evaluating code in the buffer I'm willing to put up with it being
;; slightly slower since that makes it that much easier to debug; next
;; emacs session it will be compiled for me, and it gets compiled when I
;; write the buffer anyway.
(defun eval-or-compile-defun (&optional compilep)
  "With prefix argument, compile defun; otherwise, evaluate it.
Interactively, supply optional flag argument COMPILEP."
  (interactive "P")
  ;; The nil arguments mean don't insert the value in the buffer.
  (if compilep
      (compile-defun nil)
    (eval-defun nil)))

(defun mde-inferior-lisp-mode-hook ()
  "CMU Common Lisp Inferior Lisp mode (ie, created via `run-lisp') hook."
  (setq comint-prompt-regexp "^\\(\*\\|[0-9]+\\]+\\) ")
  (if (fboundp 'fi:clman)
      (progn
        (eval-when-compile (require 'inf-lisp))
        (define-key inferior-lisp-mode-map "\C-hf" 'fi:clman)
        (define-key inferior-lisp-mode-map "\C-ha" 'fi:clman-apropos))))
(add-hook 'inferior-lisp-mode-hook 'mde-inferior-lisp-mode-hook)



;; Perhaps I can't use
;;   (modify-syntax-entry ?# ". 14" c-mode-syntax-table)
;;   (modify-syntax-entry ?| ". 23" c-mode-syntax-table)
;; because # currently has syntax quote: "'".

;;; Experimentally commenting out, 2025-04-06.
;; (defadvice forward-comment (after lisp-mode-forward-comment activate)
;;   "Partially cope with Lisp #|| ... ||# comments.
;; This skips over them, but the return value isn't sensible."
;;   (if (and (memq major-mode lisp-major-modes)
;;            (not ad-return-value)
;;            (looking-at "#||"))
;;       (let ((depth 1))
;;         (forward-char 3)
;;         (while (and (> depth 0)
;;                     (re-search-forward "||#\\|#||" nil t))
;;           (cond ((equal "||#" (match-string 0))
;;                  (setq depth (1- depth)))
;;                 ((equal "#||" (match-string 0))
;;                  (setq depth (1+ depth)))
;;                 (t
;;                  (error "What match? %s" (match-string 0)))))
;;         (if (zerop depth)
;;             (progn
;;               (skip-chars-forward " \t\n\r\f")
;;               0)
;;           (error "No closing ||# for #|| comment starter at depth %s" depth)))))


;; Reduce indentation in structure definitions.

(defvar defstruct-comment-column 20
  "*Column to indent comments to in structure definitions.
Nil means use `comment-column'.")

;; \\s< is comment-starter
(defun lisp-comment-indent ()
  "Indent appropriately for Lisp mode.
How does this differ from whatever is built in?"
  (if (looking-at "\\s<\\s<\\s<")
      (current-column)
    (if (looking-at "\\s<\\s<")
        (let ((tem (calculate-lisp-indent)))
          (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           (save-excursion
             (beginning-of-defun)
             (if (looking-at "(defstruct")
                 defstruct-comment-column
               comment-column))))))


;; For planning problems
;; This "99" approximates the current indentation in problem files
(put ':operator 'lisp-indent-function 99)
(put ':operator 'fi:lisp-indent-hook 99)
(put 'define 'lisp-indent-function 1)
(put 'define 'fi:lisp-indent-hook 1)
(put 'forall 'lisp-indent-function 1)
(put 'forall 'fi:lisp-indent-hook 1)

;; For invariant checking
(put 'with-invariants-check 'lisp-indent-function 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rust
;;;

;; Is this needed?
;; (require 'rust-mode)

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(setq rustic-cargo-clippy-trigger-fix 'on-compile)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))

(setq rust-format-on-save t)

(defun mde-rust-mode-hook ()
  (apheleia-mode +1))

(add-hook 'rust-mode-hook 'mde-rust-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; m4 and conf
;;;

(defun mde-conf-mode-hook ()
  "Run the `createcal' program after its input files have been edited."
  ;; Documentation for createcal: https://courses.cs.washington.edu/tools/createcal/doc/
  (let ((filename (and buffer-file-name (file-truename buffer-file-name))))
    (if (and filename
             (string-match "/calendar/\\(inputFiles\\|htmlTemplates\\)/" filename)
             (not (string-match "/503/17sp/" filename)))
        (add-hook 'after-save-hook 'run-createcal nil 'local))))
;; TODO: need to apply this hook to files such as hwlist.template as well as .ini files
(add-hook 'conf-mode-hook 'mde-conf-mode-hook)

(defun run-createcal ()
  "Run external program `createcal' in the parent directory."
  (interactive)
  (let ((bufname "*createcal Output*"))
    (shell-command "cd `realpath ..` && createcal" bufname)
    ;; Show output if there is any (it will all be error output)
    (if (bufferp bufname)
        (pop-to-buffer bufname))))


(defun run-createcal ()
  "Run external program `createcal' in the parent directory."
  (interactive)
  (let ((bufname "*createcal Output*"))
    (let ((default-directory (parent-directory default-directory)))
      (call-process-show-if-error "createcal"))
    ;; Show output if there is any (it will all be error output)
    (if (bufferp bufname)
        (pop-to-buffer bufname))))

(defun call-process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer 
      (list (apply 'call-process program nil (current-buffer) nil args)
            (buffer-string))))

(defun call-process-show-if-error (program &rest args)
  "Run PROGRAM with ARGS and show the output if the exit status is non-zero."
  (let* ((program-name (file-name-nondirectory program))
         (bufname (concat "*" program-name " " default-directory " Output*"))
         (status
          (with-current-buffer (get-buffer-create bufname)
            (if (get-buffer-process bufname)
                (sleep-for .5))
            (apply 'call-process program nil (current-buffer) nil args))))
    (if (not (= 0 status))
        (pop-to-buffer bufname))))


(defun mde-m4-mode-hook ()
  "Run the `createcal' program after its input files have been edited."
  ;; Documentation for createcal: https://courses.cs.washington.edu/tools/createcal/doc/
  (if (buffer-file-name)
      (let* ((filename (file-truename buffer-file-name))
             (dirname (file-name-directory filename)))
        (if (file-exists-p (concat dirname "Makefile"))
            (add-hook 'after-save-hook #'(lambda () (run-command nil "make")) nil 'local)))))
(add-hook 'm4-mode-hook 'mde-m4-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation
;;;

(use-package recompile-on-save :ensure t
  :init
  (recompile-on-save-advice compile))

;; Compile calls `save-some-buffers', but I don't want a question about the current buffer.
(defun compile--save (_command &optional _comint)
  "Save current buffer before performing compilation.
This avoids a question, the answer to which would surely be \"Yes\"."
  (save-buffer-if-modified))
(advice-add 'compile :before #'compile--save)

(defun compile--check-for-bad-regexps (_command &optional _comint)
  "Err if an element of compilation-error-regexp-alist starts with \".*\".
Such regexps have very bad performance, especially for long lines
in compilation output."
  (dolist (cer compilation-error-regexp-alist)
    (if (listp cer)
        (let ((regexp (car cer)))
          (if (string-prefix-p ".*" regexp)
              (error "Element of compilation-error-regexp-alist starts with \".*\": %s" cer))))))
(advice-add 'compile :before #'compile--check-for-bad-regexps)

(defun compile--recompute-compile-command (_command &optional _comint)
  "In certain modes, re-compute `compile-command'."
  (if (memq major-mode '(shell-mode))
      (set-compile-command-for-directory)))
(advice-add 'compile :before #'compile--recompute-compile-command)


(defun next-error-recenter ()
  "Move point to top of screen, if point is so close to bottom that some
explanatory text is probably off-screen."
  (let ((error-win (get-buffer-window next-error-last-buffer)))
    (if error-win
        (with-selected-window error-win
          (if (not (pos-visible-in-window-p (point-max) (selected-window)))
              (let ((lines-from-bottom (count-lines (point) (window-end))))
                (if (< lines-from-bottom 4)
                    (recenter 2))))))))

(add-hook 'next-error-hook 'next-error-recenter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set compilation command
;;;

(defun file-in-super-directory (filename &optional dir)
  "Return an absolute filename if the file exists in this directory or a parent,
or null if it does not exist."
  (if (not dir)
      (setq dir default-directory))
  (let ((expanded (expand-file-name filename dir)))
    (if (file-exists-p expanded)
        expanded
      (let ((parent-dir (file-name-directory (directory-file-name dir))))
        (if (null parent-dir)
            ;; If dir = "~/", then directory-file-name = ~ and parent-dir = nil
            nil
          (if (equal dir parent-dir)
              nil
            (file-in-super-directory filename parent-dir)))))))
;; (file-in-super-directory "prog-modes-mde.el")
;; (file-in-super-directory ".emacs")
;; (file-in-super-directory "foobarbazunlikely")
;; (file-in-super-directory "build.xml" nil)

;; TODO: should return false if `compile-command' is already set.
(defun should-set-compile-command ()
  "Return non-nil if the default \"make\" compilation command is inappropriate."
  (and
   ;; editing a file or directory
   (or buffer-file-name
       (memq major-mode
	     '(compilation-mode cvs-mode dired-mode grep-mode magit-status-mode
				rg-mode shell-mode svn-status-mode)))
   ;; Makefile doesn't exist, so we need a different command
   (not (or (file-exists-p (expand-file-name "Makefile"))
            (file-exists-p (expand-file-name "makefile"))
            (file-exists-p (expand-file-name "GNUmakefile"))))))

;; I would like this to work for shell mode, but I would need to make it run
;; as part of M-x compile instead of when the mode is set.
(defun set-compile-command-for-directory ()
  "Returns true if it set the `compile-command' variable.
Sets the variable to an invocation of \"ant\", \"gradle\", \"mvn\", etc.
depending on whether a build.xml, build.gradle, or pom.xml file exists
in this directory or some superdirectory."
  (if (should-set-compile-command)
      (cond
       ;; Gradle
       ((or (file-readable-p "build.gradle")
	    (file-readable-p "build.gradle.kts"))
        (make-local-variable 'compile-command)
        (let ((gradle-command (cond ((file-readable-p "gradlew")
				     "./gradlew")
				    ((file-readable-p "gradle/gradlew")
				     "./gradle/gradlew")
				    (t
				     "gradle"))))
	  ;; Not "build" because tests often take a long time to run.
          (setq compile-command
		(concat gradle-command " " (gradle-task-for-buffer)))))
       ;; Maven
       ((file-readable-p "pom.xml")
        (make-local-variable 'compile-command)
        (let ((mvn-command (if (file-readable-p "mvnw")
			       "./mvnw"
			     "mvn")))
	  (setq compile-command (concat mvn-command " -e -B verify"))))
       ;; Brazil (must precede Ant because the build.xml file also exists)
       ((file-readable-p "Config")
        (make-local-variable 'compile-command)
        (setq compile-command "brazil-build "))
       ;; Ant
       ((file-readable-p "build.xml")
        (make-local-variable 'compile-command)
        (setq compile-command "ant -e "))
       ;; Rake
       ((file-readable-p "Rakefile")
        (make-local-variable 'compile-command)
        (setq compile-command "rake"))
       ;; Seek file in a superdirectory
       ;; Gradle
       ((or (file-in-super-directory "build.gradle" default-directory)
	    (file-in-super-directory "build.gradle.kts" default-directory))
        (let* ((buildfile (or (file-in-super-directory
			       "build.gradle" default-directory)
			      (file-in-super-directory
			       "build.gradle.kts" default-directory)))
               (buildfile-dir (file-relative-name
			       (file-name-directory buildfile) default-directory))
	       (gradle-command
		(if (file-readable-p (concat buildfile-dir "/gradlew"))
		    "./gradlew"
		  "gradle"))
	       (target (gradle-task-for-buffer)))
          (make-local-variable 'compile-command)
          (setq compile-command
                (concat "cd " buildfile-dir " && " gradle-command " " target))))
       ;; Maven
       ((file-in-super-directory "pom.xml" default-directory)
        (let* ((buildfile (file-in-super-directory
                           "pom.xml" default-directory))
	       (maven-command
		(let ((mvnw (file-relative-name
			     default-directory
			     (concat (file-name-directory buildfile)
				     "mvnw"))))
		  (if (file-readable-p mvnw)
		      mvnw
		    "mvn"))))
          (make-local-variable 'compile-command)
          (setq compile-command
                (concat maven-command " -e -B" " -f " buildfile " verify"))))
       ;; Brazil
       ((let ((buildfile (file-in-super-directory
                          "Config" default-directory)))
          (and buildfile
	       ;; hack to account for mysterious directory named build.xml
               (not (file-directory-p buildfile))))
	(let ((buildfile (file-in-super-directory
			  "Config" default-directory)))
	  (make-local-variable 'compile-command)
	  (setq compile-command (concat "cd " (file-relative-name
					       default-directory
					       (file-name-directory buildfile))
					" && brazil-build "))))
       ;; Ant
       ((let ((buildfile (file-in-super-directory
                          "build.xml" default-directory)))
          (and buildfile
	       ;; hack to account for mysterious directory named build.xml
               (not (file-directory-p buildfile))))
        (make-local-variable 'compile-command)
        (setq compile-command "ant -e -find build.xml "))
       )))
(defun gradle-task-for-buffer ()
  "Returns the gradle task to run for the given file.  Usually \"assemble\"."
  (let ((file-name (or buffer-file-name default-directory)))
    (cond ((and file-name (string-match "/plume-lib" file-name))
	   "spotlessApply build")	; "build" also runs "shadowJar"
	  (t
	   "assemble"))))
(add-hook 'find-file-hook 'set-compile-command-for-directory)
(add-hook 'dired-mode-hook 'set-compile-command-for-directory)
(add-hook 'compilation-mode-hook 'set-compile-command-for-directory)
(add-hook 'cvs-mode-hook 'set-compile-command-for-directory)
(add-hook 'svn-status-mode-hook 'set-compile-command-for-directory)
(add-hook 'magit-status-mode-hook 'set-compile-command-for-directory)

;; Below are for modes that have a default to use if there is no makefile
;; or build.xml file.

(defun c-set-compile-command ()
  "Set `compile-command' appropriately for C files.
Use as a hook, like so:
  (add-hook \\='c-mode-hook \\='c-set-compile-command)"
  (if (should-set-compile-command)
      (let ((file-name (file-name-nondirectory buffer-file-name)))
        (make-local-variable 'compile-command)
        (setq compile-command
              (concat "gcc -Wall -g -o "
                      (file-name-sans-extension file-name)
                      " " file-name)))))

(defun c++-set-compile-command ()
  "Set `compile-command' appropriately for C++ files.
Use as a hook, like so:
  (add-hook \\='c++-mode-hook \\='c++-set-compile-command)"
  (if (should-set-compile-command)
      (let ((file-name (file-name-nondirectory buffer-file-name)))
        (make-local-variable 'compile-command)
        (setq compile-command
              (concat "g++ -Wall -g -o "
                      (file-name-sans-extension file-name)
                      " " file-name)))))

(defun java-set-compile-command ()
  "Set `compile-command' appropriately for Java files.
Use as a hook, like so:
  (add-hook \\='java-mode-hook \\='java-set-compile-command)"
  (if (and (should-set-compile-command)
           (not (set-compile-command-for-directory)))
      (let ((file-name (file-name-nondirectory buffer-file-name)))
        (make-local-variable 'compile-command)
        (setq compile-command (concat "javac -g " file-name)))))

;; To do: abstract this out to use a variable
(defun special-case-set-compile-command ()
  "Override default, to set `compile-command' properly for specific projects."
  (cond ((string-match "/eclat/" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml compile-no-eclipse"))

        ;;; Checker Framework demos
	;; These commented-out demos aren't currently working.
	;; ((string-match "/annotations/demos/nonnull-interned-demo/checker/" default-directory)
	;;  (make-local-variable 'compile-command)
	;;  (setq compile-command "cd $anno/demos/nonnull-interned-demo/checker/; ant -e framework"))
	;; ((string-match "/annotations/demos/nonnull-interned-demo/personalblog-demo/" default-directory)
	;;  (make-local-variable 'compile-command)
	;;  (setq compile-command "cd $anno/demos/nonnull-interned-demo/personalblog-demo/; ant -e"))
	;; ((string-match "/annotations/demos/nonnull-interned-demo/junit/"
	;; 	       default-directory)
	;;  (make-local-variable 'compile-command)
	;;  (setq compile-command "cd $anno/demos/nonnull-interned-demo/junit/; ant -e"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/IGJChecker/src/checkers/types/AnnotationLocation.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml location"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/IGJChecker/src/checkers/types/AnnotatedTypeFactory.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml factory-old"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/IGJChecker/src/org/checkerframework/framework/type/AnnotatedTypeFactory.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml factory"))
        ((and buffer-file-name (string-match "demos/nonnull-interned-demo/junit/tests/JUnitTests.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml test-assert"))
        ;; This only works if you delete the buffer and re-visit the file,
        ;; because compile-command is set when the file is first visited.
        ((and buffer-file-name
              (string-match "/personalblog-demo/src/net/eyde/personalblog/" buffer-file-name)
              (save-excursion
                (goto-char (point-min))
                (not (search-forward "executeQuery(constructQuery" nil t))))
         (make-local-variable 'compile-command)
         (setq compile-command "ant -e -find build.xml check-tainting"))
        ((and buffer-file-name
              (string-match "plume-lib-for-demo/java/src/plume/ICalAvailable.java" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command "make typecheck-only"))

	;;; Optional demo
	((and buffer-file-name
	      (string-match
	       "checker-framework-optional-demo/checker/src/main/java/org/checkerframework/checker/optional/"
	       (file-truename buffer-file-name)))
         (make-local-variable 'compile-command)
         (setq compile-command "$cf/gradlew -p $cf compileJava"))
	((and buffer-file-name
	      (string-match
	       "checker-framework-optional-demo/checker/tests/optional/SubtypeCheck.java"
	       (file-truename buffer-file-name)))
         (make-local-variable 'compile-command)
         (setq compile-command "$ch/bin/javac \\
  -processor org.checkerframework.common.subtyping.SubtypingChecker \\
  -Aquals=org.checkerframework.checker.optional.qual.Present,org.checkerframework.checker.optional.qual.MaybePresent \\
  SubtypeCheck.java")
	 ;; Set compile-history to run different command the next time
         (make-local-variable 'compile-history)
	 (setq compile-history (list "$ch/bin/javac -processor optional SubtypeCheck.java")))
	((and buffer-file-name
	      (string-match
	       "checker-framework-optional-demo/checker/tests/optional/JdkCheck.java"
	       (file-truename buffer-file-name)))
         (setq compile-command "$ch/bin/javac -processor optional JdkCheck.java"))
	((and buffer-file-name
	      (string-match
	       "checker-framework-optional-demo/checker/tests/optional/"
	       (file-truename buffer-file-name)))
         (make-local-variable 'compile-command)
         (setq compile-command
	       (concat "$ch/bin/javac -processor optional "
		       (file-name-nondirectory buffer-file-name))))
	;; Is this necessary?
	((and buffer-file-name
              (string-match "/checker/tests/optional/" buffer-file-name))
         (make-local-variable 'compile-command)
         (setq compile-command (concat "javacheck -processor optional "
				       (file-name-nondirectory buffer-file-name))))
        ;;; end of Checker Framework demos

	;;; Checker Framework tests
        ((string-match "\\(^.*\\)/\\(?:checker\\|framework\\)/tests/\\([^/]*\\)/" default-directory)
         (let ((cf-dir (file-relative-name (match-string 1 default-directory) default-directory))
	       (base-dir (match-string 2 default-directory)))
           (if (equal base-dir "src")
               (setq base-dir "all"))
	   ;; (setq base-dir (replace-regexp-in-string "flow2" "flow" base-dir))
           (make-local-variable 'compile-command)
	   (let* ((testname1 (hyphens-to-camelcase-and-capitalize-first base-dir))
		  (testname (cond ((equal testname1 "Initialization") "Nullness")
				  ((equal testname1 "Flowexpression") "FlowExpressionChecker")
				  ((equal testname1 "AinferTestchecker") "ainfer")
				  (t testname1))))
	     (setq compile-command (concat cf-dir "/gradlew -p " cf-dir " "
					   testname "Test")))))
	((and buffer-file-name
	      (string-match
	       "\\(^.*\\)/\\(?:checker\\|framework\\)/src/test/java/org/checkerframework/.*/\\([^/]+\\)Test.java"
	       (file-truename buffer-file-name)))
         (let ((cf-dir (file-relative-name (match-string 1 (file-truename buffer-file-name)) default-directory))
	       (testname (match-string 2 (file-truename buffer-file-name))))
           (make-local-variable 'compile-command)
	   (setq compile-command (concat cf-dir "/gradlew -p " cf-dir " "
					 testname "Test"))))
	((string-match "\\(^.*\\)/\\(?:checker\\|framework\\)/jtreg/" default-directory)
         (let ((cf-dir (file-relative-name (match-string 1 default-directory))))
	   (setq compile-command (concat cf-dir "/gradlew -p " cf-dir " " "jtregTests"))))

	;; Checker Framework manual
	((string-match "\\(^.*/checker-framework\\(?:-fork.[^/]*\\|-branch[^/]*\\)?\\)/docs/manual/" default-directory)
         (make-local-variable 'compile-command)
	 (setq compile-command "make"))
	((string-match "\\(^.*/checker-framework\\(?:-fork.[^/]*\\|-branch[^/]*\\)?\\)/dataflow/manual/" default-directory)
         (make-local-variable 'compile-command)
	 (setq compile-command "make"))

	;; General Checker Framework rule, to rebuild everything rather than just one project.
	((string-match "\\(^.*/checker-framework\\(?:-fork.[^/]*\\|-branch[^/]*\\)?\\)/" default-directory)
	 (make-local-variable 'compile-command)
	 (setq compile-command
	       (if (= (match-end 1) (1- (length default-directory)))
		   "./gradlew assembleForJavac"
		 (let ((cf-dir (file-relative-name (match-string 1 default-directory) default-directory)))
		   (concat cf-dir "/gradlew -p " cf-dir " " "assembleForJavac")))))

        ((string-match "/bzr/.*/doc/en/user-guide/" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "make -C ../../.. doc/en/user-guide/index.html"))
        ((equal (substitute-in-file-name "$HOME/java/plume/") default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command "make -C $HOME/bin/src/plume-lib/java"))
	((string-match "^\\(.*/commons-io-fork-nikshinde1996[^/]*/\\)" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command (concat "cd " (match-string 1 default-directory) " && mvn -e -B install")))
	((string-match "^\\(.*/jdime[^/]*/\\)" default-directory)
         (make-local-variable 'compile-command)
         (setq compile-command (concat "./gradlew installDist")))
        ))
(add-hook 'find-file-hook 'special-case-set-compile-command 'append)
(add-hook 'dired-mode-hook 'special-case-set-compile-command 'append)
(add-hook 'compilation-mode-hook 'special-case-set-compile-command 'append)
(add-hook 'shell-mode-hook 'special-case-set-compile-command 'append)
(add-hook 'magit-status-mode-hook 'special-case-set-compile-command 'append)

(defun hyphens-to-camelcase-and-capitalize-first (hyphens)
  "Change a-qualified-word to AQualifiedWord."
  (mapconcat 'capitalize (split-string hyphens "-") ""))
;; (hyphens-to-camelcase-and-capitalize-first "a-qualified-word")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation error regexps
;;;

;;; Experimentally commenting out, 2025-04-06.
;; ;; omake pattern is said to be inefficient (http://emacs.1067599.n5.nabble.com/bug-13369-24-1-compile-message-parsing-slow-because-of-omake-hack-td274585.html) and I don't know what it is needed for.
;; (with-eval-after-load "compile"
;;   (setq compilation-error-regexp-alist
;;         (delete 'omake compilation-error-regexp-alist)))

;; To debug slowness in parsing compilation errors (due to inefficient
;; regexes in compilation-error-regexp-alist), edit
;; `compilation-parse-errors' to add these at the top of the dolist loop:
;;    (message "%s compilation-parse-errors: working on %s" (current-time-string) item)
;;    (message "%s compilation-parse-errors: done with %s" (current-time-string) item)
;; Maven regex is very slow on long
;; lines, such as those created when building the Daikon manual.
;; (I tried re-enabling this in March 2016 and it still made Emacs unusable.)
;;; TODO: Is this still necessary?
(with-eval-after-load "compile"
  (setq compilation-error-regexp-alist
        (delete 'maven compilation-error-regexp-alist)))
;; Disabled by default because the regexp is slow.  If I am using Maven, run:
;; (use-maven-compilation-error-regexp)
(defun use-maven-compilation-error-regexp ()
  (interactive)
  (require 'compile)
  (add-to-list 'compilation-error-regexp-alist 'maven))

(with-eval-after-load "compile"
  (setq compilation-error-regexp-alist
	(append
	 (list
          ;; Markdownlint omits the last colon (:).
          '("^\\([^\n:]*+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\( error\\)? MD" 1 2)

	  ;; Java stack trace, as printed by a program
	  ;; This permits 2 or 4 leading spaces.
	  '("\\(?:^[ ][ ]\\(?:[ ][ ]\\)?\\|; Stack trace: \\)[A-Za-z0-9_.$]+\\(?:\\.<init>\\)?(\\([A-Za-z0-9_.]+\\):\\([0-9]+\\))$" 1 2)

	  ;; An example Gradle failure:
	  ;; Build file '/home/mernst/class/331/18au/hw/build.gradle' line: 96
	  '("Build file '\\(.*\\)' line: \\([0-9]+\\)$" 1 2)
	  '("^  build file '\\([^']+\\)': \\([0-9]+\\): " 1 2) ;; column number is also available
          
          ;; Kotlin
          '("^e: file://\\(.*\\):\\([0-9]\\):\\([0-9]\\): " 1 2 3)

	  ;; For dmalloc's ra_info output
	  '("^Line \\([0-9]+\\) of \"\\([^\"]*\\)\"" 2 1)
	  ;; For linkchecker
	  '("^Parent URL file:\\(.*\\), line \\([0-9]+\\)" 1 2)
	  ;; For html5validator
	  '("^\\(?::validate\\)?\\(?:WARNING:html5validator.validator:\\)?\"file:\\(.*\\)\":\\([0-9]+\\).\\([0-9]+\\)" 1 2 3)

	  ;; JUnit under ant, such as
	  ;;    [junit] 	at org.checkerframework.framework.stub.StubParser.processCompilationUnit(StubParser.java:447)
	  `(,(concat "    \\[junit\\] 	at [^ \n]+"
		     "("
		     "\\([a-zA-Z][a-zA-Z._0-9]+.java\\):\\([0-9,]+\\)"
		     ")$")
	    1 2)

	  ;; jdb output, such as
	  ;; "  [4] daikon.VarInfo$1GuardingVisitor.visitSlice (VarInfo.java:1,690)"
	  ;; Notice the comma!!  Yuck...   [Does that actually mean line 1690?]
	  `(
	    ,(concat "  \\[[0-9]+\\] [^ \n]+ "
		     "("
		     "\\([a-zA-Z][a-zA-Z._0-9]+.java\\):\\([0-9,]+\\)"
		     ")$")
	    1 2)

	  ;; JJTree (?): "In file daikon/PrintInvariants.javax: Encountered "const vi =" at line 399, column 13."
	  '("^In file \\([a-zA-Z0-9_$]+\\)\\.[a-zA-Z0-9_$]+: .* at line \\([0-9]+\\), column \\([0-9]+\\)" 1 2 3)

	  ;; For checklink:
	  ;; [ant:checkstyle] [ERROR] /....
	  '("^\\(?::[-A-Za-z0-9_]+:checkstyle[-A-Za-z0-9_]+\\)?\\[ant:checkstyle\\] \\[\\(?:ERROR\\|WARN\\)] \\(.*\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3) 
	  '("^\\(?::[-A-Za-z0-9_]+:checkstyle[-A-Za-z0-9_]+\\)?\\[ant:checkstyle\\] \\[\\(?:ERROR\\|WARN\\)] \\(.*\\):\\([0-9]+\\):" 1 2) 

	  ;; Perl error messages
	  '("\\bat \\([^ ]+\\) line \\([0-9]+\\)\\($\\|[\\.,]\\)" 1 2) 
	  '("^syntax error in file \\([^ ]*\\) at line \\([0-9]+\\)," 1 2) 

	  ;; Python error messages
	  '("^ *File \"\\(.*\\)\", line \\([0-9]+\\)" 1 2) 
	  '("^SyntaxError: ('invalid syntax', ('\\(.*\\)', \\([0-9]+\\), " 1 2) 
          ;; Python ty typechecker messages
	  '("^ *--> \\(.*\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3)

	  ;; Parse CMUCL error messages.
	  ;; Problem: these are character numbers, not line numbers.
	  ;;    Reader error at 47821 on #<Stream for file "...">:
	  '("^Reader error at \\([0-9]+\\) on #<Stream for file \"\\(.*\\)\">:" 2 1)

	  ;; for gud
	  '("\\bat \\([^ \n]+\\):\\([0-9]+\\)$" 1 2) 

	  ;; Parse BibTeX error messages
	  '("Warning--.*\n--line \\([0-9]+\\) of file \\(.*\\)" 2 1) 
	  '("---line \\([0-9]+\\) of file \\(.*\\)" 2 1) 

	  ;; XOM
	  '("^\\[Fatal Error\\] \\([^ ]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)

          ;; Ruff
          '("error: Failed to parse \\([^ ]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)

          ;; Asciidoctor
          '("asciidoctor: ERROR: \\([^:]+\\): line \\([0-9]+\\):" 1 2)
          ;; asciidoc-linter
          '("✗ \\([^,]+\\), line \\([0-9]+\\):" 1 2)

          ;; htmlproofer
          '("^\\* At \\(.*\\):\\([0-9]+\\):" 1 2)

          ;; Spotless
          '("^ *\\(.*\\):L\\([0-9]+\\) " 1 2)

          ;; Claude Code
          '("^\\*\\*File\\*\\*: `\\(.*\\):\\([0-9]+\\)\\([-[0-9]\\|, [0-9]\\|`$\\)" 1 2)
          '("^- `\\(.*\\):\\([0-9]+\\)`: " 1 2)

          ;; Unmake
          '("^error: \\(.*\\):\\([0-9]+\\):\\([0-9]+\\) " 1 2 3)

	  )

	 compilation-error-regexp-alist

	 (list
	  ;; gradle leaves text such as ":util:compileJava" in front of error message.
          '("^\\(?::[-_a-zA-Z0-9]+\\)\\{0,2\\}\\(/.*\\):\\([0-9]+\\):\\([0-9]+\\): " 1 2 3)
          '("^\\(?::[-_a-zA-Z0-9]+\\)\\{0,2\\}\\(/.*\\):\\([0-9]+\\): " 1 2)

	  ;; this regex is so general that it needs to be at the end of the list
          '("\\bfile \\([^'][^ ]*[^':]\\) line \\([0-9]+\\)$" 1 2)

	  ;; What language is this for??
	  '("at line \\([0-9]+\\) of file \"\\([^\"]*\\)\"" 2 1)

	  ;; I suspect this regexp is extremely inefficient, and I don't understand it.
	  ;; ;; ant output, such as
	  ;; ;; "    [javac] /afs/athena.mit.edu/user/m/e/mernst/6.170/ps0/src/ps0/Ball.java:18: cannot find symbol"
	  ;; (list
	  ;;    (concat "^ *\\[[a-z]+\\] "
	  ;;            "\\([a-zA-Z][-a-zA-Z._0-9]+: ?\\)?" ;; what is this for?
	  ;;            "\\([a-zA-Z]?:?[^:( \t\n]*[^:( \t\n0-9][^:( \t\n]*\\)[:(][ \t]*\\([0-9]+\\)"
	  ;;            "\\([) \t]\\|:\\(\\([0-9]+:\\)\\|[0-9]*[^:0-9]\\)\\)")
	  ;;    2 3 6)

	  ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic compilation of changed files
;;;

;; Emacs 19's local-write-file-hooks (or Emacs 22's write-file-functions)
;; isn't the right thing, since it runs BEFORE the file is written.
;; Perhaps rather than this code, I should just add the hooks
;; unconditionally, but have them check major-mode and perhaps do nothing.
;; That would be a bit less efficient, but would be less code.


;;; Ask to compile .el and .scm files after saving them.
;; There might be a cleaner way to do this with local hook variables.

(add-hook 'after-save-hook 'run-mode-specific-after-save-buffer-hooks)

(defvar mode-specific-after-save-buffer-hooks nil "\
Alist (MAJOR-MODE . HOOK) of after-save-buffer hooks specific to major modes.")

(defun run-mode-specific-after-save-buffer-hooks ()
  "Run hooks that match the current buffer's major mode.
The hooks are found in `mode-specific-after-save-buffer-hooks'.
A call to this should be put in `after-save-buffer-hooks'."
  (let ((hooks mode-specific-after-save-buffer-hooks))
    (while hooks
      (let ((hook (car hooks)))
        (if (eq (car hook) major-mode)
            (funcall (cdr hook))))
      (setq hooks (cdr hooks)))))

(setq mode-specific-after-save-buffer-hooks
      '((emacs-lisp-mode . ask-to-byte-compile)
        (fi:emacs-lisp-mode . ask-to-byte-compile) ; yuck, Franz changes name
        (scheme-mode . ask-to-scheme-compile)))

(defun ask-to-byte-compile ()
  "Ask the user whether to byte compile the current buffer,
if its name ends in `.el' and the `.elc' file also exists."
  (let ((name (buffer-file-name)))
    (and name (string-match "\\.el$" name)
         (file-exists-p (concat name "c"))
         (if (y-or-n-p (format "Byte-compile %s? " name))
             (byte-compile-file name)
           (message ""))
         )))

(defun ask-to-scheme-compile ()
  "Ask the user whether to compile the current buffer,
if its name ends in `.scm' and the `.bin' or `.com' file also exists."
  (let* ((name (buffer-file-name))
         (root-name (and name
                         (string-match "\\(.*\\)\\.scm$" name)
                         (substring name (match-beginning 1) (match-end 1)))))
    (if root-name
        (cond ((file-exists-p (concat root-name ".com"))
               (if (y-or-n-p (format "Compile (via `cf') %s? " name))
                   (progn
                     (xscheme-send-string (format "(cf \"%s\")" name))
                     (message "Producing %s.com...continue at will" root-name))
                 (message "")))
              ((file-exists-p (concat root-name ".bin"))
               (if (y-or-n-p (format "Compile (via `sf') %s? " name))
                   (progn
                     (xscheme-send-string (format "(sf \"%s\")" name))
                     (message "Producing %s.bin...continue at will" root-name))
                 (message "")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging
;;;

;; ;; Is there not a way to do this in GDB itself?
;; ;; Did I want this only for Vortex, or is there some other reason?
;; (defvar gud-filter-sigsegv-gcfindlimit-state nil)
;; (make-variable-buffer-local 'gud-filter-sigsegv-gcfindlimit-state)
;; (defadvice gud-filter (before skip-sigsegv-in-gcfindlimit activate)
;;   "Continue past segmentation faults in procedure GC_find_limit."
;;   (let ((str (ad-get-arg 1)))           ; not "string" as that's the formal name
;;     ;; Leave this in for debugging, for the time being.
;;     ;; (message "filter (state %s) got <<%s>>" gud-filter-sigsegv-gcfindlimit-state str)
;;     (cond
;;      ((and (not gud-filter-sigsegv-gcfindlimit-state)
;;            (equal str "Program received signal SIGSEGV, Segmentation fault.\n"))
;;       (setq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv))
;;      ((and (eq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv)
;;            (string-match "^0x[0-9a-f]* in GC_find_limit ()\n$" str))
;;       (setq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv-in-gcfindlimit))
;;      ((or (and (eq gud-filter-sigsegv-gcfindlimit-state 'saw-sigsegv-in-gcfindlimit)
;;                (equal str "(gdb) "))
;;           (and (not gud-filter-sigsegv-gcfindlimit-state)
;;                (string-match "\\(^\\|\n\\)Program received signal SIGSEGV, Segmentation fault.\n0x[0-9a-f]* in GC_find_limit ()\n(gdb) $" str)))
;;       (setq gud-filter-sigsegv-gcfindlimit-state nil)
;;       ;; (message "Calling gud-cont")
;;       (gud-cont nil))
;;      (t
;;       (setq gud-filter-sigsegv-gcfindlimit-state nil))))
;;   ;; (message (format "filter ends in state %s" gud-filter-sigsegv-gcfindlimit-state))
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The shell
;;;

(setq comint-scroll-to-bottom-on-input nil)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-scroll-show-maximum-output nil)

(with-eval-after-load "shell"
  (require 'honorary-compile))

;; When I call this, if Info isn't loaded, I get an error
;;  "** reference to free variable Info-current-file"
;; because the byte-compiler appears not to compile advices, but leave them
;; in source form in the .elc file (with or without Emacs 19 compatibility on).
;; This has nothing to do with whether the variable is referenced when the
;; function is actually run.
;; (eval-when-compile (require 'info)) does no good.
;; (defvar Info-current-file) also does no good.
;; But putting eval-when-compile *in the defadvice body* does the trick.

;; (defadvice shell (around choose-alternate-shell activate)
;;   "Select the appropriate shell.
;; If in Python mode, look for a buffer associated with a python process, etc."
;;   ;; This causes an error from M-x edebug-defun:
;;   ;;   While compiling toplevel forms:
;;   ;;     !! Wrong type argument ((consp nil))
;;   (eval-when-compile (defvar Info-current-file))
;;   (cond ((and (or (eq major-mode 'cecil-mode)
;;                   ;; I ought to know which mode I should be in when this
;;                   ;; search will succeed.
;;                   (save-excursion
;;                     (goto-char (point-min))
;;                     (or (looking-at "#include \"vortex-defs-C\\+\\+\\.h\"\n")
;;                         (looking-at "#include \"vortex-defs-Cecil\\.h\"\n")
;;                         (search-forward "\n(** language(\"C++\") **)"
;;                                         ;; not just 200 if buffer is narrowed
;;                                         (+ (point-min) 200) t))))
;;               (get-buffer "*vortex*"))
;;          (switch-to-buffer (get-buffer "*vortex*")))
;;         ((and (memq major-mode '(lisp-mode
;;                                  fi:common-lisp-mode fi:clman-mode
;;                                  fi:lisp-listener-mode))
;;               (or (get-buffer "*allegro*")
;;                   (get-buffer "*inferior-lisp*")))
;;          (switch-to-buffer (or (get-buffer "*allegro*")
;;                                (get-buffer "*inferior-lisp*"))))
;;         ((and (or (memq major-mode '(python-mode))
;;                   (and (eq major-mode 'Info-mode)
;;                        (string-match "python" Info-current-file)))
;;               (get-buffer "*Python*"))
;;          (switch-to-buffer (get-buffer "*Python*")))
;;         ((and (memq major-mode '(comint-mode shell-mode inferior-lisp-mode
;;                                              fi:inferior-common-lisp-mode))
;;               (not (eobp)))
;;          (goto-char (point-max)))
;;         (t
;;          (let ((processes (process-list))
;;                (dir default-directory)
;;                (result nil))
;;            (while processes
;;              (let* ((process (car processes))
;;                     (pbuffer (process-buffer process)))
;;                (if (and pbuffer
;;                         (buffer-live-p pbuffer)
;;                         (not (eq pbuffer (current-buffer)))
;;                         (eq 'run (process-status process))
;;                         (equal default-directory
;;                                (with-current-buffer pbuffer
;;                                  default-directory))
;;                         (with-current-buffer pbuffer
;;                           (and
;;                            (memq major-mode '(comint-mode shell-mode inferior-lisp-mode
;;                                                           fi:inferior-common-lisp-mode))
;;                            (not (string-equal (buffer-name) "*Async Shell Command*")))))
;;                    (setq result pbuffer
;;                          processes nil)
;;                  (setq processes (cdr processes)))))
;;            (if result
;;                (switch-to-buffer result)
;;              ad-do-it)))))

;; (defadvice shell-directory-tracker (before handle-back activate)
;;   "Convert \"back\" into \"cd -\", which `shell-directory-tracker' understands."
;;   (if (and shell-dirtrackp
;;            (string-match "^\\s-*back\\s-*$" (ad-get-arg 0)))
;;       (ad-set-arg 0 "cd -")))

;; This does not work; "(dirs)" is executed before the command is executed,
;; even if this command sleeps first.  I should create a new funtion and
;; install it on `comint-output-filter-functions' rather than modify
;; shell-directory-tracker, which is on comint-input-filter-functions.

;; (defadvice shell-directory-tracker (after handle-gcb-gnb activate)
;;   "Handle gcb and gnb commands that checkout or create a branch."
;;   (if (string-match "^\\s-*g[cn]b\\s-+" (ad-get-arg 0))
;;       (shell-resync-dirs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commenting
;;;

;; (defadvice comment-region (around strip-whitespace activate)
;;   "If `comment-start' begins with a space and `comment-padding' is 1,
;; then set `comment-padding' to nil."
;;   (let ((comment-padding
;;          (if (and (or (and (numberp comment-padding) (= 1 comment-padding))
;;                       (and (stringp comment-padding) (equal " " comment-padding)))
;;                   comment-start
;;                   (= ?\  (aref comment-start (1- (length comment-start)))))
;;              nil
;;            comment-padding)))
;;     ad-do-it))



(provide 'prog-modes-mde)

;;; prog-modes-mde.el ends here
