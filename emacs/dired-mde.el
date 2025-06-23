;;; -*- lexical-binding: t -*-

;;; dired-mde.el -- Mike's dired setup; mde-dired-load-hook (only) loads this
;;; Michael Ernst <mernst@cs.washington.edu>

;;; Commentary:
;; (none)

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'dired)
  (require 'dired-aux)
  (require 'dired-x))
(require 'dired)
(require 'dired-aux)
(require 'dired-x)

(setq dired-recursive-copies 'top)	; ask for each directory at top level

(setq-default dired-omit-mode t)

(put 'dired-find-alternate-file 'disabled nil)

(setq dired-isearch-filenames 'dwim)

;; problem with autoload:  dired-readin-hook called before dired-mode-hook.
;; (autoload 'dired-extra-startup "dired-extra")

;; (setq dired-listing-switches "-alF")	; F for trailing /, @, and *.
;; "-v" option avoids bizarre sort order in GNU ls 4.1; only add if supported.
(if (= 0
       ;; Setting default-directory prevents this from failing when that
       ;; is set to a non-existent directory
       (let ((default-directory "/"))
	 (call-process "ls" nil nil nil "-v" (getenv "HOME"))))
    (setq dired-listing-switches (concat dired-listing-switches "v")))
(setq dired-find-subdir t)

;; Don't create new marks on hard and soft links or on copies.
(setq dired-keep-marker-hardlink nil
      dired-keep-marker-symlink nil
      dired-keep-marker-copy nil)
;; Do preserve the time on copies.
(setq dired-copy-preserve-time t)

;; Use other visible dired buffer's directory as target for copy, rename, etc.
(setq dired-dwim-target t)

;; "Z" uncompresses file.jar into directory file-jar
(push '("\\.jar\\'" "-jar" "unzip -o -d %o %i")
      dired-compress-file-suffixes)


;;;
;;; Key mappings
;;;

(define-key dired-mode-map "1" 'dired-find-file-one-window) ; default: digit-argument
;; (define-key dired-mode-map "\M-r" 'dired-rmail)

;; TODO: This doesn't respect marks, but always operates on the file at point.
(define-key dired-mode-map "b" 'browse-url-of-dired-file)

;; This favorite of mine gets clobbered.
(define-key dired-mode-map "\e\C-u" 'bury-or-raise-buffer)
(define-key dired-mode-map "\M-o" 'dired-omit-mode)

(defun dired-find-file-one-window ()
  "Find the current file in one window."
  (interactive)
  (dired-find-file)
  (delete-other-windows))


;;;
;;; Omitting
;;;

;; The default is "^#\\|^\\.$\\|^\\.\\.$" rather than "^#\\|^\\.\\.?$".  Why?
(defvar dired-omit-regexps-default
  '("^#"
    "^\\.$"
    "^\\.\\.$"
    "^\\.Z[0-9]+.*\\.tex$"		; slatex temporary files
    "^\\.latexrun\\.db$"
    "^paper.synctex(busy)$"
    "^\\.fls$"			; pdflatex temporary files, I think
    "^\\.h\\(aux\\|toc\\)$"		; hevea temporary files
    "^\\.#.+\\.[0-9]+\\.[0-9]+$"	; CVS temporary files
    "^\\.#"				; symbolic links (for multi-user??)
    "^R[emqsx][ab][0-9][0-9][0-9][0-9][0-9]$" ; appear in /tmp/
    "^\\.maildrop-junk\\(-weekly\\)?$"
    "^System Volume Information$"
    "^AdobeFnt.lst$"		; acroread file
    ))
(setq-default dired-omit-regexps dired-omit-regexps-default)
(setq dired-omit-files
      (mapconcat (function identity) dired-omit-regexps-default "\\|"))

(add-hook 'dired-mode-hook 'show-poundfiles-in-mail-dir)
(defun show-poundfiles-in-mail-dir ()
  "Resets `dired-omit-files'."
  (if (string-match "/Mail/" dired-directory)
      (progn
	(make-local-variable 'dired-omit-files)
	(setq dired-omit-files
	      (mapconcat (function identity)
			 (remove "^#" dired-omit-regexps-default)
			 "\\|")))))

;; dired-omit-extensions is defined (and set) in dired-x.el
;; there must be a character before anything in dired-omit-extensions
(defvar dired-omit-extensions-default
  (cl-delete-duplicates (sort (append
                               ;; do omit these
			       '(".backup" ".elc"
			         ".fdb_latexmk"
			         ".synctex.gz"
			         ".ilg" ".pyc" ".pyo")
			       (cl-set-difference dired-omit-extensions
                                                  ;; don't omit these
					          '(".log" ".otl" ".pdf" ".PDF")
					          :test #'equal)))
		        :test (function equal)))
(setq-default dired-omit-extensions dired-omit-extensions-default)

(setq dired-omit-size-limit 30000)	; default 20000; my emacs dir is bigger


;;;
;;; dired-shell-guesses
;;;

(defvar run-office-program (concat office-program " * &")
  "A command to run `office-program'.")

;; Variable `my-dired-shell-guesses' is used by function `dired-guess-default'.
(defvar my-dired-shell-guesses
  '(("\\.e?ps$" "gv * &" "xv" "lpr")	; ghostview runs in background
    ("\\.E?PS$" "gv * &" "xv" "lpr")	; uppercase
    ("\\.jpg$" "xloadimage -shrink")		; uppercase
    ("\\.JPG$" "xloadimage -shrink")		; uppercase
    ("\\.pdf$" "papers * &" "evince * &" "acroread * &" "xpdf * &" "gv * &")	; add acroread, gv
    ("\\.PDF$" "papers * &" "evince * &" "acroread * &" "xpdf * &" "gv * &")	; uppercase
    ("\\.mp4$" "vlc")			; not in background
    ("\\.tex\\'" "pdflatex")		; default: ("\\.tex\\'" "latex" "tex")
    ("\\.tgz$" "tar xvfz")
    ;; Microsoft formats
    ("\\.docx?$" run-office-program)
    ("\\.pptx?$" run-office-program)
    ("\\.ppsx?$" run-office-program)
    ("\\.xlsx?$" run-office-program)
    ;; OpenOffice formats
    ("\\.sxi$" run-office-program)
    ("\\.sxw$" run-office-program)
    ("\\.rtf$" run-office-program)
    ("\\.odp$" run-office-program)
    ("\\.ods$" run-office-program)
    ("\\.odt$" run-office-program)
    ))

(if (equal 'darwin system-type)
    (progn
      (setq my-dired-shell-guesses
	    (mapcar #'(lambda (extension-and-guesses)
		        (cons (car extension-and-guesses)
			      (cons "open"
				    (cdr extension-and-guesses))))
		    my-dired-shell-guesses))
      (setq my-dired-shell-guesses
	    (append
	     '(
	       ("\\.pdf$" "/Applications/Skim.app/Contents/MacOS/Skim * &" "open")
	       ("\\.pdf$" "/Applications/Skim.app/Contents/MacOS/Skim * &" "open")	; uppercase
	       )
	     my-dired-shell-guesses))))
(setq dired-guess-shell-alist-user
      (append my-dired-shell-guesses
	      dired-guess-shell-alist-user))

;; Debugging output
;; (message "dired-guess-shell-alist-user: %s" dired-guess-shell-alist-user)

(setq dired-guess-shell-gnutar "tar")	; have GNU tar, which supports -z option


;;;
;;; dired-jump
;;;

;; Not needed any longer as of 2024-04-06.
;; ;; There's no key to bind this to, as archive-subfile-mode doesn't have
;; ;; a keymap of its own.
;; (defun archive-jump ()
;;   "Like `dired-jump', but for buffers in `archive-subfile-mode'."
;;   (interactive)
;;   (eval-when-compile (require 'arc-mode)) ; file is NOT named archive-mode
;;   ;; archive-subfile-mode is a vector of the form
;;   ;; [EXT-FILE-NAME INT-FILE-NAME CASE-FIDDLED MODE ...]
;;   (let* ((file buffer-file-name)
;; 	 (ext-file-name (elt archive-subfile-mode 0))
;; 	 (arc-file (and file
;; 			(equal (concat ":" ext-file-name)
;; 			       (substring file (- (1+ (length ext-file-name)))))
;; 			(substring file 0 (- (1+ (length ext-file-name)))))))
;;     (if arc-file
;; 	(find-file arc-file)
;;       (error "Can't figure out archive file"))
;;     ;; Now go to the correct line.
;;     (goto-char (point-min))
;;     (search-forward (concat "  " ext-file-name "\n"))
;;     (goto-char (+ 2 (match-beginning 0)))))
;; 
;; (defadvice dired-jump (around dired-jump-archive-jump activate)
;;   "If in `archive-subfile-mode', use `archive-jump'."
;;   (if (and (boundp 'archive-subfile-mode) archive-subfile-mode)
;;       (archive-jump)
;;     ad-do-it))
;; 
;; (defadvice dired-jump (before dired-jump-vm-summary-mode activate)
;;   "Make `dired-jump' work even in the VM summary mode."
;;   (if (eq major-mode 'vm-summary-mode)
;;       (vm-select-folder-buffer)))


;;;
;;; Everything else
;;;


;;; Experimentally commented out, 2025-04-06.
;; ;; Perhaps this is doable as an advice, but it would be gross.
;; ;; When the argument is a pure prefix argument (just C-u), then
;; ;; add "-R" to the command's arguments.
;; ;; I should submit a cleaner patch to FSF.
;; (with-eval-after-load "dired-aux"
;;   (defun dired-do-chxxx (attribute-name program op-symbol arg)
;;      "Redefined by MDE to add -R if pure prefix argument."
;;      ;; Change file attributes (mode, group, owner) of marked files and
;;      ;; refresh their file lines.
;;      ;; ATTRIBUTE-NAME is a string describing the attribute to the user.
;;      ;; PROGRAM is the program used to change the attribute.
;;      ;; OP-SYMBOL is the type of operation (for use in dired-mark-pop-up).
;;      ;; ARG describes which files to use, as in dired-get-marked-files.
;;      (let* ((files (dired-get-marked-files t (and (not (equal arg '(4))) arg)))
;; 	    (new-attribute
;; 	     (dired-mark-read-string
;; 	      (concat "Change " attribute-name " of %s to: ")
;; 	      nil op-symbol arg files))
;; 	    (operation (concat program " " new-attribute))
;; 	    failures)
;;        (setq failures
;; 	     (dired-bunch-files 10000
;; 				(function dired-check-process)
;; 				(append
;; 				 (if (equal arg '(4))
;; 				     (list operation program "-R" new-attribute)
;; 				   (list operation program new-attribute))
;; 				 (if (string-match "gnu" system-configuration)
;; 				     '("--") nil))
;; 				files))
;;        (dired-do-redisplay arg);; moves point if ARG is an integer
;;        (if failures
;; 	   (dired-log-summary
;; 	    (format "%s: error" operation)
;; 	    nil)))))


(defun dired-do-shell-command--point-not-on-marked-file (_command &optional _arg file-list)
  "Warn if just one file is marked, but point is not on that mark.
It's too easy to make a mistake when running a shell command and believing that
it is being applied to the file under point."
  (if (= 1 (length file-list))
      (let ((action-file (car file-list))
            ;; `dired-filename-at-point' (from dired-x) can't handle spaces in filename.
	    (file-at-point (dired-get-filename 'no-directory)))
	(if (not (string-equal action-file file-at-point))
	    (error "Applying shell command to one file %s, but that file is not under point (%s is)" action-file file-at-point)))))
(advice-add 'dired-do-shell-command :before #'dired-do-shell-command--point-not-on-marked-file)



(defun dired-unmark-files-regexp (regex)
  "Unmark all files matching REGEX."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (dired-unmark 1))))

(defun find-grep-dired-and-replace (dir old-re new)
  "Run `find-grep-dired' then `dired-do-query-replace-regexp'."
  (interactive)
  (find-grep-dired dir old-re)
  ;; Need to wait for find-grep-dired to complete
  (y-or-n-p "Wait until find-grep completes, then press y ")
  (dired-mark-files-regexp "." nil)
  (dired-unmark-files-regexp "~$")
  (dired-unmark-files-regexp "\.pdf$")
  (dired-unmark-files-regexp "\.pyo$")
  (dired-unmark-files-regexp "\.zip$")
  (dired-unmark-files-regexp "\.hg/")
  (dired-do-query-replace-regexp old-re new nil))
;; Example invocations:
;; (find-grep-dired-and-replace "~/class/331/13sp/" "/cse/www/education/courses/" "/cse/web/courses/")
;; (find-grep-dired-and-replace "~/class/331/13sp/" "http://www.cs.washington.edu/education/courses/" "http://courses.cs.washington.edu/courses/")


(defun dired-mark-read-file-name--delete-symlink-target (orig-function prompt dir op-symbol arg files &optional default)
  "When symlinking, offer to replace an existing link (not make a link inside it)."
  (let ((return-value (funcall orig-function prompt dir op-symbol arg files default)))
    ;; For a filename ending in "/", file-symlink-p returns false and
    ;; delete-file fails.
    (if (string-suffix-p "/" return-value)
        (setq return-value (substring return-value 0 -1)))
    (if (and (eq op-symbol 'relsymlink)
             (= 1 (length files))
	     (file-symlink-p return-value)
             ;; file-exists-p returns false if it's a symlink to a deleted directory.
	     (or (not (file-exists-p return-value))
	         (file-directory-p return-value))
	     (y-or-n-p "Replace existing link? (n means make link inside it)"))
        (delete-file return-value))
    return-value))
(advice-add 'dired-mark-read-file-name :around #'dired-mark-read-file-name--delete-symlink-target)


(defun dired--insert-disk-space (beg _dir)
  "Don't show available disk space in dired output."
  beg)

(provide 'dired-mde)

;;; dired-mde.el ends here
