;;; -*- lexical-binding: t -*-

(eval-when-compile
  '(require 'etags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ediff
;;;

;;; Experimentally commented out, 2025-04-06.
;; ;;; I sent mail to Michael Kifer on Nov 10, 2017, but never got a response.
;; ;; Problem with ediff merging:  if both files use CRLF (DOS) eol-type, but
;; ;; the most-preferred coding system does not use DOS eol-type, then the
;; ;; ediff merge tool creates a non-DOS merged file!  That results in a
;; ;; needless change to every line, which creates very large diffs and
;; ;; destroys the version control history.
;; ;; The problem is in ediff-setup, where it does
;; ;;          (with-current-buffer buffer-C
;; ;;            (insert-buffer-substring buf)
;; ;; and buffer-C's eol-type is retained.
;; ;; The fix should go in or near `ediff-choose-syntax-table', which has a
;; ;; similar purpose.
;; ;; For a test case, see file ediff-merge-test.zip .
;; (defadvice ediff-choose-syntax-table (after set-eol-type activate)
;;   "Set the coding-system for the merged file."
;;   (let* ((eol-type-a (coding-system-eol-type
;;                       (with-current-buffer ediff-buffer-A
;;                         buffer-file-coding-system)))
;;          (eol-type-b (coding-system-eol-type
;;                       (with-current-buffer ediff-buffer-B
;;                         buffer-file-coding-system)))
;;          (ancestor-buffer (ediff-buffer-live-p ediff-ancestor-buffer))
;;          (eol-type-ancestor (and ancestor-buffer
;;                                  (with-current-buffer ancestor-buffer
;;                                    buffer-file-coding-system)))
;;          (coding-system-c (and ediff-buffer-C
;;                                (with-current-buffer ediff-buffer-C
;;                                  buffer-file-coding-system)))
;;          (eol-type-c (and coding-system-c
;;                           (coding-system-eol-type coding-system-c))))
;;     (if (equal eol-type-a eol-type-b)
;;         ;; The two children have the same line endings, use it.
;;         (setq eol-type-c eol-type-a)
;;       (progn
;;         ;; The two children have different line endings;
;;         ;; use the ancestor's line endings, but warn.
;;         (message "Warning: ediff is merging files with different eol-type:\n  %s %s\n  %s %s"
;;                  eol-type-a ediff-buffer-A
;;                  eol-type-b ediff-buffer-B)
;;         (setq eol-type-c eol-type-ancestor)))
;;     (if (and eol-type-c ediff-buffer-C)
;;         (with-current-buffer ediff-buffer-C
;;           (set-buffer-file-coding-system
;;            (coding-system-change-eol-conversion
;;             coding-system-c eol-type-c))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diff
;;;

(defvar diff-clean-removed-files
  nil
    "A list of regular expressions of filenames that should not be shown in diffs.
Each regexp must match the entire filename: add .* at the beginning and end as
necessary.
Do not use anchoring characters ^ and $.
In many cases, using diff's --exclude or --exclude-from is better, but those
only match basenames whereas this handles pathnames.")

;; [@BIO\ncd] is what can start a line at the end of a hunk
(defvar empty-diff-hunk-regexp-1 "^@.*@\n\\( .*\n\\)*\\(?:\\\\ No newline at end of file\n\\)?\\([@BIO\ncd]\\|\\'\\|--- \\)")
(defvar empty-diff-hunk-regexp-2 "^@@ .* @@ .*\n\\( .*\n\\)*\\(?:\\\\ No newline at end of file\n\\)?\\([@BIO\ncd]\\|\\'\\|--- \\)")
;; It is important to set case-fold-search to nil when using `empty-diff-filesection-regexp'.
(defvar empty-diff-filesection-regexp
  (concat
   "^diff.*\n\\(?:\\(?:index .*\n\\)?---.*\n\\+\\+\\+.*\n\\)?"
   "\\(diff\\|Only in \\|Binary files \\|\nDiff finished.\\|\\'\\)"))

;; TODO: This could perhaps use functions like `diff-hunk-kill'.
(defun diff-clean ()
  "Cleans up a diff to remove uninteresting changes.
Removes some files entirely (see `diff-clean-removed-files').
Removes trivial diffs, such as hunks or files with empty/no differences.
Reduces size of diffs with common prefix or suffix.
The latter two changes are semantics-preserving and are useful after
editing a diff buffer to remove uninteresting changes."
  (interactive)

  (let ((inhibit-read-only t))

    (save-excursion

      (goto-char (point-min))
      (delete-matching-lines "^\\\\ No newline at end of file$")

      ;; Remove certain files
      (goto-char (point-min))
      (let ((filename-regexp
	     (concat "\\("
		     (mapconcat #'(lambda (r) (concat "\\(" r "\\)"))
				diff-clean-removed-files
				"\\|")
		     "\\)")))
	(while (re-search-forward
		(concat "^diff .*\n\\("
			"--- " filename-regexp "\t.*\n\\+\\+\\+ .*$"
			"\\|"
			"--- .*\n\\+\\+\\+ " filename-regexp "\t.*$"
			"\\)")
		nil t)
	  (let* ((begin (match-beginning 0)))
	    (re-search-forward "\n[^-+ @]")
	    (goto-char (match-beginning 0))
	    (delete-region begin (1+ (point)))))

	(goto-char (point-min))
	(delete-matching-lines (concat "^Only in " filename-regexp "$")))

      ;; Remove lines starting "Only in " for certain files.
      ;; The "Only in " lines put ": " in place of the last "/"
      ;; directory separator, so regexp `diff-clean-removed-files'
      ;; does not match them.
      (goto-char (point-min))
      (let ((onlyin-regexp
	     (concat "^Only in \\("
		     (mapconcat #'(lambda (r) (concat "\\(" (file-regexp-to-colon-regexp r) "\\)"))
				diff-clean-removed-files
				"\\|")
		     "\\)")))
	(delete-matching-lines onlyin-regexp))

      ;; TODO: Remove "Binary files XXX and YYY differ" lines

      (let ((case-fold-search nil))

	;; Remove identical hunks.
	(goto-char (point-min))
	(while (re-search-forward "^-" nil t)
	  (let ((hunk-beginning (1- (point))))
            (re-search-forward "^[^-]")
            (backward-char 1)
            (let* ((hunk-text-negative (buffer-substring-no-properties hunk-beginning (point)))
		   (hunk-text-length (length hunk-text-negative))
		   (hunk-end (+ (point) hunk-text-length))
		   (hunk-text-positive (replace-regexp-in-string "^-" "+" hunk-text-negative)))
              (if (and (<= hunk-end (point-max))
                       (equal hunk-text-positive (buffer-substring-no-properties (point) hunk-end)))
		  (progn
		    ;; This uses interactive editing commands (eg, kill-region rather
		    ;; than delete-region) so that diff-mode updates the hunk headers.
		    (kill-region hunk-beginning hunk-end)
		    (insert (replace-regexp-in-string "^-" " " hunk-text-negative)))))))

	;; First two lines are identical (one -, one +).
	(goto-char (point-min))
	(replace-regexp-noninteractive "^-\\(.*\\)\n\\+\\1\\.?\n" " \\1\n")
	;; First - line is identical to first + line
	(goto-char (point-min))
	(replace-regexp-noninteractive "^-\\(.*\\)\n\\(\\(-.*\n\\)+\\)\\+\\1\\.?\n" " \\1\n\\2")
	;; Last - line is identical to last + line
	(goto-char (point-min))
	(replace-regexp-noninteractive "^-\\(.*\\)\n\\(\\(\\+.*\n\\)+\\)\\+\\1\\.?\n" "\\2 \\1\n")
	;; Last - line is identical to first + line
	(goto-char (point-min))
	(replace-regexp-noninteractive "^-\\(.*\\)\n-\\(.*\n\\)\\+\\1\\.?\n" " \\1\n-\\2")
	;; Needs to be tested before uncommenting
	;; (goto-char (point-min))
	;; (query-replace-regexp "^-\\(.*\\)\n\\+\\(.*\n\\)\\+\\1\\.?\n" " \\2+\\1\n")

	;; ;; Remove identical lines with one different one between them.
	;; (goto-char (point-min))
	;; (replace-regexp-noninteractive "^-\\(.*\\)\n\\([-+].*\n\\)\\+\\1\\.?\n" "\\2 \\1\n")

        ;;; Should do the same as the above, with any number of different lines between them.

	;; Remove empty parts of the file: empty hunks and empty file sections.
	(replace-all-occurrrences-iteratively empty-diff-hunk-regexp-1 "\\2")
	(replace-all-occurrrences-iteratively empty-diff-hunk-regexp-2 "\\2")
	(replace-all-occurrrences-iteratively empty-diff-filesection-regexp "\\1")
	)

      )))

(defun diff-clean-imports ()
  "Cleans up a diff to remove changes in import statements.
Removes some files entirely (see `diff-clean-removed-files').
Removes trivial diffs, such as hunks or files with empty/no differences.
Reduces size of diffs with common prefix or suffix.
The latter two changes are semantics-preserving and are useful after
editing a diff buffer to remove uninteresting changes."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      ;; Remove certain files
      (goto-char (point-min))
      (while (re-search-forward "^[-+]import.*;$" nil t)
	(goto-char (match-beginning 0))
	(kill-line))))
  (diff-clean))

;; This name may need to be changed, so that completing "diff-clean" is easier to do.
(defun diff-clean-target ()
  "Like `diff-clean', but also ignores generated files."
  (interactive)
  (let ((diff-clean-removed-files
	 (append diff-clean-removed-files
		 '(".*/target/.*"))))
    (diff-clean)))

;; This name may need to be changed, so that completing "diff-clean" is easier to do.
(defun diff-clean-build ()
  "Like `diff-clean', but also ignores generated files."
  (interactive)
  (let ((diff-clean-removed-files
	 (append diff-clean-removed-files
		 '(".*/build/.*"))))
    (diff-clean)))

(defun diff-clean-javadoc ()
  "Remove Javadoc files from a diff."
  (interactive)
  (let ((diff-clean-removed-files '(".*/docs/api/.*")))
    (diff-clean)))

(defun diff-clean-backup ()
  "Remove backup files from a diff."
  (interactive)
  (let ((diff-clean-removed-files '(".*~$")))
    (diff-clean)))

(defun delete-non-matching-hunks (regexp)
  "Delete hunks that do not contain a match for the given regexp."
  (interactive)
  (save-excursion
    (while (re-search-forward "^@@ " nil t)
      (let ((hunk-start (match-beginning 0)))
	(re-search-forward "^[^-+ ]\\|\\'")
	(let ((hunk-end (match-beginning 0)))
	  (goto-char hunk-start)
	  (if (re-search-forward regexp hunk-end t)
	      (goto-char hunk-end)
	    (delete-region hunk-start hunk-end)))))))

(defun file-regexp-to-colon-regexp (regexp)
  "Change the last slash in `regexp` to \": \"."
  (let ((result (replace-regexp-in-string "\\(/\\)[^/]*$" ": " regexp nil nil 1)))
    (if (string-suffix-p "/.*" regexp)
	(let ((additional (file-regexp-to-colon-regexp (substring regexp 0 (- (length regexp) 3)))))
	  (concat "\\(" result "\\)\\|\\(" additional "\\)"))
      result)))
;; (file-regexp-to-colon-regexp ".*/defects4j[^/]*/framework/test/d4j.log")
;; (file-regexp-to-colon-regexp ".*/daikon[^/]*/utils/.*")
;; (file-regexp-to-colon-regexp	".*/logging-log4j2.*/target/.*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version control conflicts
;;;

;;; Typical workflow:
;; (eval-buffer)
;; (read-conflict-files-from-tags-table)
;; (resolve-import-conflicts)
;; (cf-imports-whitespace-cleanup)
;; (tags-search "^<<<<<<")   ;; Fix manually, or write code to handle
;; ;; (sort-cf-imports) ;; Takes too long.  Do it periodically on master.
;; (tags-search "^<<<<<<")   ;; Run vc-resolve-conflicts for each file

(defun read-conflict-files-from-tags-table ()
  "Reald all the files in the tags table into their own buffers."
  (interactive)
  (tags-search "^<<<<<<")
  (while t
    (fileloop-continue)))


(defun read-all-files-from-tags-table ()
  "Reald all the files in the tags table into their own buffers.
This takes up a ridiculous amount of Emacs memory, for large TAGS tables."
  (interactive)
  (tags-search "\\`[^z]")
  (while t
    (fileloop-continue)))


;; This is superseded by merge-java-imports-driver.sh .
(defun resolve-import-conflicts ()
  "Resolve conflicts that invove only import lines.
Two caveats:
1. You may have to adjust whitespace at the beginning and end manually.
2. The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; (read-conflict-files-from-tags-table)
  (tags-search
   "<<<<<<< HEAD
\\(\\(?:\\(?:import .*;\\)?\n\\)*\\)||||||| merged common ancestors
\\(\\(?:\\(?:import .*;\\)?\n\\)*\\)=======
\\(\\(?:\\(?:import .*;\\)?\n\\)*\\)>>>>>>> [0-9a-f]\\{40\\}
")
  (while t
    ;; (message "#1 %s" (match-string 1))
    ;; (message "#3 %s" (match-string 3))
    (replace-match (sorted-non-duplicate-lines (match-string 1) (match-string 3)))
    (fileloop-continue))
  ;; TODO: does not get run because previous loop throws an exception
  ;; (cf-imports-whitespace-cleanup)
  )

(defun resolve-annotatedfor-conflicts ()
  "Resolve conflicts that invove only @AnnotatedFor lines.
A caveat:
The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; (read-conflict-files-from-tags-table)
  (tags-search
   "<<<<<<< HEAD
\\(?:@AnnotatedFor(\\(.*\\))\n\\)?||||||| merged common ancestors
\\(?:@AnnotatedFor(\\(.*\\))\n\\)?=======
\\(?:@AnnotatedFor(\\(.*\\))\n\\)?>>>>>>> [0-9a-f]\\{40\\}
")
  (while t
    ;; (message "#1 %s" (match-string 1))
    ;; (message "#2 %s" (match-string 1))
    ;; (message "#3 %s" (match-string 3))
    (replace-match (merged-annotated-for (remove-text-properties-string (match-string 1)) (remove-text-properties-string (match-string 3))))
    (fileloop-continue))
  ;; TODO: does not get run because previous loop throws an exception
  )

(defun remove-text-properties-string (s)
  (set-text-properties 0 (length s) nil s)
  s)

(defun merged-annotated-for (annotatedfor-arg-1 annotatedfor-arg-2)
  "Merge two @AnnotatedFor annotation arguments into one @AnnotatedFor annotation."
  (save-match-data
  (let* ((args1 (parse-annotatedfor-argument annotatedfor-arg-1))
	 (args2 (parse-annotatedfor-argument annotatedfor-arg-2))
	 (args (sort (delete-dups (append args1 args2)))))
    (concat "@AnnotatedFor({\""
	    (mapconcat #'identity args "\", \"")
	    "\"})\n"))))
;; (merged-annotated-for "{\"signature\", \"nullness\" ,\"interning\"}" "{\"propkey\", \"signature\"")

(defun parse-annotatedfor-argument (arg)
  "Given an argument to @AnnotatedFor, return a list of the string arguments."
  (split-string arg "\" *, *\"" 'omit-separators "[ {}\"]*"))
;; (cl-assert (equal '("signature") (parse-annotatedfor-argument "{\"signature\"}")))
;; (cl-assert (equal '("signature") (parse-annotatedfor-argument " \"signature\"  "))
;; (cl-assert (equal '("signature" "nullness" "interning") (parse-annotatedfor-argument "{\"signature\", \"nullness\" ,\"interning\"}")



(defun resolve-annotatedfor-usesobjectequals-conflicts ()
  "Resolve conflicts that invove only @AnnotatedFor and @UsesObjectEquals.
A caveat:
The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; (read-conflict-files-from-tags-table)
  (tags-search
   "<<<<<<< HEAD
\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\(.*\\)class \\(.*\\)\n||||||| merged common ancestors
\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\2class \\3\n=======
\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\(\\2\\(?:@UsesObjectEquals \\|@Interned \\)*class \\3\n\\)>>>>>>> [0-9a-f]\\{40\\}
")
  (while t
    ;; (message "#1 %s" (match-string 1))
    ;; (message "#2 %s" (match-string 1))
    ;; (message "#3 %s" (match-string 3))
    (message "#6 %s" (match-string 6))
    (replace-match
     (concat (merged-annotated-for (remove-text-properties-string (match-string 1)) (remove-text-properties-string (match-string 5)))
	     (match-string 6)))
    (fileloop-continue))
  ;; TODO: does not get run because previous loop throws an exception
  )




;; (tags-query-replace "<<<<<<< HEAD
;; @AnnotatedFor({?\"interning\"}?)
;; ||||||| merged common ancestors
;; =======
;; @AnnotatedFor({\"lock\"})
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; " "@AnnotatedFor({\"interning\", \"lock\"})
;; ")



(defun resolve-one-annotatedfor-conflict (annotatedfor-arg-1 annotatedfor-arg-2 annotatedfor-arg-combined)
  (tags-query-replace
   (format "<<<<<<< HEAD
@AnnotatedFor({?%s}?)
public\\(.*\\) @UsesObjectEquals class \\(.*\\)
||||||| merged common ancestors
public\\1 class \\2
=======
@AnnotatedFor({?%s}?)
public\\1 class \\2
>>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
" annotatedfor-arg-1 annotatedfor-arg-2)
   (format
    "@AnnotatedFor({%s})
public\\1 @UsesObjectEquals class \\2
" annotatedfor-arg-combined)))




(defun sorted-non-duplicate-lines (lines1 lines2)
  "Return a string consisting of the unique lines in the two input strings.
In the result, the lines are sorted."
    (with-temp-buffer "*sorted-non-duplicate-lines*"
      (insert lines1)
      (insert lines2)
      (delete-duplicate-lines (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (buffer-string)))
;; (sorted-non-duplicate-lines "a\nc\nd\n" "d\ne\nb\nd\n")


;; Help for merge conflicts:
;; 
;; (tags-query-replace "<<<<<<< HEAD
;; \\([^|]*\\)||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; \\([^|]*\\)||||||||| merged common ancestors
;; =========
;; =======
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; " "\\1\\2")
;; 
;; (tags-query-replace "<<<<<<< HEAD
;; \\([^|]*\\)||||||| merged common ancestors
;; >>>>>>>>> Temporary merge branch 2
;; =======
;; 
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; " "\\1")
;; 
;; (tags-query-replace "<<<<<<< HEAD
;; 
;; ||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; =======
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; " "")
;; 
;; (tags-query-replace "<<<<<<< HEAD
;; \\([^|]*\\)||||||| merged common ancestors
;; >>>>>>>>> Temporary merge branch [0-9]
;; =======
;; \\([^>]*\\)>>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; " "\\1\\2")
;; 
;; (tags-query-replace "<<<<<<< HEAD
;; \\([^|]*\\)||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; \\([^|]*\\)||||||||| merged common ancestors
;; \\([^>]*\\)=========
;; =======
;; \\([^>]*\\)>>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; " "\\1\\2\\3\4")
;; 
;; 
;; 
;; (tags-query-replace "<<<<<<< HEAD
;; @AnnotatedFor({?\"interning\"}?)
;; ||||||| merged common ancestors
;; =======
;; @AnnotatedFor({\"lock\"})
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; " "@AnnotatedFor({\"interning\", \"lock\"})
;; ")
;; 
;; \\(\\(\\(?:import .*;\\)?
;; \\)*\\)"
;;
;; (tags-query-replace "<<<<<<< HEAD
;; @AnnotatedFor({\"interning\"})
;; public\\(.*\\) @UsesObjectEquals class \\(.*\\)
;; ||||||| merged common ancestors
;; public\\1 class \\2
;; =======
;; @AnnotatedFor({\"lock\"})
;; public\\1 class \\2
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; " "@AnnotatedFor({\"interning\", \"lock\"})
;; public\\1 @UsesObjectEquals class \\2
;; ")
;; 
;; (tags-query-replace "<<<<<<< HEAD
;; @AnnotatedFor({\"formatter\", \"i18n\"})
;; public\\(.*\\) @UsesObjectEquals class \\(.*\\)
;; ||||||| merged common ancestors
;; public\\1 class \\2
;; =======
;; @AnnotatedFor({\"lock\"})
;; public\\1 class \\2
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; " "@AnnotatedFor({\"formatter\", \"i18n\", \"lock\"})
;; public\\1 @UsesObjectEquals class \\2
;; ")

(provide 'file-comparison)
