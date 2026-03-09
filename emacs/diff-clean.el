;;; -*- lexical-binding: t -*-

;; This file contains functions that simplify diffs.
;; Also see file conflict-resolve.el, which is for version control conflicts (not diffs).

;; Typical workflow for simplifying diffs:
;; (diff-clean)
;; and optionally see other functions.


(eval-when-compile
  (require 'etags)
  (require 'util-mde))

(autoload 'replace-all-occurrrences-iteratively "util-mde")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diff-clean
;;;

;; diff-clean simplifies a diff file.  It does nothing to the
;; underlying files.  It does not operate on a file containing merge
;; conflicts.


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
(defun diff-clean (&optional dont-remove-files)
  "Cleans up a diff to remove uninteresting changes.
Removes some files entirely (see `diff-clean-removed-files').
Removes trivial diffs, such as hunks or files with empty/no differences.
Reduces size of diffs with common prefix or suffix.
The latter two changes are semantics-preserving and are useful after
editing a diff buffer to remove uninteresting changes."
  (interactive)

  (let ((inhibit-read-only t))

    (if (not dont-remove-files)
        (diff-clean-files diff-clean-removed-files))

    (diff-clean-meaning-preserving)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remove whole files
;;;

(defun diff-clean-files (remove-regexes)
  "Delete files whose pathname matches any of the regexes."
  (save-excursion

    ;; (goto-char (point-min))
    ;; (delete-matching-lines "^\\\\ No newline at end of file$")

    ;; Remove certain files
    (goto-char (point-min))
    (let ((filename-regexp
	   (concat "\\("
		   (mapconcat #'(lambda (r) (concat "\\(" r "\\)"))
			      remove-regexes
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
	  (kill-region begin (1+ (point)))))

      (goto-char (point-min))
      (kill-matching-lines (concat "^Only in " filename-regexp "$")))

    ;; Remove lines starting "Only in " for certain files.
    ;; The "Only in " lines put ": " in place of the last "/"
    ;; directory separator, so regexp `remove-regexes'
    ;; does not match them.
    (goto-char (point-min))
    (let ((onlyin-regexp
	   (concat "^Only in \\("
		   (mapconcat #'(lambda (r) (concat "\\(" (file-regexp-to-colon-regexp r) "\\)"))
			      remove-regexes
			      "\\|")
		   "\\)")))
      (kill-matching-lines onlyin-regexp))

    ;; TODO: Remove "Binary files XXX and YYY differ" lines

    ))

;; These names may need to be changed, so that completing "diff-clean" is easier to do.

(defun diff-clean-more-files (regex)
  "Like `diff-clean', but ignores additional files as well.
The regex matches the whole filename. It must not start with ^ nor end with $."
  (interactive "sRegex for whole filename (no ^$): ")
  (diff-clean-files (list regex))
  (diff-clean))

(defun diff-clean-only-files (regex)
  "Like `diff-clean', but only does the specified files.
The regex matches the whole filename. It must not start with ^ nor end with $."
  (interactive "sRegex for whole filename (no ^$): ")
  (diff-clean-files (list regex))
  (diff-clean 'dont-remove-files))

(defun diff-clean-target ()
  "Like `diff-clean', but also ignores generated files."
  (interactive)
  (diff-clean-more-files ".*/target/.*"))

;; This name may need to be changed, so that completing "diff-clean" is easier to do.
(defun diff-clean-build ()
  "Like `diff-clean', but also removes generated files."
  (interactive)
  (diff-clean-more-files ".*/build/.*"))

(defun diff-clean-backup ()
  "Remove backup files from a diff."
  (interactive)
  (diff-clean-more-files ".*~"))

(defun diff-clean-javadoc ()
  "Like `diff-clean', but also removes Javadoc files."
  (interactive)
  (diff-clean-more-files ".*/docs/api/.*"))

(defun diff-clean-json ()
  "Like `diff-clean', but also removes JSON files."
  (interactive)
  (diff-clean-more-files ".*\\.json"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meaning-preserving transformations
;;;

;; These transformations are never applicable to a diff created by the
;; diff program, but other transformations may make them applicable.

(defun diff-clean-meaning-preserving ()
  "Perform meaning-preserving simplifications on the diff."
  (interactive)

  (diff-realign-hunks)
  (diff-concatenate-hunks)
  
  (diff-clean-prefix-suffix)
  (diff-clean-empty-parts)
  )


(defun diff-clean-prefix-suffix ()
  "Simplify a diff when a hunk has identical prefix or suffix for - and +."
  (interactive)

  (save-excursion
    ;; First two lines are identical (one -, one +).
    (goto-char (point-min))
    (replace-all-occurrrences-iteratively "^-\\(.*\\)\n\\+\\1\\.?\n" " \\1\n")
    ;; First - line is identical to first + line
    (goto-char (point-min))
    (replace-all-occurrrences-iteratively "^-\\(.*\\)\n\\(\\(-.*\n\\)+\\)\\+\\1\\.?\n" " \\1\n\\2")
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

    ;; Should do the same as the above, with any number of different lines between them.
    ))

(defun diff-clean-empty-parts ()
  "Remove empty parts of the file: empty hunks and empty file sections."
  (interactive)
  (replace-all-occurrrences-iteratively empty-diff-hunk-regexp-1 "\\2")
  (replace-all-occurrrences-iteratively empty-diff-hunk-regexp-2 "\\2")
  (replace-all-occurrrences-iteratively empty-diff-filesection-regexp "\\1"))


(defun diff-realign-hunks ()
  ;; TODO: also do the reverse, moving lines from beginning to end of hunk.
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[^-+].*\n\\(\\(?:[+].*\n\\)+\\|\\(?:[-].*\n\\)+\\)[^-+]" nil t)
      (let* ((change-begin (match-beginning 1))
             (change-end (match-end 1))
             (indicator-char (buffer-substring change-begin (1+ change-begin)))
             (indicator-char-at-bol (concat "^[" indicator-char "]"))
             (indicator-char-line (concat indicator-char-at-bol "$")))
        (goto-char change-end)
        (when (re-search-backward indicator-char-line change-begin t)
          ;; Found a blank line in the added text.
          (let* ((moved-text-begin (1+ (point)))
                 (moved-text-end change-end)
                 (moved-text-length (- moved-text-end moved-text-begin))
                 (moved-text (buffer-substring moved-text-begin moved-text-end))
                 (moved-text-without-plus (replace-regexp-in-string indicator-char-at-bol " " moved-text))
                 (candidate-start (- change-begin moved-text-length))
                 (candidate-end change-begin))
            (when (and
                   (>= candidate-start 0)
                   (string= moved-text-without-plus (buffer-substring candidate-start candidate-end)))
              (change-indicator-char-in-region " " indicator-char candidate-start candidate-end)
              (change-indicator-char-in-region indicator-char " " moved-text-begin moved-text-end))))
        (goto-char change-end)))))

(defun diff-concatenate-hunks ()
  "Merges two hunks that are separated only by punctuation."
  (diff-concatenate-hunks-with-indicator "+")
  (diff-concatenate-hunks-with-indicator "-")
  )

(defun diff-concatenate-hunks-with-indicator (indicator-char)
  "Merges two hunks that are separated only by punctuation.
`indicator-char' is '+' or '-'."
  (save-excursion
    (goto-char (point-min))
    (let ((regex (concat "^[^" indicator-char "].*\n"
                         "\\(\\(?:[" indicator-char "].*\n\\)+\\)"
                         "\\(?: [][(){}* \t\n\r]*\n\\)+"
                         "\\(\\(?:[" indicator-char "].*\n\\)+\\)"
                         "[^" indicator-char "]")))
      (while (re-search-forward regex nil t)
        ;; These two `goto-char` are for debugging; remove them.
        (goto-char (match-beginning 0))
        (goto-char (match-beginning 1))
        (let* ((change1-begin (match-beginning 1))
               (punctuation-begin (match-end 1))
               (punctuation-end (match-beginning 2))
               (change2-end (match-end 1))
               (punctuation-length (- punctuation-end punctuation-begin))
               (punctuation (buffer-substring punctuation-begin punctuation-end)))
          (change-indicator-char-in-region
           " " indicator-char punctuation-begin punctuation-end)
          (cond ((equal "+" indicator-char)
                 (goto-char change1-begin)
                 (insert punctuation)
                 (change-indicator-char-in-region
                  " " "-" change1-begin (+ change1-begin punctuation-length)))
                ((equal "-" indicator-char)
                 (goto-char change2-end)
                 (insert punctuation)
                 (change-indicator-char-in-region
                  " " "-" change2-end (+ change2-end punctuation-length)))
                (t
                 (error "bad indicator character '%s'" indicator-char)))
          (goto-char change1-begin)
          (forward-line -1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-meaning-preserving transformations
;;;


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
      (while (re-search-forward "^[-+]\\(import.*;\\|from .* import .*\\)$" nil t)
	(goto-char (match-beginning 0))
	(kill-line))))
  (diff-clean))


;; TODO: also define kill-matching-hunks, which can share a lot of code with this.
(defun kill-non-matching-hunks (regexp)
  "Delete hunks that do not contain a match for the given regexp."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@@ " nil t)
      (let ((hunk-start (match-beginning 0)))
	(re-search-forward "^[^-+ ]\\|\\'")
	(let ((hunk-end (match-beginning 0)))
	  (goto-char hunk-start)
	  (if (re-search-forward regexp hunk-end t)
	      (goto-char hunk-end)
	    (kill-region hunk-start hunk-end)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

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

(defun opposite-indicator-char (indicator-char)
  "Given \"-\", returns \"+\".  Given \"+\", returns \"-\"."
  (cond ((equal "-" indicator-char)
         "+")
        ((equal "+" indicator-char)
         "-")
        (t
         (error (concat "bad indicator char: " indicator-char)))))

(defun change-indicator-char-in-region (old-indicator new-indicator begin end)
  "Replaces the indicator character (the character in column 1), in the region.
Does nothing with lines that do not begin with `old-indicator'."
  (replace-regexp-in-region
   (concat "^[" old-indicator "]")
   new-indicator
   begin
   end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General refactoring
;;;

;; This was necessary once in the Checker Framework annotated JDK.
(defun standardize-array-declarations ()
  "Converts Java array declarations from \"short a2[]\" to \"short[] a2\" or from
  \"@PolySigned short a2 @Nullable []\" to \"@PolySigned short @Nullable [] a2\""
  (tags-query-replace
   (concat
    "\\([^@]\\)\\b\\([A-Z][a-z][A-Za-z0-9]+\\|byte\\|short\\|int\\|long\\|float\\|double\\|boolean\\|char\\) "
    "\\([A-Za-z0-9]+\\)\\(\\( ?@[A-Za-z0-9]+ ?\\)*\\[\\]\\)")
   "\\1\\2\\4 \\3")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

(provide 'diff-clean)
