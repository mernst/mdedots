;;; -*- lexical-binding: t -*-

;; This file contains functions that simplify diffs.
;; Also see file conflict-resolve.el, which is for version control conflicts (not diffs).

;; Typical workflow for simplifying diffs:
;; (diff-clean)
;; and optionally see other functions.


(require 'subr-x)                       ; for `string-remove-suffix'

(eval-when-compile
  (require 'etags)
  (require 'util-mde))

(autoload 'replace-all-occurrrences-iteratively "util-mde")
(autoload 'diff-fixup-modifs "diff-mode")


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

;; The header of a hunk is either a line ending in "@" (as in "@@ -1,2 +1,2 @@")
;; or a line of the form "@@ ... @@ ..." whose trailing text names the enclosing
;; function.  [@BCDFIOd] is what can start a line at the end of a hunk:
;; "@" a hunk header, "B" "Binary files ", "C" "Common subdirectories: ",
;; "D" "Diff finished.", "F" "Files ", "I" "Index: ", "O" "Only in ", and
;; "d" "diff ".  A hunk can also be followed by the blank line that precedes
;; Emacs's "Diff finished." line, but any other blank line is within a hunk,
;; because editing a diff can strip the leading space from a blank context
;; line.
;; "B" and "F" are separate because diff writes "Binary files X and Y differ"
;; for a binary file, but "Files X and Y differ" (with --brief) or "Files X
;; and Y are identical" (with --report-identical-files) for a text file.
;; "D" and "d" both appear because they start two different lines, not because
;; of case.  `case-fold-search' must be nil when using this regexp, so each
;; letter matches only the case that diff writes.
(defconst diff-clean-empty-hunk-regexp
  "^@\\(?:.*@\\|@ .* @@ .*\\)\n\\( .*\n\\)*\\(?:\\\\ No newline at end of file\n\\)?\\([@BCDFIOd]\\|\nDiff finished\\.\\|\\'\\|--- \\)"
  "Matches a hunk that has no added or removed lines.
Group 2 is the text following the hunk, which must be retained.
Bind `case-fold-search' to nil when using this regexp.")

(defconst diff-clean-empty-filesection-regexp
  (concat
   "^diff.*\n"
   ;; Git's extended header lines, which precede "index" and "---", are
   ;; deliberately not matched.  A section that contains one is not empty even
   ;; if it contains no hunks: that is how git represents creating an empty
   ;; file, a deletion, a rename, a copy, or a mode change.
   ;; The "index" line is matched only within the branch that requires the
   ;; "---"/"+++" pair, because a section consisting of "diff" and "index"
   ;; alone is not empty either: that is how git represents a change to a
   ;; binary file ("diff --git", "index", "Binary files X and Y differ").
   "\\(?:\\(?:index .*\n\\)?---.*\n\\+\\+\\+.*\n\\)?"
   ;; The lines that can follow a file's section, which are the same lines that
   ;; can follow a hunk; see `diff-clean-empty-hunk-regexp'.
   "\\(diff\\|Only in \\|Binary files \\|Files \\|Common subdirectories: \\|Index: "
   "\\|\nDiff finished\\.\\|\\'\\)")
  "Matches a file's diff section that contains no hunks.
Group 1 is the text following the section, which must be retained.
Bind `case-fold-search' to nil when using this regexp.")

;; TODO: This could perhaps use functions like `diff-hunk-kill'.
(defun diff-clean (&optional dont-remove-gitignored)
  "Clean up a diff to remove uninteresting changes.
Remove the files that git ignores, and the files that match
`diff-clean-removed-files'.
Remove trivial diffs, such as hunks or files with empty/no differences.
Reduce size of diffs with common prefix or suffix.
The latter two changes are semantics-preserving and are useful after
editing a diff buffer to remove uninteresting changes.
With a prefix argument, or if DONT-REMOVE-GITIGNORED is non-nil, keep the
diffs of the files that git ignores; `diff-clean-gitignored-filenames' explains
which files those are."
  (interactive "P")

  (let ((inhibit-read-only t))

    (diff-clean-remove-files dont-remove-gitignored)

    (diff-clean-meaning-preserving)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remove whole files
;;;

(defun diff-clean-remove-files (dont-remove-gitignored &optional extra-regexps)
  "Remove the diffs of the files that `diff-clean' removes.
Those are the files that match EXTRA-REGEXPS or `diff-clean-removed-files', and
the files that git ignores.
If DONT-REMOVE-GITIGNORED is non-nil, the files that git ignores are retained."
  (diff-clean-files (append extra-regexps diff-clean-removed-files)
                    (if dont-remove-gitignored
                        '()
                      (diff-clean-gitignored-filenames))))

(defun diff-clean-files (remove-regexes &optional remove-filenames)
  "Delete files whose pathname matches any of the regexes.
Also delete files whose pathname is one of REMOVE-FILENAMES, which are
compared literally.  Testing a filename against a long list of filenames
is much faster this way than via a regexp for each filename.
Does nothing if REMOVE-REGEXES and REMOVE-FILENAMES are both nil."
  (let ((inhibit-read-only t))

    (when (or remove-regexes remove-filenames)
      (save-excursion

        ;; (goto-char (point-min))
        ;; (delete-matching-lines "^\\\\ No newline at end of file$")

        (let ((matcher (diff-clean-make-matcher remove-regexes remove-filenames))
              ;; The "Only in " lines put ": " in place of the last "/"
              ;; directory separator, so neither `remove-regexes' nor
              ;; `remove-filenames' matches them; `file-regexp-to-colon-regexp'
              ;; and `file-name-to-colon-name' adjust for that.  The adjusted
              ;; regexps are anchored at the end: otherwise ".*: build", from
              ;; ".*/build/.*", would match "proj: buildSrc".
              (colon-matcher (diff-clean-make-matcher
                              (mapcar #'file-regexp-to-colon-regexp remove-regexes)
                              (mapcar #'file-name-to-colon-name remove-filenames))))

          ;; Remove certain files.
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "^diff .*\n"
                          ;; Git's extended header lines, any number of which may
                          ;; precede the optional "index" line.
                          "\\(?:\\(?:old mode\\|new mode\\|new file mode\\|deleted file mode"
                          "\\|copy from\\|copy to\\|rename from\\|rename to"
                          "\\|similarity index\\|dissimilarity index\\) .*\n\\)*"
                          "\\(?:index .*\n\\)?"
                          "\\(?:"
                          "--- \\(.*\\)\n\\+\\+\\+ \\(.*\\)$"
                          "\\|"
                          ;; Git reports a difference in a binary file with a
                          ;; "Binary files " line in place of the "---"/"+++" pair
                          ;; and the hunks.
                          "Binary files \\(.*\\) differ$"
                          "\\)")
                  nil t)
            ;; The filenames are tested here rather than in the regexp above,
            ;; because an alternation of all of `remove-regexes' can be too
            ;; large for Emacs to compile.
            (when (if (match-beginning 3)
                      (diff-clean-differ-line-matches-p (match-string 3) matcher)
                    (or (diff-clean-diff-filename-matches-p (match-string 1) matcher)
                        (diff-clean-diff-filename-matches-p (match-string 2) matcher)))
              (let* ((begin (match-beginning 0))
                     ;; The end of the file's diff is the start of the next line
                     ;; that begins neither a diff line, a hunk header, a blank
                     ;; line, nor a "\\ No newline at end of file" marker; or end
                     ;; of buffer.  A blank line is within the diff because
                     ;; editing a diff can strip the leading space from a blank
                     ;; context line.  The character class contains a single
                     ;; backslash, doubled for Emacs string syntax; a character
                     ;; class has no escape sequences.
                     (end (if (re-search-forward "\n[^-+ @\\\\\n]" nil t)
                              (1+ (match-beginning 0))
                            (point-max))))
                (delete-region begin end)
                (beginning-of-line 0))))

          ;; A regexp that ends in ".*" can match an "Only in " line directly,
          ;; because the ".*" absorbs the ": " described above.
          (goto-char (point-min))
          (while (re-search-forward "^Only in \\(.*\\)$" nil t)
            (when (or (diff-clean-matches-p (match-string 1) matcher)
                      (diff-clean-matches-p (match-string 1) colon-matcher))
              (delete-region (match-beginning 0)
                             (min (point-max) (1+ (match-end 0))))))

          ;; Unlike git, diff writes these lines outside any file's diff section,
          ;; so the loop above does not remove them.  Diff writes "Binary files X
          ;; and Y differ" for a binary file, and, when it is run with --brief,
          ;; "Files X and Y differ" for any file.  This runs after that loop, so
          ;; that a line within a section that the loop removes is not removed on
          ;; its own, which would orphan the "diff" and "index" lines above it.
          (goto-char (point-min))
          (while (re-search-forward
                  "^\\(?:Binary files \\|Files \\)\\(.*\\) differ$" nil t)
            (when (diff-clean-differ-line-matches-p (match-string 1) matcher)
              (delete-region (match-beginning 0)
                             (min (point-max) (1+ (match-end 0)))))))))))

(defconst diff-clean-matcher-length 2000
  "The approximate maximum length of a regexp from `diff-clean-regexp-matchers'.
Emacs signals `invalid-regexp' with message \"Regular expression too big\" for
a regexp whose compiled form exceeds a fixed size, which corresponds to a
source regexp of roughly 30000 characters.  This value is far below that.")

(defconst diff-clean-matcher-overhead 8
  "The number of characters that `diff-clean-matcher' adds per element.
It wraps each element in \"\\(?:\" and \"\\)\" and separates each element from
the previous one by \"\\|\".  Without this, a list of many short regexps would
build a regexp far longer than `diff-clean-matcher-length'.")

(defun diff-clean-make-matcher (regexps &optional filenames)
  "Return a matcher for the strings that REGEXPS or FILENAMES match.
A string matches a regexp in REGEXPS if the regexp matches it entirely, and it
matches FILENAMES if it is an element of FILENAMES.
The result's car is a hash table whose keys are FILENAMES, and its cdr is a
list of regexps, as returned by `diff-clean-regexp-matchers'.
Use `diff-clean-matches-p' to test a string against the result."
  (let ((table (make-hash-table :test #'equal :size (length filenames))))
    (dolist (filename filenames)
      (puthash filename t table))
    (cons table (diff-clean-regexp-matchers regexps))))

(defun diff-clean-regexp-matchers (regexps &optional prefix-only)
  "Return a list of regexps that matches what REGEXPS matches.
Each result element is anchored at the beginning and, unless PREFIX-ONLY is
non-nil, at the end, so it matches an entire string.
The result is a list rather than a single alternation of all of REGEXPS,
because such an alternation can be too large for Emacs to compile."
  (let ((result '())
        ;; The regexps of the alternation that is being built, reversed.
        (pending '())
        (pending-length 0))
    (dolist (regexp regexps)
      (let ((regexp-length (+ (length regexp) diff-clean-matcher-overhead)))
        (when (and pending
                   (> (+ pending-length regexp-length) diff-clean-matcher-length))
          (push (diff-clean-matcher pending prefix-only) result)
          (setq pending '())
          (setq pending-length 0))
        (push regexp pending)
        (setq pending-length (+ pending-length regexp-length))))
    (when pending
      (push (diff-clean-matcher pending prefix-only) result))
    result))

(defun diff-clean-matcher (regexps prefix-only)
  "Return a regexp that matches a string that any of REGEXPS matches entirely.
The regexp is anchored at the end unless PREFIX-ONLY is non-nil."
  (concat "\\`\\(?:"
          (mapconcat #'(lambda (r) (concat "\\(?:" r "\\)")) regexps "\\|")
          "\\)"
          (if prefix-only "" "\\'")))

(defun diff-clean-matches-p (string matcher)
  "Return non-nil if MATCHER matches STRING.
MATCHER is as returned by `diff-clean-make-matcher'."
  (let ((result (gethash string (car matcher)))
        (regexps (cdr matcher)))
    (while (and regexps (not result))
      (setq result (string-match-p (car regexps) string))
      (setq regexps (cdr regexps)))
    result))

(defun diff-clean-diff-filename-matches-p (text matcher)
  "Return non-nil if MATCHER matches a filename in TEXT.
TEXT is the text that follows \"--- \" or \"+++ \" on a line of a diff.
Diff writes a tab and a timestamp after the filename, and git writes nothing
after it, but a filename may contain a tab; so the filename is any prefix of
TEXT that ends before a tab, or all of TEXT."
  ;; `string-match' sets the match data, which the caller is still using.
  (save-match-data
    (let ((result (diff-clean-matches-p text matcher))
          (start 0))
      (while (and (not result) (string-match "\t" text start))
        (setq result (diff-clean-matches-p (substring text 0 (match-beginning 0))
                                           matcher))
        (setq start (match-end 0)))
      result)))

(defun diff-clean-differ-line-matches-p (text matcher)
  "Return non-nil if MATCHER matches a filename in TEXT.
TEXT is the text between \"files \" and \" differ\" on a line of a diff, as in
\"Binary files X and Y differ\".  Because a filename may contain \" and \",
every way of splitting TEXT into two filenames is tried."
  ;; `string-match' sets the match data, which the caller is still using.
  (save-match-data
    (let ((result nil)
          (start 0))
      (while (and (not result) (string-match " and " text start))
        (setq result (or (diff-clean-matches-p (substring text 0 (match-beginning 0))
                                               matcher)
                         (diff-clean-matches-p (substring text (match-end 0))
                                               matcher)))
        (setq start (1+ (match-beginning 0))))
      result)))

;; These names may need to be changed, so that completing "diff-clean" is easier to do.

(defun diff-clean-more-files (regex &optional dont-remove-gitignored)
  "Like `diff-clean', but also removes the files that match REGEX.
The regex matches the whole filename. It must not start with ^ nor end with $.
DONT-REMOVE-GITIGNORED is as in `diff-clean'."
  (interactive "sRegex for whole filename (no ^$): \nP")
  (let ((inhibit-read-only t))
    (diff-clean-remove-files dont-remove-gitignored (list regex))
    (diff-clean-meaning-preserving)))

(defun diff-clean-only-files (regex)
  "Like `diff-clean', but removes only the specified files.
Removes the files that match the regex; unlike `diff-clean', removes neither
the files that git ignores nor the files listed in `diff-clean-removed-files'.
The regex matches the whole filename. It must not start with ^ nor end with $."
  (interactive "sRegex for whole filename (no ^$): ")
  (let ((inhibit-read-only t))
    (diff-clean-files (list regex))
    (diff-clean-meaning-preserving)))

(defun diff-clean-target (&optional dont-remove-gitignored)
  "Like `diff-clean', but also removes generated files.
DONT-REMOVE-GITIGNORED is as in `diff-clean'."
  (interactive "P")
  (diff-clean-more-files ".*/target/.*" dont-remove-gitignored))

;; This name may need to be changed, so that completing "diff-clean" is easier to do.
(defun diff-clean-build (&optional dont-remove-gitignored)
  "Like `diff-clean', but also removes generated files.
DONT-REMOVE-GITIGNORED is as in `diff-clean'."
  (interactive "P")
  (diff-clean-more-files ".*/build/.*" dont-remove-gitignored))

(defun diff-clean-backup (&optional dont-remove-gitignored)
  "Remove backup files from a diff.
DONT-REMOVE-GITIGNORED is as in `diff-clean'."
  (interactive "P")
  (diff-clean-more-files ".*~" dont-remove-gitignored))

(defun diff-clean-javadoc (&optional dont-remove-gitignored)
  "Like `diff-clean', but also removes Javadoc files.
DONT-REMOVE-GITIGNORED is as in `diff-clean'."
  (interactive "P")
  (diff-clean-more-files ".*/docs/api/.*" dont-remove-gitignored))

(defun diff-clean-json (&optional dont-remove-gitignored)
  "Like `diff-clean', but also removes JSON files.
DONT-REMOVE-GITIGNORED is as in `diff-clean'."
  (interactive "P")
  (diff-clean-more-files ".*\\.json" dont-remove-gitignored))

(defun diff-clean-tests (&optional dont-remove-gitignored)
  "Like `diff-clean', but also removes test files.
DONT-REMOVE-GITIGNORED is as in `diff-clean'."
  (interactive "P")
  (diff-clean-more-files ".*/\\(tests?\\|jtreg\\|systemTests?\\)/.*" dont-remove-gitignored))

(defun diff-clean-gitignored-filenames ()
  "Return the filenames in the current buffer's diff that git ignores.
Each is a filename as it appears in the diff, except that a file named by an
\"Only in DIR: NAME\" line is returned as \"DIR/NAME\".
A file is ignored if `git check-ignore' reports it in the repository that
contains it.  Therefore, a file that git tracks is not ignored even if an
ignore rule matches it, and a file in no git repository is not ignored.  The
files may be in different repositories, as when diffing two checkouts.
`diff-clean-gitignored-pathname' describes how a filename in the diff is
mapped to a file on disk."
  (let ((diff-names '())
        ;; Alist from a directory to an alist from a basename to the filenames,
        ;; as they appear in the diff, of the files with that basename.  Git is
        ;; run once per directory rather than once per file, because starting a
        ;; process is much more expensive than testing one more file.
        (by-directory '()))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^\\(?:--- \\|\\+\\+\\+ \\)\\([^\t\n]+\\)"
                      "\\|^Only in \\(.+\\): \\(.+\\)$"
                      ;; A binary file, or any file in --brief output, has no
                      ;; "---"/"+++" line.
                      "\\|^\\(?:Binary files \\|Files \\)\\(.+\\) and \\(.+\\) differ$")
              nil t)
        (cond ((match-string 1)
               (push (match-string 1) diff-names))
              ((match-string 2)
               ;; An "Only in " line writes ": " in place of the last "/".
               (push (concat (match-string 2) "/" (match-string 3)) diff-names))
              (t
               (push (match-string 4) diff-names)
               (push (match-string 5) diff-names)))))

    (dolist (diff-name diff-names)
      (let ((pathname (diff-clean-gitignored-pathname diff-name)))
        (when pathname
          (let* ((directory (file-name-directory pathname))
                 (basename (file-name-nondirectory pathname))
                 (directory-cell (assoc directory by-directory)))
            (unless directory-cell
              (setq directory-cell (list directory))
              (push directory-cell by-directory))
            (let ((basename-cell (assoc basename (cdr directory-cell))))
              ;; Two filenames in the diff can name the same file on disk, as
              ;; git's "a/foo" and "b/foo" do, so a basename maps to a list.
              (if basename-cell
                  (setcdr basename-cell (cons diff-name (cdr basename-cell)))
                (setcdr directory-cell
                        (cons (list basename diff-name) (cdr directory-cell)))))))))

    (let ((result '()))
      (dolist (directory-cell by-directory)
        (let ((basename-alist (cdr directory-cell)))
          (dolist (basename (diff-clean-gitignored-basenames
                             (car directory-cell)
                             (mapcar #'car basename-alist)))
            (dolist (diff-name (cdr (assoc basename basename-alist)))
              (push diff-name result)))))
      result)))

(defun diff-clean-gitignored-pathname (diff-name)
  "Return the pathname on disk of DIFF-NAME, a filename that appears in a diff.
DIFF-NAME is relative to `default-directory', which is the directory in which
the diff was created if Emacs's `diff' command created it.
Git writes filenames with an \"a/\" or \"b/\" prefix, and \"diff -r\" writes them
with the compared directory as a prefix, so DIFF-NAME without its first
component is also a candidate.  An existing file is preferred, and otherwise a
file in an existing directory, because git can report whether a nonexistent
file would be ignored but must be run in a directory that exists.
Returns nil for \"/dev/null\", which git writes for a created or deleted file,
and nil if neither candidate is in an existing directory."
  (let* ((expanded (expand-file-name diff-name))
         ;; nil if DIFF-NAME has only one component, which has no prefix.
         (stripped (and (string-match "\\`[^/]+/" diff-name)
                        (expand-file-name (substring diff-name (match-end 0))))))
    (cond ((equal diff-name "/dev/null")
           nil)
          ((file-exists-p expanded)
           expanded)
          ((and stripped (file-exists-p stripped))
           stripped)
          ;; Neither candidate exists, as when the diff deletes a file.
          ((file-directory-p (file-name-directory expanded))
           expanded)
          ((and stripped (file-directory-p (file-name-directory stripped)))
           stripped))))

(defun diff-clean-gitignored-basenames (directory basenames)
  "Return the elements of BASENAMES that git ignores.
BASENAMES are the names of files in DIRECTORY.
Returns nil if DIRECTORY does not exist or is not in a git repository.
No element of BASENAMES may contain a newline, which is how they are passed
to git."
  (when (file-directory-p directory)
    (with-temp-buffer
      (dolist (basename basenames)
        (insert basename "\n"))
      (let ((default-directory directory))
        ;; Git exits with status 1 if no file is ignored, and with status 128 on
        ;; an error such as DIRECTORY not being in a git repository.  In every
        ;; case its standard output is exactly the ignored files, so the exit
        ;; status need not be examined.  Its standard error is discarded, to
        ;; keep messages such as "fatal: not a git repository" out of the
        ;; result.
        (call-process-region (point-min) (point-max) "git" t '(t nil) nil
                             "check-ignore" "--stdin"))
      (split-string (buffer-string) "\n" t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meaning-preserving transformations
;;;

;; These transformations are never applicable to a diff created by the
;; diff program, but other transformations may make them applicable.

(defun diff-clean-meaning-preserving ()
  "Perform meaning-preserving simplifications on the diff."
  (interactive)

  (let ((inhibit-read-only t)
        (case-fold-search nil))

    (diff-realign-hunks)
    (diff-concatenate-hunks)

    ;; Each of `diff-clean-prefix-suffix' and `diff-clean-regroup-interleaved'
    ;; can create text that the other one changes, so alternate them until
    ;; neither changes anything.  The iteration terminates:  each regrouping
    ;; turns a context line that starts a Javadoc comment into a removed line
    ;; and an added line that `diff-clean-prefix-suffix' leaves alone, and
    ;; `diff-clean-prefix-suffix' creates such a context line only from a
    ;; removed line and an added line that no regrouping created.
    (let ((tick nil))
      (while (not (eql tick (buffer-chars-modified-tick)))
        (setq tick (buffer-chars-modified-tick))
        (diff-clean-prefix-suffix)
        (diff-clean-regroup-interleaved)))
    (diff-clean-empty-parts)
    ))


(defun diff-clean-prefix-suffix ()
  "Simplify a diff when a hunk has identical prefix or suffix for - and +."
  (interactive)

  ;; `case-fold-search' must be nil: otherwise the "\\1" back-references match
  ;; case-insensitively, so a line pair that differs only in case, such as
  ;; "-b" and "+B", collapses to a single context line.  That discards the
  ;; change, and the resulting all-context hunk is then removed as empty.
  (let ((inhibit-read-only t)
        (case-fold-search nil))
    (save-excursion
      ;; Each rule leaves alone the lines that `diff-clean-regroup-interleaved'
      ;; creates.  Otherwise, it would turn the "/**" lines at the start of
      ;; the regrouped old text and new text into a context line, and then
      ;; `diff-clean-regroup-interleaved' would regroup the lines again.
      ;; First two lines are identical (one -, one +).
      (diff-clean-replace-iteratively-unless
       "^-\\(.*\\)\n\\+\\1\n" " \\1\n" #'diff-clean-match-is-regrouped-p)
      ;; First - line is identical to first + line
      (diff-clean-replace-iteratively-unless
       "^-\\(.*\\)\n\\(\\(-.*\n\\)+\\)\\+\\1\n" " \\1\n\\2"
       #'diff-clean-match-is-regrouped-p)
      ;; Last - line is identical to last + line
      (diff-clean-replace-iteratively-unless
       "^-\\(.*\\)\n\\(\\(\\+.*\n\\)+\\)\\+\\1\n" "\\2 \\1\n"
       #'diff-clean-match-is-regrouped-p)
      ;; There is no case for "last - line is identical to first + line": the
      ;; regexp for "first - line is identical to first + line", above,
      ;; already covers it.
      ;; Needs to be tested before uncommenting
      ;; (goto-char (point-min))
      ;; (query-replace-regexp "^-\\(.*\\)\n\\+\\(.*\n\\)\\+\\1\n" " \\2+\\1\n")

      ;; ;; Remove identical lines with one different one between them.
      ;; (goto-char (point-min))
      ;; (replace-all-occurrrences-iteratively "^-\\(.*\\)\n\\([-+].*\n\\)\\+\\1\n" "\\2 \\1\n")

      ;; Should do the same as the above, with any number of different lines between them.
      )))

(defun diff-clean-match-is-regrouped-p ()
  "Return true if the last match contains a marked line of a regrouping.
The marked lines are the first removed line and the first added line that
`diff-clean-regroup-interleaved' writes for a region, which have the text
property `diff-clean-regrouped'."
  (text-property-any (match-beginning 0) (match-end 0) 'diff-clean-regrouped t))

(defun diff-clean-replace-iteratively-unless (regexp replacement keep-p)
  "Replace the matches for REGEXP in the current buffer by REPLACEMENT.
A match is left unchanged if KEEP-P, a function of no arguments that is called
with the match data of the match, returns true.  Like
`replace-all-occurrrences-iteratively', iterate until every remaining match is
one that is left unchanged.  REPLACEMENT can refer to groups of REGEXP, as in
\"\\\\1\", and its case is not altered."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (if (save-match-data (funcall keep-p))
          (progn
            (goto-char (match-beginning 0))
            (forward-line 1))
        (let ((replacement-begin (match-beginning 0)))
          (replace-match replacement t)
          ;; The replacement can create a match that starts before it.  A match
          ;; consists of removed and added lines only, so it starts no earlier
          ;; than the run of such lines that precedes the replacement.
          (goto-char replacement-begin)
          (while (and (not (bobp))
                      (save-excursion
                        (forward-line -1)
                        (looking-at "[-+]")))
            (forward-line -1)))))))


(defconst diff-clean-regroup-trigger-regexp
  "^ [ \t]*/\\*\\*[ \t]*\n[-+]"
  "Matches a context line that starts a Javadoc comment, followed by a change.
An example is \"   /**\" followed by a line that starts with \"-\" or \"+\".
Each such context line starts a region for `diff-clean-regroup-interleaved'.")

(defconst diff-clean-unified-hunk-header-regexp
  "@@ -\\(?3:[0-9]+\\)\\(?:,\\(?1:[0-9]+\\)\\)? \\+\\(?4:[0-9]+\\)\\(?:,\\(?2:[0-9]+\\)\\)? @@"
  "Matches, at the start of a line, the header of a hunk of a unified diff.
An example is \"@@ -10,7 +12,8 @@\", whose hunk has 7 old lines starting at
line 10 of the old file and 8 new lines starting at line 12 of the new file.
Group 1 is the number of old lines, and group 2 is the number of new lines.
Each of those groups is absent, as in \"@@ -10 +12,2 @@\", when its number is 1.
Group 3 is the starting line number in the old file, and group 4 is the
starting line number in the new file.")

(defun diff-clean-hunk-remaining-counts ()
  "Return the numbers of old and new lines of the current hunk that start at point.
Point is at the start of a line.  The result is a cons such as (3 . 4), whose
car counts the context and removed lines from point to the end of the hunk, and
whose cdr counts the context and added lines.  It is computed from the line
counts in the hunk's header.  The result is nil if point is not in a hunk of a
unified diff; an example is a hunk of a combined diff, whose header starts with
\"@@@\" and which has an indicator column for each parent of a merge."
  (let ((line-begin (point)))
    (save-excursion
      (when (and (re-search-backward "^@@" nil t)
                 (looking-at diff-clean-unified-hunk-header-regexp))
        (let ((old (if (match-beginning 1) (string-to-number (match-string 1)) 1))
              (new (if (match-beginning 2) (string-to-number (match-string 2)) 1)))
          (forward-line 1)
          (while (< (point) line-begin)
            ;; An empty line is a blank context line.
            (let ((indicator (char-after)))
              (when (memq indicator '(?\s ?- ?\n))
                (setq old (1- old)))
              (when (memq indicator '(?\s ?+ ?\n))
                (setq new (1- new))))
            (forward-line 1))
          (cons old new))))))

(defconst diff-clean-regroup-filler-regexp
  "\\(?: [^[:alnum:]\n]*\\)?$"
  "Matches, at the start of a line, a context line that has no letter or digit.
Examples are \"    */\", \"   }\", and a blank line.  A blank context line can be
empty, because editing a diff can strip the leading space from it.")

(defun diff-clean-regroup-interleaved ()
  "Regroup the changes that follow each context line that starts a Javadoc comment.
A region starts at a context line that consists of \"/**\" and that is followed
by a removed or added line.  The region extends through the hunk's removed
lines, added lines, and context lines that have no letter or digit, such as
\"   }\" and \"    */\".  It ends at the last removed or added line before a
context line that has a letter or digit, or before the end of the hunk.  The
end of the hunk is computed from the line counts in its header, and a hunk
without such a header, such as a hunk of a combined diff, is left alone.
If a context line separates two of the region's changes, then the changes are
likely to be unrelated, and they share only short lines such as \"    */\";
showing them interleaved is confusing.  So, replace the region by all of its
old text as removed lines, followed by all of its new text as added lines.
Each context line of the region, including the \"/**\" line, appears in both.
A \"\\ No newline at end of file\" line just after the region stays with the
line that precedes it.  The first removed line and the first added line, which
are the \"/**\" lines, get the text property `diff-clean-regrouped', so that
`diff-clean-prefix-suffix' leaves the regrouped lines alone.
Operates on the current buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward diff-clean-regroup-trigger-regexp nil t)
        (goto-char (match-beginning 0))
        (let ((counts (diff-clean-hunk-remaining-counts)))
          (if (not (and counts (> (car counts) 0) (> (cdr counts) 0)))
              (forward-line 1)
            (let ((region-begin (point))
                  ;; The end of the region's last removed or added line.
                  (region-end nil)
                  ;; The numbers of old and new lines of the hunk after the
                  ;; current line.
                  (old-remaining (1- (car counts)))
                  (new-remaining (1- (cdr counts)))
                  ;; True if the region has a context line after a change.
                  (context-after-change nil)
                  ;; True if a context line separates two of the region's changes.
                  (interleaved nil))
              (forward-line 1)
              (while (and (not (eobp))
                          (cond ((and (looking-at "-") (> old-remaining 0))
                                 (setq old-remaining (1- old-remaining))
                                 (when context-after-change
                                   (setq interleaved t))
                                 (setq region-end (line-beginning-position 2))
                                 t)
                                ((and (looking-at "\\+") (> new-remaining 0))
                                 (setq new-remaining (1- new-remaining))
                                 (when context-after-change
                                   (setq interleaved t))
                                 (setq region-end (line-beginning-position 2))
                                 t)
                                ((and (> old-remaining 0) (> new-remaining 0)
                                      (looking-at diff-clean-regroup-filler-regexp))
                                 (setq old-remaining (1- old-remaining))
                                 (setq new-remaining (1- new-remaining))
                                 (when region-end
                                   (setq context-after-change t))
                                 t)))
                (forward-line 1))
              (if (not interleaved)
                  (progn
                    (goto-char region-begin)
                    (forward-line 1))
                (let ((old-lines '())
                      (new-lines '())
                      ;; The indicator character of the previous line.
                      (previous-indicator nil))
                  ;; A "\ No newline at end of file" line belongs to the line
                  ;; before it, so it moves along with that line.
                  (when (save-excursion
                          (goto-char region-end)
                          (looking-at "\\\\"))
                    (setq region-end (save-excursion
                                       (goto-char region-end)
                                       (line-beginning-position 2))))
                  (dolist (line (split-string
                                 (string-remove-suffix
                                  "\n" (buffer-substring region-begin region-end))
                                 "\n"))
                    (let ((indicator (if (string-empty-p line) ?\s (aref line 0)))
                          (text (if (string-empty-p line) "" (substring line 1))))
                      (if (eq indicator ?\\)
                          (if (eq previous-indicator ?-)
                              (push line old-lines)
                            (push line new-lines))
                        (when (memq indicator '(?\s ?-))
                          (push (concat "-" text) old-lines))
                        (when (memq indicator '(?\s ?+))
                          (push (concat "+" text) new-lines)))
                      (setq previous-indicator indicator)))
                  (delete-region region-begin region-end)
                  (goto-char region-begin)
                  (dolist (lines (list (nreverse old-lines)
                                       (nreverse new-lines)))
                    (insert (propertize (car lines) 'diff-clean-regrouped t)
                            "\n")
                    (dolist (line (cdr lines))
                      (insert line "\n"))))))))))))

(defun diff-clean-empty-parts ()
  "Remove empty parts of the file: empty hunks and empty file sections."
  (interactive)
  ;; Both regexps match diff keywords such as "diff" and "Only in ", which must
  ;; not match text that differs from a keyword only in case.  Otherwise, for
  ;; example, "^diff.*\n" matches the "Diff finished." line that Emacs's `diff'
  ;; appends, and the line is removed from a diff that has real content.
  (let ((inhibit-read-only t)
        (case-fold-search nil))
    (replace-all-occurrrences-iteratively diff-clean-empty-hunk-regexp "\\2")
    (replace-all-occurrrences-iteratively diff-clean-empty-filesection-regexp "\\1")))


(defun diff-realign-hunks ()
  "Shift a run of added or deleted lines earlier in the diff, when possible.
A run may be shifted when it contains a blank line and its lines after that
blank line are identical to the context lines that precede the run.  The
shifted run ends at the blank line, which groups the changed lines more
meaningfully.  Operates on the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
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
                     (>= candidate-start (point-min))
                     (string= moved-text-without-plus (buffer-substring candidate-start candidate-end)))
                (change-indicator-char-in-region " " indicator-char candidate-start candidate-end)
                (change-indicator-char-in-region indicator-char " " moved-text-begin moved-text-end))))
          (goto-char change-end))))))

(defun diff-concatenate-hunks ()
  "Merge two hunks that are separated only by punctuation."
  (interactive)
  (let ((inhibit-read-only t))
    (diff-concatenate-hunks-with-indicator "+")
    (diff-concatenate-hunks-with-indicator "-")
    ))

(defun diff-concatenate-hunks-with-indicator (indicator-char)
  "Merge two hunks that are separated only by punctuation.
INDICATOR-CHAR is \"+\" or \"-\"."
  (save-excursion
    (goto-char (point-min))
    (let ((regex (concat "^[^" indicator-char "].*\n"
                         "\\(\\(?:[" indicator-char "].*\n\\)+\\)"
                         "\\(?: [][(){}* \t\n\r]*\n\\)+"
                         "\\(\\(?:[" indicator-char "].*\n\\)+\\)"
                         "[^" indicator-char "]")))
      (while (re-search-forward regex nil t)
        (let* ((change1-begin (match-beginning 1))
               (punctuation-begin (match-end 1))
               (punctuation-end (match-beginning 2))
               (change2-end (match-end 2))
               (punctuation-length (- punctuation-end punctuation-begin))
               (punctuation (buffer-substring punctuation-begin punctuation-end)))
          (change-indicator-char-in-region
           " " indicator-char punctuation-begin punctuation-end)
          ;; The punctuation is now part of the change, so it must also appear
          ;; with the opposite indicator character: before the first change
          ;; block for "+" (where it belongs to the old text), and after the
          ;; second change block for "-" (where it belongs to the new text).
          (let* ((insertion-point (cond ((equal "+" indicator-char)
                                         change1-begin)
                                        ((equal "-" indicator-char)
                                         change2-end)
                                        (t
                                         (error "Bad indicator character `%s'" indicator-char))))
                 (new-indicator (opposite-indicator-char indicator-char)))
            (goto-char insertion-point)
            (insert punctuation)
            (change-indicator-char-in-region
             " " new-indicator insertion-point (+ insertion-point punctuation-length)))
          (goto-char change1-begin)
          (forward-line -1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-meaning-preserving transformations
;;;


(defconst diff-clean-import-regexp
  (concat "\\([-+]\\)\\(?:"
          (string-join '("import .*;"              ; Java
                         "package .*;"             ; Java
                         "import [[:alnum:]_., ]+" ; Python
                         "from .* import .*")      ; Python
                       "\\|")
          "\\)\r?$")
  "Matches, at line start, an added or removed import or package statement.
An example is \"-import java.util.List;\".  The statement is a Java or Python
import statement or a Java package statement.  Group 1 is the indicator
character, \"-\" or \"+\".")

(defun diff-clean-imports ()
  "Cleans up a diff to remove changes in import and package statements.
Deletes every added or removed line that is a Java or Python import
statement or a Java package statement, along with the added or removed
blank lines that immediately follow it and have the same indicator
character, then runs `diff-clean'.  Only hunks of a unified diff are
cleaned.  A \"\\ No newline at end of file\" line that follows a deleted
line is deleted too.  The hunk headers are adjusted so that the first old
line and the first new line of each hunk have the correct line numbers,
but a line that follows a deleted line in the same hunk has a smaller
line number than in the file, because the deleted line is not counted."
  (interactive)
  (let ((inhibit-read-only t)
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@@" nil t)
        (goto-char (match-beginning 0))
        (if (looking-at diff-clean-unified-hunk-header-regexp)
            (diff-clean-imports-in-hunk)
          (forward-line 1))))
    ;; The deletions make the line counts in the hunk headers wrong.
    (diff-fixup-modifs (point-min) (point-max)))
  (diff-clean))

(defun diff-clean-imports-in-hunk ()
  "Delete the import and package statements of the hunk whose header is at point.
The hunk is a hunk of a unified diff, and the statements are the lines that
`diff-clean-import-regexp' matches.  Also deletes the blank lines that
immediately follow each such line and have the same indicator character,
and a \"\\ No newline at end of file\" line that follows the deleted lines.
Changes the starting line numbers in the hunk's header to those of the first
old line and the first new line that remain, but leaves the line counts
unchanged.  Leaves point at the end of the hunk."
  (looking-at diff-clean-unified-hunk-header-regexp)
  (let* ((header-begin (point))
         (old-start (string-to-number (match-string 3)))
         (new-start (string-to-number (match-string 4)))
         (old-remaining (if (match-beginning 1) (string-to-number (match-string 1)) 1))
         (new-remaining (if (match-beginning 2) (string-to-number (match-string 2)) 1))
         ;; The numbers of old and new lines deleted before the first old line
         ;; and the first new line that remain.
         (old-skipped 0)
         (new-skipped 0)
         ;; True if an old line, or a new line, remains before point.
         (old-kept nil)
         (new-kept nil))
    (forward-line 1)
    (while (and (or (> old-remaining 0) (> new-remaining 0)) (not (eobp)))
      (if (looking-at diff-clean-import-regexp)
          (let* ((removed (equal (match-string 1) "-"))
                 (blank-line-regexp
                  (concat (regexp-quote (match-string 1)) "[ \t]*\r?$"))
                 (start (point))
                 (deleted 1))
            (forward-line 1)
            (while (and (> (if removed old-remaining new-remaining) deleted)
                        (not (eobp))
                        (looking-at blank-line-regexp))
              (setq deleted (1+ deleted))
              (forward-line 1))
            ;; The marker describes the last deleted line.
            (when (looking-at "\\\\")
              (forward-line 1))
            (delete-region start (point))
            (if removed
                (progn
                  (setq old-remaining (- old-remaining deleted))
                  (unless old-kept
                    (setq old-skipped (+ old-skipped deleted))))
              (setq new-remaining (- new-remaining deleted))
              (unless new-kept
                (setq new-skipped (+ new-skipped deleted)))))
        ;; An empty line is a blank context line.
        (let ((indicator (char-after)))
          (when (memq indicator '(?\s ?- ?\n))
            (setq old-remaining (1- old-remaining)
                  old-kept t))
          (when (memq indicator '(?\s ?+ ?\n))
            (setq new-remaining (1- new-remaining)
                  new-kept t)))
        (forward-line 1)))
    ;; A "\ No newline at end of file" line that follows the hunk's last line
    ;; belongs to the hunk.
    (when (looking-at "\\\\")
      (forward-line 1))
    (let ((hunk-end (point-marker)))
      (goto-char header-begin)
      (looking-at diff-clean-unified-hunk-header-regexp)
      ;; A side with no lines has the number of the line that precedes the
      ;; hunk, as in "@@ -5,0 +6,1 @@".  Replace group 4 before group 3,
      ;; because replacing group 4 does not move group 3.
      (when (> new-skipped 0)
        (replace-match (number-to-string
                        (+ new-start new-skipped (if new-kept 0 -1)))
                       t t nil 4))
      (when (> old-skipped 0)
        (replace-match (number-to-string
                        (+ old-start old-skipped (if old-kept 0 -1)))
                       t t nil 3))
      (goto-char hunk-end)
      (set-marker hunk-end nil))))


;; TODO: also define diff-clean-delete-matching-hunks, which can share a lot of
;; code with this.
(defun diff-clean-delete-non-matching-hunks (regexp)
  "Delete hunks that do not contain a match for the given regexp."
  (interactive "sRegexp: ")
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^@@ " nil t)
        (let ((hunk-start (match-beginning 0)))
	  ;; A "\\ No newline at end of file" line belongs to the hunk.
	  (re-search-forward "^[^-+ \\\\]\\|\\'")
	  (let ((hunk-end (match-beginning 0)))
	    (goto-char hunk-start)
	    (if (re-search-forward regexp hunk-end t)
	        (goto-char hunk-end)
	      (delete-region hunk-start hunk-end))))))
    ;; Remove the headers of files whose hunks were all deleted.
    (diff-clean-empty-parts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

(defun file-regexp-to-colon-regexp (regexp)
  "Change the last slash in REGEXP to \": \"."
  (let ((result (replace-regexp-in-string "\\(/\\)[^/]*$" ": " regexp nil nil 1)))
    (if (string-suffix-p "/.*" regexp)
	(let ((additional (file-regexp-to-colon-regexp (substring regexp 0 (- (length regexp) 3)))))
	  (concat "\\(" result "\\)\\|\\(" additional "\\)"))
      result)))
;; (file-regexp-to-colon-regexp ".*/defects4j[^/]*/framework/test/d4j.log")
;; (file-regexp-to-colon-regexp ".*/daikon[^/]*/utils/.*")
;; (file-regexp-to-colon-regexp	".*/logging-log4j2.*/target/.*")

(defun file-name-to-colon-name (filename)
  "Change the last slash in FILENAME to \": \", as an \"Only in \" line does."
  (if (string-match "/[^/]*\\'" filename)
      (concat (substring filename 0 (match-beginning 0))
              ": "
              (substring filename (1+ (match-beginning 0))))
    filename))

(defun opposite-indicator-char (indicator-char)
  "Given \"-\", returns \"+\".  Given \"+\", returns \"-\"."
  (cond ((equal "-" indicator-char)
         "+")
        ((equal "+" indicator-char)
         "-")
        (t
         (error "Bad indicator char: %s" indicator-char))))

(defun change-indicator-char-in-region (old-indicator new-indicator begin end)
  "Replace the indicator character (the character in column 1), in the region.
Do nothing with lines that do not begin with OLD-INDICATOR."
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
  "Convert Java array declarations to put the brackets on the element type.
For example, convert \"short a2[]\" to \"short[] a2\", or
\"@PolySigned short a2 @Nullable []\" to \"@PolySigned short @Nullable [] a2\"."
  (interactive)
  (tags-query-replace
   (concat
    "\\([^@]\\)\\b\\([A-Z][a-z][A-Za-z0-9]+\\|byte\\|short\\|int\\|long\\|float\\|double\\|boolean\\|char\\) "
    "\\([A-Za-z0-9]+\\)\\(\\( ?@[A-Za-z0-9]+ ?\\)*\\[\\]\\)")
   "\\1\\2\\4 \\3")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Batch use
;;;

(defun diff-clean-batch-tests-and-imports ()
  "Simplify each diff file named in `command-line-args-left', in place.
Runs `diff-clean-tests' and then `diff-clean-imports' on each file, and
saves it without a backup file.  Does not ask whether to open a large file.
Usage:
  emacs -Q --batch -L DIR -l diff-clean \\
    -f diff-clean-batch-tests-and-imports FILE.diff ...
where DIR is the directory that holds diff-clean.el."
  (unless noninteractive
    (error "`diff-clean-batch-tests-and-imports' is only for batch mode"))
  (let ((make-backup-files nil)
        (large-file-warning-threshold nil))
    (dolist (file command-line-args-left)
      (with-current-buffer (find-file-noselect file)
        (diff-clean-tests)
        (diff-clean-imports)
        (save-buffer))))
  (setq command-line-args-left nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

(provide 'diff-clean)
