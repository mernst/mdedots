;;; difffontify.el --- Print a fontified diff  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; `difffontify-pdf' writes the diff in the current `diff-mode' buffer to a PDF
;; file, and `difffontify-print' prints it.  Each line of a hunk is fontified
;; as it would be in a buffer visiting its file.  The output is legible in
;; black and white:  the font (bold, oblique) distinguishes syntactic
;; categories, and deleted lines are shaded.  A prefix argument to either
;; command inverts `difffontify-color' for that call.
;;
;; Each page's header shows the diff's name and the page number.  The first
;; page's header also shows the date.  Each other page's header also shows the
;; file and the function at the top of the page, if the page starts within a
;; hunk.  A page that starts elsewhere, such as in a file header (the "diff ",
;; "index ", "--- ", and "+++ " lines), gets nothing more in its header.  The
;; function is the last one that is defined before the top of the page, in a
;; hunk line or in the hunk header, where "diff -p" and "git diff" write the
;; definition line that precedes the hunk.  For a file in a programming
;; language, the header omits the definition's annotations, modifiers, and
;; return type, as in "count(int x) {" for
;; "@Pure public int count(@NonNull int x) {".  To fit beside the page
;; number, a header that is too long first shortens the function's formal
;; parameters, and then the start of the file name, leaving its last path
;; components.
;;
;; From the command line, to write FILE.diff.pdf:
;;   emacs -Q --batch -l difffontify.el -f difffontify-batch-pdf FILE.diff ...

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'ps-print)
(require 'subr-x)

(defgroup difffontify nil
  "Print diffs with syntax highlighting."
  :group 'diff-mode)

(defcustom difffontify-color nil
  "Non-nil means that `difffontify-pdf' and `difffontify-print' use color.
Nil means black and white.  A prefix argument to either command inverts
this."
  :type 'boolean)

(defcustom difffontify-lpr-command "lpr"
  "Program that `difffontify-print' runs, with a PDF file as its argument."
  :type 'string)

(defcustom difffontify-latin-1-command "iconv-latin-1"
  "Program that converts UTF-8 to Latin 1, approximating other characters.
It reads standard input and writes standard output, and preserves line breaks.
`ps-print' prints each character that is not in Latin 1 as \"?\"."
  :type 'string)

(defconst difffontify--paper-type 'letter)
(defconst difffontify--font-size 8.7)
(defconst difffontify--margin 36
  "Page margins, in points.")
(defconst difffontify--header-font-size 10)

(defconst difffontify--diff-faces
  '((diff-file-header nil nil bold)
    ;; These diff faces are listed, without attributes, so that
    ;; `difffontify--wrap-lines' can recognize hunk lines.
    (diff-indicator-added nil nil)
    (diff-added nil nil)
    (diff-context nil nil))
  "Face specifications for diff faces, except deleted lines, in both modes.")

(defconst difffontify--mono-faces
  `((font-lock-keyword-face nil nil bold)
    (font-lock-builtin-face nil nil bold)
    (font-lock-type-face nil nil bold)
    (font-lock-warning-face nil nil bold)
    (font-lock-function-name-face nil nil bold italic)
    ;; Inherits from `font-lock-function-name-face', but only definitions are
    ;; highlighted.
    (font-lock-function-call-face nil nil)
    (font-lock-comment-face nil nil italic)
    (font-lock-comment-delimiter-face nil nil italic)
    (font-lock-doc-face nil nil italic)
    (font-lock-doc-markup-face nil nil italic)
    (font-lock-preprocessor-face nil nil italic)
    (diff-removed nil "#e6e6e6")
    (diff-indicator-removed nil "#e6e6e6")
    ,@difffontify--diff-faces)
  "Face specifications for black and white.
The format is that of `ps-black-white-faces'.  A face that is not listed,
and does not inherit from a listed face, is printed in the regular font.")

(defconst difffontify--color-faces
  `((font-lock-keyword-face "#00008b" nil bold)
    (font-lock-builtin-face "#00008b" nil bold)
    (font-lock-type-face "#00008b" nil bold)
    (font-lock-warning-face "#8b0000" nil bold)
    (font-lock-function-name-face "#5a005a" nil bold italic)
    (font-lock-function-call-face nil nil)
    (font-lock-comment-face "#006400" nil italic)
    (font-lock-comment-delimiter-face "#006400" nil italic)
    (font-lock-doc-face "#006400" nil italic)
    (font-lock-doc-markup-face "#006400" nil italic)
    (font-lock-preprocessor-face "#6b3a00" nil italic)
    (font-lock-string-face "#8b0000" nil)
    (diff-removed nil "#ffe4e4")
    (diff-indicator-removed nil "#ffe4e4")
    ,@difffontify--diff-faces)
  "Face specifications for color, in the format of `ps-black-white-faces'.
Each face has the same font as in `difffontify--mono-faces', so that a color
printout is legible on a black-and-white printer.  Each foreground color has
a contrast ratio of at least 7:1 against white.")

(defvar ps-multibyte-buffer)            ; in ps-mule.el

(defvar difffontify--header-title nil
  "The diff's name, which starts each page header.")


;;; Commands

;;;###autoload
(defun difffontify-pdf (&optional invert-color)
  "Write the current buffer's diff, with syntax highlighting, to a PDF file.
The file is named after the diff file, or for a `*Diff*' buffer, after the
new file or directory.  An existing file is overwritten.  Use color if
`difffontify-color' is non-nil; a prefix argument INVERT-COLOR inverts that.
Return the PDF file name."
  (interactive "P")
  (let ((pdf (difffontify--pdf-file-name)))
    (difffontify--write-pdf pdf (difffontify--title pdf)
                            (difffontify--color-p invert-color))
    (message "Wrote %s" pdf)
    pdf))

;;;###autoload
(defun difffontify-print (&optional invert-color)
  "Print the diff in the current buffer, with syntax highlighting.
Run `difffontify-lpr-command' on a temporary PDF file, and delete the file if
printing succeeds.  Use color if `difffontify-color' is non-nil; a prefix
argument INVERT-COLOR inverts that."
  (interactive "P")
  (let ((title (difffontify--title (difffontify--pdf-file-name)))
        (pdf (make-temp-file "difffontify-" nil ".pdf")))
    (condition-case err
        (difffontify--write-pdf pdf title (difffontify--color-p invert-color))
      ;; `make-temp-file' created the file, so delete it.
      (t (delete-file pdf)
         (signal (car err) (cdr err))))
    (with-temp-buffer
      (if (eql 0 (call-process difffontify-lpr-command nil t nil pdf))
          (progn
            (delete-file pdf)
            (message "Printed %s" title))
        (let ((output (string-trim (buffer-string))))
          (error "%s failed, so %s was not deleted%s"
                 difffontify-lpr-command pdf
                 (if (string-empty-p output) "" (concat ": " output))))))))

(defun difffontify-batch-pdf ()
  "Write a PDF file for each diff file named in `command-line-args-left'.
Usage:
  emacs -Q --batch -l difffontify.el -f difffontify-batch-pdf FILE.diff ..."
  (unless noninteractive
    (error "`difffontify-batch-pdf' is only for batch mode"))
  (dolist (file command-line-args-left)
    (with-current-buffer (find-file-noselect file)
      (difffontify-pdf)))
  (setq command-line-args-left nil))


;;; File names

(defun difffontify--color-p (invert)
  "Return non-nil if the output should use color.
INVERT non-nil inverts `difffontify-color'."
  (xor invert difffontify-color))

(defun difffontify--pdf-file-name ()
  "Return the name of the PDF file for the diff in the current buffer."
  (if buffer-file-name
      (concat buffer-file-name ".pdf")
    (let ((new (difffontify--new-file-name)))
      (expand-file-name
       (if new
           (concat (file-name-nondirectory (directory-file-name new))
                   ".diff.pdf")
         "diff.pdf")))))

(defun difffontify--new-file-name ()
  "Return the new file or directory in the current buffer's diff, or nil.
In a `*Diff*' buffer, the first line is the diff command, whose last
argument is the new file or directory.  Otherwise, use the first \"+++\"
line."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (cond ((looking-at "diff .*")
             (car (last (split-string-shell-command (match-string 0)))))
            ((re-search-forward "^\\+\\+\\+ \\([^\t\n]+\\)" nil t)
             (match-string 1))))))

(defun difffontify--title (pdf)
  "Return the page header for the PDF file named PDF."
  (if buffer-file-name
      (file-name-nondirectory buffer-file-name)
    (file-name-nondirectory (file-name-sans-extension pdf))))


;;; Fontification

(defun difffontify--faces (color)
  "Return the face specifications for COLOR (non-nil) or black and white."
  (if color difffontify--color-faces difffontify--mono-faces))

(defun difffontify--prepare (color)
  "Return a new buffer that holds the current buffer's diff, ready to print.
Each character's `face' property is a list of faces from `difffontify--faces'
for COLOR, each line has a `difffontify-location' property (see
`difffontify--mark-locations'), long lines are wrapped, and the \"Diff
finished\" line that ends a `*Diff*' buffer is omitted.  The caller must
kill the buffer."
  (let ((text (save-restriction
                (widen)
                (buffer-substring-no-properties (point-min) (point-max))))
        ;; `diff-mode' highlights a hunk using the real files only if
        ;; `diff-default-directory' is non-nil.  `diff' sets it; for a diff
        ;; file, the compared files' names are relative to the diff file's
        ;; directory.
        (dir (or diff-default-directory
                 (and buffer-file-name (file-name-directory buffer-file-name))
                 default-directory))
        (buffer (generate-new-buffer " *difffontify*")))
    (with-current-buffer buffer
      (insert text)
      (difffontify--delete-diff-finished)
      (setq default-directory dir)
      ;; A fresh buffer, rather than the user's buffer, so that the user's
      ;; settings (such as `diff-refine', whose faces would hide the syntax
      ;; faces) and hooks do not affect the result, and the user's buffer is
      ;; not modified.
      (let ((diff-font-lock-syntax 'hunk-also)
            (diff-refine nil)
            (diff-font-lock-prettify nil)
            (orig-fontify-props (symbol-function 'diff-syntax-fontify-props))
            (orig-file-names (symbol-function 'diff-hunk-file-names)))
        (delay-mode-hooks (diff-mode))
        (setq-local diff-default-directory dir)
        (setq tab-width 8)
        (cl-letf (((symbol-function 'diff-syntax-fontify-props)
                   (lambda (file text line-nb &optional hunk-only)
                     (difffontify--fontify-props
                      orig-fontify-props file text line-nb hunk-only)))
                  ((symbol-function 'diff-hunk-file-names)
                   (lambda (&optional old)
                     (mapcar #'difffontify--strip-git-prefix
                             (funcall orig-file-names old)))))
          ;; Major modes for the compared files print messages, such as
          ;; Python's "Can't guess python-indent-offset".
          (let ((inhibit-message t))
            (font-lock-ensure))))
      (difffontify--embolden-diff-lines)
      (difffontify--flatten-faces (difffontify--faces color))
      (difffontify--latin-1)
      (difffontify--mark-locations)
      (difffontify--wrap-lines (difffontify--line-width))
      ;; `ps-print' calls `font-lock-ensure', which would replace the faces.
      (setq-local font-lock-ensure-function #'ignore)
      (set-buffer-modified-p nil))
    buffer))

(defun difffontify--delete-diff-finished ()
  "Delete the \"Diff finished\" line, and the blank line before it, at the end.
`diff' appends them to a `*Diff*' buffer, and a diff file saved from that
buffer contains them too."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^\nDiff finished.*\n?\\'"
                              (line-beginning-position -1) t)
      (replace-match ""))))

(defun difffontify--fontify-props (orig file text line-nb hunk-only)
  "Call ORIG, which is `diff-syntax-fontify-props'.
Pass it FILE, TEXT, LINE-NB, and HUNK-ONLY.  When `diff-mode' highlights a
hunk using a real file but cannot find the hunk text in the file, it does
not highlight the hunk at all.  That happens if the file was changed after
the diff was made, or if the diff ignored whitespace (diff -b).  In that
case, highlight the hunk text alone."
  (or (funcall orig file text line-nb hunk-only)
      (and (not hunk-only)
           ;; As in `diff-syntax-fontify-hunk', an error from the file's major
           ;; mode does not prevent highlighting the rest of the diff.
           (with-temp-buffer
             (insert text)
             (with-demoted-errors "%S"
               (funcall orig file text line-nb t))))))

(defun difffontify--strip-git-prefix (file)
  "Return FILE, a file name from a diff, without a leading \"a/\" or \"b/\".
\"git diff\" writes the old file \"dir/foo.c\" as \"a/dir/foo.c\" and the new
file as \"b/dir/foo.c\"."
  (if (string-match-p "\\`[ab]/" file)
      (substring file 2)
    file))

(defun difffontify--embolden-diff-lines ()
  "Give the `diff-file-header' face to each file header line.
These lines start with \"diff \" (from \"diff -r\", \"git diff\", or the first
line of a `*Diff*' buffer), \"--- \", or \"+++ \".  `diff-mode' gives the first
kind the same face as other header lines, and gives the `diff-file-header'
face to only the file name in the others."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(?:diff \\|--- \\|\\+\\+\\+ \\).*$" nil t)
      ;; A deleted line can start with "--- ", but its face is not
      ;; `diff-header'.
      (when (memq 'diff-header
                  (ensure-list (get-text-property (match-beginning 0) 'face)))
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'diff-file-header)))))

(defun difffontify--flatten-faces (specs)
  "Replace overlay faces and `face' properties by lists of faces from SPECS.
SPECS is in the format of `ps-black-white-faces'.  At each position, the list
holds the overlays' faces (`diff-mode' highlights syntax with overlays)
followed by the `face' property's faces, each replaced by itself or the
nearest face it inherits from that is in SPECS, or omitted if there is none.
`ps-print' uses only an overlay's face, if there is one, so without this,
syntax-highlighted text on a deleted line would not be shaded."
  (let ((pos (point-min))
        runs)
    (while (< pos (point-max))
      (let ((next (min (next-single-property-change pos 'face nil (point-max))
                       (next-overlay-change pos)))
            (overlays (sort (cl-remove-if-not (lambda (o) (overlay-get o 'face))
                                              (overlays-at pos))
                            (lambda (o1 o2)
                              (> (or (overlay-get o1 'priority) 0)
                                 (or (overlay-get o2 'priority) 0))))))
        (push (list pos next
                    (delete-dups
                     (difffontify--spec-faces
                      (append
                       (mapcar (lambda (o) (overlay-get o 'face)) overlays)
                       (ensure-list (get-text-property pos 'face)))
                      specs)))
              runs)
        (setq pos next)))
    (remove-overlays)
    (dolist (run runs)
      (put-text-property (nth 0 run) (nth 1 run) 'face (nth 2 run)))))

(defun difffontify--spec-faces (face specs)
  "Return a list of the faces in SPECS that correspond to FACE.
FACE is a value of a `face' property:  a face, an anonymous face, or a list
of them."
  (cond ((null face) nil)
        ((symbolp face)
         (let ((spec-face (difffontify--spec-face face specs)))
           (and spec-face (list spec-face))))
        ((keywordp (car face))
         (difffontify--spec-faces (plist-get face :inherit) specs))
        ((memq (car face) '(foreground-color background-color))
         nil)
        (t
         (mapcan (lambda (f) (difffontify--spec-faces f specs)) face))))

(defun difffontify--spec-face (face specs)
  "Return FACE or the nearest face it inherits from that is in SPECS, or nil."
  (let (seen)
    (while (and face
                (not (assq face specs))
                (not (memq face seen)))
      (push face seen)
      (let ((inherit (and (facep face) (face-attribute face :inherit nil nil))))
        (setq face (if (listp inherit) (car inherit) inherit))))
    (and face (assq face specs) face)))


;;; Latin 1

(defun difffontify--latin-1 ()
  "Replace each character that is not in Latin 1 by an approximation, or \"?\".
The replacement has the same text properties as the character."
  (save-excursion
    (let ((replacements (difffontify--latin-1-replacements
                         (difffontify--non-latin-1-chars))))
      (goto-char (point-min))
      (while (re-search-forward "[^\u0000-\u00ff]" nil t)
        (let ((props (text-properties-at (1- (point))))
              (replacement (gethash (char-before) replacements)))
          (delete-char -1)
          (insert (apply #'propertize replacement props)))))))

(defun difffontify--latin-1-string (string)
  "Return STRING, with each character that is not in Latin 1 approximated.
See `difffontify--latin-1'."
  (with-temp-buffer
    (insert string)
    (difffontify--latin-1)
    (buffer-string)))

(defun difffontify--non-latin-1-chars ()
  "Return a list of the distinct characters in the buffer that are not in Latin 1."
  (save-excursion
    (goto-char (point-min))
    (let (chars)
      (while (re-search-forward "[^\u0000-\u00ff]" nil t)
        (cl-pushnew (char-before) chars))
      chars)))

(defun difffontify--latin-1-replacements (chars)
  "Return a hash table that maps each of CHARS to its Latin-1 approximation.
Run `difffontify-latin-1-command' on CHARS, one per line."
  (let ((table (make-hash-table))
        ;; A character that UTF-8 cannot encode, such as a raw byte, would make
        ;; the program read all of its input as Windows-1252.
        (unicode (cl-remove-if (lambda (c) (> c (max-char t))) chars)))
    (dolist (c chars)
      (puthash c "?" table))
    (when unicode
      (with-temp-buffer
        (insert (mapconcat #'string unicode "\n") "\n")
        (let* ((coding-system-for-write 'utf-8-unix)
               (coding-system-for-read 'latin-1-unix)
               (status (call-process-region (point-min) (point-max)
                                            difffontify-latin-1-command
                                            t t nil))
               (lines (split-string (buffer-string) "\n")))
          (unless (and (eql 0 status)
                       ;; The output ends with a newline.
                       (= (length lines) (1+ (length unicode))))
            (error "Program %s failed: %s" difffontify-latin-1-command
                   (string-trim (buffer-string))))
          (cl-mapc (lambda (c line) (puthash c line table))
                   unicode lines))))
    table))

;;; Locations

(defconst difffontify--hunk-header-re
  (concat diff-hunk-header-re "\\|^@@@+ .* @@@+")
  "Matches a hunk header, including one in a combined diff from \"git diff\".
In a combined diff (as for a merge), a hunk header such as
\"@@@ -1,3 -1,3 +1,4 @@@\" has one more \"@\" at each end than the diff
has parents, and each line of the hunk starts with one prefix character per
parent.")

(defun difffontify--mark-locations ()
  "Give each line of each hunk a `difffontify-location' property.
Its value is a cons (FILE . FUNCTION), such as (\"src/Foo.java\" .
\"bar() {\"), that holds the file and the function (or nil) at that line,
for the page header.  The hunk header is part of the hunk, and lines
outside hunks get no property.  The function comes from the last line of
the hunk, up to and including that line, that defines a function (that has
`font-lock-function-name-face'), or else from the function context in the
hunk header; see `difffontify--function'.  Call this after
`difffontify--flatten-faces', which gives that face to definitions only."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward difffontify--hunk-header-re nil t)
      (goto-char (match-beginning 0))
      (let* ((start (point))
             (prefix-length (difffontify--hunk-prefix-length))
             ;; At least the hunk header, so that the loop terminates.
             (end (max (save-excursion (difffontify--end-of-hunk))
                       (line-beginning-position 2)))
             (file (difffontify--hunk-file))
             (code (and file (difffontify--code-file-p file)))
             (function (difffontify--function (difffontify--hunk-function)
                                              code))
             (location (difffontify--location file function)))
        (while (< (point) end)
          (let ((bol (point))
                (eol (line-end-position)))
            (when (and (> bol start) (difffontify--defines-function-p bol eol))
              (setq function (difffontify--function
                              (buffer-substring-no-properties
                               (min (+ bol prefix-length) eol) eol)
                              code)
                    location (difffontify--location file function)))
            (forward-line 1)
            ;; Each line of the hunk until the next definition gets the same
            ;; cons, so the lines' text properties form one interval.
            (put-text-property bol (min (point) end)
                               'difffontify-location location)))
        (goto-char end)))))

(defun difffontify--location (file function)
  "Return the location for FILE and FUNCTION, which can be nil.
It is (FILE . FUNCTION), or nil if FILE is nil."
  (and file (cons file function)))

(defun difffontify--code-file-p (file)
  "True if FILE is in a programming language.
That is, the major mode that `auto-mode-alist' gives for FILE's name derives
from `prog-mode'.  This loads the major mode, if necessary."
  (let ((mode (assoc-default file auto-mode-alist #'string-match)))
    (and (symbolp mode)
         (fboundp mode)
         (progn
           (let ((definition (indirect-function mode)))
             (when (autoloadp definition)
               (autoload-do-load definition)))
           (provided-mode-derived-p mode 'prog-mode)))))

(defun difffontify--function (definition code)
  "Return DEFINITION, a line that defines a function, for a page header.
CODE is true if DEFINITION is in a programming language; then omit its
annotations, modifiers, and return type (see
`difffontify--simplify-definition').  Prose, such as the function context
that \"git diff\" writes for a Markdown file, is not simplified.  Also omit
leading and trailing whitespace, and replace each other run of spaces and
tabs by one space, because a page header has no tab stops.  Return nil if
DEFINITION is nil or nothing remains."
  (and definition
       (let ((result (string-trim
                      (replace-regexp-in-string "[ \t]+" " " definition))))
         (when code
           (setq result (difffontify--simplify-definition result)))
         (and (not (string-empty-p result)) result))))

(defconst difffontify--annotation-re
  (concat "@[[:alpha:]_$][[:alnum:]_$.]*"
          ;; Arguments, with parentheses nested at most one level.
          "\\(?:([^()]*\\(?:([^()]*)[^()]*\\)*)\\)? ?")
  "Matches a Java annotation and the space after it, if any.
Examples are \"@Override\" and \"@SuppressWarnings(\"unchecked\")\".  It also
matches the Java keyword \"@interface\".")

(defconst difffontify--modifiers-re
  (concat "\\`\\(?:"
          (regexp-opt '("public" "protected" "private" "static" "final"
                        "abstract" "synchronized" "native" "strictfp" "default"
                        "transient" "volatile" "sealed" "non-sealed")
                      "\\(?:")
          " \\)+")
  "Matches the Java modifiers, such as \"public static \", that start a line.")

(defconst difffontify--return-type-re
  (concat "\\`\\([^(=]*[] *&>]\\)"
          "\\([[:alpha:]_$~][[:alnum:]_$:~]*\\(?:<[^()]*>\\)? ?(\\)")
  "Matches a function definition that starts with a return type or keyword.
Examples are \"int f(\", \"Map<K, V> get(\", and \"def f(\".  Group 1
matches the text before the function name, and group 2 matches the function
name (and any type parameters) through the \"(\" that follows it.  Text that
contains \"=\" before the \"(\", as in JavaScript's \"const f = function(\",
is not a return type.")

(defconst difffontify--keep-before-name-re
  (concat "\\(?:\\`\\|[^[:alnum:]_$]\\)"
          (regexp-opt '("class" "interface" "enum" "record" "struct" "union"
                        "operator")
                      "\\(?:")
          " \\'")
  "Matches text that ends with a keyword that must stay before the next word.
Examples are \"class \", as in Python's \"class Foo(Base):\", and
\"operator \", as in C++'s \"operator int()\".")

(defconst difffontify--keyword-name-re
  (concat "\\`" (regexp-opt '("function" "async") "\\(?:") " ?(")
  "Matches a keyword and the \"(\" after it, at the start of a string.
An example is \"function(\", as in JavaScript's \"foo: function(x) {\".  The
keyword is not a function name.")

(defun difffontify--simplify-definition (definition)
  "Return DEFINITION, a line that defines a function, simplified.
DEFINITION has no tabs, no runs of spaces, and no leading or trailing
whitespace.  Omit Java annotations, leading Java modifiers, and the return
type or keyword (such as \"def\") before the function name.  For example,
return \"count(int x) {\" for \"@Pure public static int count(@NonNull int x)
{\"."
  (let* ((case-fold-search nil)
         (result
          (string-trim
           (replace-regexp-in-string
            " +" " "
            (replace-regexp-in-string
             difffontify--annotation-re
             (lambda (annotation)
               (if (equal (string-trim-right annotation) "@interface")
                   annotation
                 ""))
             definition)))))
    (setq result (replace-regexp-in-string difffontify--modifiers-re "" result))
    (when (and (string-match difffontify--return-type-re result)
               (not (string-match-p difffontify--keep-before-name-re
                                    (match-string 1 result)))
               (not (string-match-p difffontify--keyword-name-re
                                    (substring result (match-beginning 2)))))
      (setq result (substring result (match-beginning 2))))
    result))

(defun difffontify--hunk-prefix-length ()
  "Return the number of prefix characters in each line of the hunk at point.
For example, it is 1 for \"+\" in a unified diff, and 2 for \"! \" in a
context diff or \"< \" in a normal diff."
  (cond ((looking-at "@\\(@+\\)@ ") (length (match-string 1)))
        ((looking-at "@@ ") 1)
        (t 2)))

(defun difffontify--end-of-hunk ()
  "Return the end of the hunk whose header is at point."
  (if (looking-at "@@@")
      (progn
        (forward-line 1)
        (if (re-search-forward "^[^-+ \\]" nil t)
            (match-beginning 0)
          (point-max)))
    (diff-end-of-hunk)))

(defun difffontify--hunk-file ()
  "Return the name of the file that the hunk at point changes, or nil.
It is the new file, or the old file if the hunk deletes the file.  A \"git
diff\" prefix such as \"b/\" is omitted."
  (let ((name (cl-find "/dev/null" (diff-hunk-file-names) :test-not #'equal)))
    (and name (difffontify--strip-git-prefix (substring-no-properties name)))))

(defun difffontify--hunk-function ()
  "Return the function context in the hunk header at point, or nil.
\"diff -p\" and \"git diff\" write it after the line numbers, as in
\"@@ -10,7 +10,8 @@ void bar() {\"."
  (and (looking-at "\\(?:@@+ .*? @@+\\|\\*\\{15\\}\\)\\(.*\\)")
       (match-string-no-properties 1)))

(defun difffontify--defines-function-p (bol eol)
  "Return non-nil if the text from BOL to EOL defines a function.
That is, some of it has `font-lock-function-name-face', other than a LaTeX
environment name, as in \"\\begin{equation}\"."
  (save-excursion
    (let ((pos bol)
          found)
      (while (and (not found) (< pos eol))
        (goto-char pos)
        (if (and (memq 'font-lock-function-name-face
                       (get-text-property pos 'face))
                 (not (looking-back "\\\\\\(?:begin\\|end\\){" bol)))
            (setq found t)
          (setq pos (next-single-property-change pos 'face nil eol))))
      found)))

(defun difffontify--header ()
  "Return the parts of the left part of the current page's header.
The result is a list of four strings (TITLE FILE NAME PARAMETERS), any of
which but TITLE can be empty.  TITLE is `difffontify--header-title',
followed on the first page by the date.  On each other page, FILE and the
function come from the location (see `difffontify--mark-locations') at
point, if any:  NAME is \":  \" and the function up to the \"(\" that
starts its formal parameters, and PARAMETERS is the rest of the function.
For example, the parts can be (\"x.diff      \" \"src/Foo.java\" \":  get\"
\"(int n) {\").  Call this at the start of each page, with point at the
beginning of the page's first line."
  (let ((location (and (not (bobp))
                       (get-text-property (point) 'difffontify-location)))
        (separator "      "))
    (cond ((bobp)
           (list (concat difffontify--header-title separator
                         (format-time-string "%Y-%m-%d"))
                 "" "" ""))
          ((null location)
           (list difffontify--header-title "" "" ""))
          (t
           (let ((function (cdr location)))
             (if (and function
                      ;; A "(" before the function name, as in Lisp's "(defun
                      ;; foo (n)", does not start the formal parameters.
                      (string-match "\\`[^(]*[[:alnum:]_$>][ \t]*(" function))
                 (list (concat difffontify--header-title separator)
                       (car location)
                       (concat ":  " (substring function 0 (1- (match-end 0))))
                       (substring function (1- (match-end 0))))
               (list (concat difffontify--header-title separator)
                     (car location)
                     (if function (concat ":  " function) "")
                     "")))))))

(defun difffontify--output-header ()
  "Write PostScript that sets the current page's `DifffontifyHeaderParts'.
Its value is an array of the strings that `difffontify--header' returns.
`ps-print' calls this at the start of each page, with point at the beginning
of the page's first line."
  (ps-output "/DifffontifyHeaderParts [")
  (dolist (part (difffontify--header))
    ;; `ps-output-string' would write each non-ASCII character of a
    ;; multibyte string as its UTF-8 bytes, but the header font uses Latin 1.
    (ps-output-string (encode-coding-string part 'latin-1))
    (ps-output " "))
  (ps-output "] def\n"))

(defconst difffontify--header-prologue
  "% The parts of the left part of the current page's header:
% [TITLE FILE NAME PARAMETERS], as in Emacs's `difffontify--header'.
/DifffontifyHeaderParts [() () () ()] def

% string -- width
/DfWidth{stringwidth pop}def

% string1 ... stringN N -- string1...stringN
% Return the concatenation of N strings, where N is at least 1.
/DfConcat{1 sub{strcat}repeat}def

% string max -- length
% Return the length of the longest prefix of string that is at most max wide.
/DfPrefixLength{
  /DfMax exch def
  dup length
  {dup 0 le{exit}if
   2 copy 0 exch getinterval DfWidth DfMax le{exit}if
   1 sub}loop
  exch pop
}def

% string max at-slash -- start true
% string max at-slash -- false
% Return the start of the longest suffix of string that is at most max wide.
% If at-slash is true, the suffix must start with \"/\".  Return false if there
% is no such suffix.
/DfSuffixStart{
  /DfAtSlash exch def /DfMax exch def /DfString exch def
  false
  0 1 DfString length{
    /DfStart exch def
    DfAtSlash{
      DfStart DfString length lt{DfString DfStart get 47 eq}{false}ifelse
    }{true}ifelse{
      DfString DfStart DfString length DfStart sub getinterval
      DfWidth DfMax le{pop DfStart true exit}if
    }if
  }for
}def

% -- string
% Return the left part of the current page's header, from
% DifffontifyHeaderParts, shortened if necessary so that it fits beside the
% page number.  To shorten it, first replace the end of PARAMETERS by \"...\",
% keeping the \"(\" and at least one character after it.  If that is not
% enough, omit PARAMETERS, and if that is not enough, also replace the start
% of FILE by \"...\", leaving whole path components if possible.  If that is
% not enough, replace the end of the whole header by \"...\".  Call this while
% showing the header, when the header font is the current font.
/DifffontifyHeader{
  /DfAvailable
    PrintHeaderWidth HFPad 2 mul sub
    pagenumberstring DfWidth sub
    (    ) DfWidth sub
  def
  DifffontifyHeaderParts aload pop
  /DfParameters exch def /DfName exch def /DfFile exch def /DfTitle exch def
  % ---- the width of the parts that are never shortened, except as a whole
  /DfFixed DfTitle DfWidth DfName DfWidth add def
  DfFixed DfFile DfWidth add DfParameters DfWidth add DfAvailable le{
    DfTitle DfFile DfName DfParameters 4 DfConcat
  }{
    % ---- the length of the prefix of the formal parameters that fits
    DfParameters
    DfAvailable DfFixed sub DfFile DfWidth sub (...) DfWidth sub
    DfPrefixLength
    dup 2 ge{
      % ---- shorten the formal parameters
      DfParameters 0 3 -1 roll getinterval
      DfTitle DfFile DfName 4 -1 roll (...) 5 DfConcat
    }{
      pop
      DfFixed DfFile DfWidth add DfAvailable le{
        % ---- omit the formal parameters
        DfTitle DfFile DfName 3 DfConcat
      }{
        /DfFileMax DfAvailable DfFixed sub (...) DfWidth sub def
        DfFile length 0 gt DfFileMax 0 ge and{
          % ---- shorten the file name too
          DfFile DfFileMax true DfSuffixStart
          not{DfFile DfFileMax false DfSuffixStart pop}if
          DfFile exch dup DfFile length exch sub getinterval
          DfTitle (...) 3 -1 roll DfName 4 DfConcat
        }{
          DfTitle DfFile DfName DfParameters 4 DfConcat
          dup DfAvailable (...) DfWidth sub DfPrefixLength
          0 exch getinterval (...) strcat
        }ifelse
      }ifelse
    }ifelse
  }ifelse
}def
"
  "PostScript code that `ps-print' puts in its prologue, for the page header.
Emacs does not know the widths of the characters in the header font.")

;;; Wrapping

(defun difffontify--line-width ()
  "Return the number of characters that fit on a printed line."
  (floor (- (nth 1 (assq difffontify--paper-type ps-page-dimensions-database))
            (* 2 difffontify--margin))
         ;; Each Courier character is 0.6 em wide.
         (* 0.6 difffontify--font-size)))

(defun difffontify--wrap-lines (width)
  "Wrap lines that are wider than WIDTH columns.
Each wrapped line ends with \"\\\" in the rightmost column, so that it is
not confused with a \"\\\" in the text, and each continuation line starts
with \"»\".  In a hunk, the \"»\" replaces the prefix character.  The marks
are in the regular font, but on a deleted line they are shaded."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((bol (point)))
      (end-of-line)
      (if (<= (current-column) width)
          (forward-line 1)
        (let* ((prefix-faces (get-text-property bol 'face))
               (hunk-line (cl-intersection prefix-faces
                                           '(diff-indicator-removed
                                             diff-indicator-added
                                             diff-context)))
               (mark-faces (and (memq 'diff-indicator-removed prefix-faces)
                                '(diff-removed))))
          (goto-char (difffontify--break-position bol width))
          (insert (propertize (concat (make-string
                                       (max 0 (- width 1 (current-column))) ?\s)
                                      "\\\n")
                              'face mark-faces)
                  (propertize "»"
                              'face (and hunk-line prefix-faces)
                              'difffontify-location
                              (get-text-property bol 'difffontify-location)))
          ;; The continuation line might need to be wrapped too.
          (forward-line 0))))))

(defun difffontify--break-position (bol width)
  "Return the position at which to wrap the line that starts at BOL.
Text before the position fits in WIDTH - 1 columns, leaving room for the
\"\\\".  Break after the last space or tab in the second half of the line, if
any."
  (goto-char bol)
  (move-to-column (1- width))
  ;; A tab can extend past the column.
  (when (> (current-column) (1- width))
    (backward-char))
  ;; At least 2 characters precede the break, so the continuation line, which
  ;; starts with a 1-character mark, is shorter than this line.  Otherwise, a
  ;; character too wide to fit after the mark would cause an infinite loop.
  (let ((end (max (point) (min (+ bol 2) (line-end-position))))
        (limit (progn (move-to-column (/ width 2)) (point))))
    (goto-char end)
    (if (and (< limit end) (re-search-backward "[ \t]" limit t))
        (1+ (point))
      end)))


;;; Output

(defun difffontify--write-pdf (pdf title color)
  "Write the current buffer's diff to file PDF, with page header TITLE.
Use color if COLOR is non-nil."
  (unless (derived-mode-p 'diff-mode)
    (user-error "Not in a diff buffer"))
  (unless (executable-find "ps2pdf")
    (user-error "Program ps2pdf not found; install Ghostscript"))
  (unless (executable-find difffontify-latin-1-command)
    (user-error "Program %s not found" difffontify-latin-1-command))
  (let* ((buffer (difffontify--prepare color))
         (dir (make-temp-file "difffontify-" t))
         (ps (expand-file-name "diff.ps" dir))
         ;; Ghostscript writes the PDF file in the temporary directory, and
         ;; Emacs moves it into place, because a security policy may forbid
         ;; Ghostscript to write to PDF's directory.  For example, Ubuntu's
         ;; AppArmor profile for Ghostscript forbids writing under ~/bin/.
         (temp-pdf (expand-file-name "diff.pdf" dir)))
    (unwind-protect
        (progn
          (difffontify--write-ps buffer ps title color)
          (with-temp-buffer
            ;; The PostScript file does not specify a paper size, and
            ;; Ghostscript's default might differ.
            (unless (eql 0 (call-process
                            "ps2pdf" nil t nil
                            (format "-sPAPERSIZE=%s" difffontify--paper-type)
                            ps temp-pdf))
              (when (file-exists-p pdf)
                (delete-file pdf))
              (error "Program ps2pdf failed: %s"
                     (string-trim (buffer-string)))))
          (rename-file temp-pdf pdf t))
      (kill-buffer buffer)
      (delete-directory dir t))))

(defun difffontify--write-ps (buffer ps title color)
  "Write BUFFER, prepared by `difffontify--prepare', to PostScript file PS.
TITLE is the page header.  Use color if COLOR is non-nil."
  (with-current-buffer buffer
    (let ((difffontify--header-title (difffontify--latin-1-string title))
          ;; The `black-white' mode prints each face according to
          ;; `ps-black-white-faces' rather than according to its appearance on
          ;; the screen, so the output does not depend on the user's faces.
          ;; Despite the name, the specifications can have colors.
          (ps-print-color-p 'black-white)
          (ps-black-white-faces (difffontify--faces color))
          (ps-use-face-background t)
          (ps-default-fg nil)
          (ps-default-bg nil)
          (ps-paper-type difffontify--paper-type)
          (ps-landscape-mode nil)
          (ps-number-of-columns 1)
          (ps-n-up-printing 1)
          (ps-font-family 'Courier)
          (ps-font-size difffontify--font-size)
          (ps-left-margin difffontify--margin)
          (ps-right-margin difffontify--margin)
          (ps-top-margin difffontify--margin)
          (ps-bottom-margin difffontify--margin)
          (ps-line-spacing 0)
          (ps-paragraph-spacing 0)
          (ps-print-header t)
          (ps-print-header-frame nil)
          (ps-print-only-one-header nil)
          (ps-print-footer nil)
          (ps-header-lines 1)
          (ps-header-offset 10)
          (ps-header-line-pad 0.15)
          (ps-header-font-family 'Helvetica)
          (ps-header-font-size difffontify--header-font-size)
          (ps-header-title-font-size difffontify--header-font-size)
          (ps-user-defined-prologue difffontify--header-prologue)
          ;; This hook runs at the start of each page, because each sheet of
          ;; paper holds one page.
          (ps-print-begin-sheet-hook (list #'difffontify--output-header))
          ;; ps-print executes a procedure when it shows the header.
          (ps-left-header '("/DifffontifyHeader load"))
          (ps-right-header '("/pagenumberstring load"))
          (ps-show-n-of-n t)
          (ps-selected-pages nil)
          (ps-even-or-odd-pages nil)
          (ps-banner-page-when-duplexing nil)
          (ps-print-control-characters 'control-8-bit)
          (ps-multibyte-buffer nil)
          (ps-zebra-stripes nil)
          (ps-line-number nil)
          (ps-print-background-image nil)
          (ps-print-background-text nil)
          (ps-spool-duplex nil)
          (ps-razzle-dazzle nil))
      ;; `ps-print' ignores colors if `display-color-p' is nil, as in batch
      ;; mode, and `color-values' approximates colors on a terminal (for
      ;; example, "#e6e6e6" becomes white).  All the colors in
      ;; `difffontify--faces' are "#RRGGBB", which `tty-color-standard-values'
      ;; converts exactly.
      (cl-letf (((symbol-function 'display-color-p) (lambda (&rest _) t))
                ((symbol-function 'color-values)
                 (lambda (color &rest _) (tty-color-standard-values color))))
        ;; `difffontify--header' recognizes the first page by point being at
        ;; the beginning of the buffer.  `ps-print' moves point to the start
        ;; of each later page, but not to the start of the first page.
        (save-excursion
          (goto-char (point-min))
          (ps-print-buffer-with-faces ps))))))

(provide 'difffontify)

;;; difffontify.el ends here
