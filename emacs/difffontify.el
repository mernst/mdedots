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
;; From the command line, to write FILE.diff.pdf:
;;   emacs -Q --batch -l difffontify.el -f difffontify-batch-pdf FILE.diff ...
;;
;; The design is described in difffontify-plan.md.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'ps-print)

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

(defconst difffontify--paper-type 'letter)
(defconst difffontify--font-size 8.7)
(defconst difffontify--margin 36
  "Page margins, in points.")

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

(defvar difffontify--page-header nil
  "The page header, for `ps-left-header'.")


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
for COLOR, long lines are wrapped, and the \"Diff finished\" line that ends a
`*Diff*' buffer is omitted.  The caller must kill the buffer."
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
            (orig-fontify-props (symbol-function 'diff-syntax-fontify-props)))
        (delay-mode-hooks (diff-mode))
        (setq-local diff-default-directory dir)
        (setq tab-width 8)
        (cl-letf (((symbol-function 'diff-syntax-fontify-props)
                   (lambda (file text line-nb &optional hunk-only)
                     (difffontify--fontify-props
                      orig-fontify-props file text line-nb hunk-only))))
          ;; Major modes for the compared files print messages, such as
          ;; Python's "Can't guess python-indent-offset".
          (let ((inhibit-message t))
            (font-lock-ensure))))
      (difffontify--embolden-diff-lines)
      (difffontify--flatten-faces (difffontify--faces color))
      (difffontify--latin-1)
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

(defconst difffontify--latin-1-replacements
  (let ((greek '((?Α . "Alpha") (?α . "alpha") (?Β . "Beta") (?β . "beta")
                 (?Γ . "Gamma") (?γ . "gamma") (?Δ . "Delta") (?δ . "delta")
                 (?Ε . "Epsilon") (?ε . "epsilon") (?Ζ . "Zeta") (?ζ . "zeta")
                 (?Η . "Eta") (?η . "eta")
                 (?Θ . "Theta") (?θ . "theta") (?ϑ . "theta")
                 (?Ι . "Iota") (?ι . "iota") (?Κ . "Kappa") (?κ . "kappa")
                 (?Λ . "Lambda") (?λ . "lambda") (?Μ . "Mu") (?μ . "mu")
                 (?Ν . "Nu") (?ν . "nu") (?Ξ . "Xi") (?ξ . "xi")
                 (?Ο . "Omicron") (?ο . "omicron") (?Π . "Pi") (?π . "pi")
                 (?Ρ . "Rho") (?ρ . "rho")
                 (?Σ . "Sigma") (?σ . "sigma") (?ς . "sigma")
                 (?Τ . "Tau") (?τ . "tau") (?Υ . "Upsilon") (?υ . "upsilon")
                 (?Φ . "Phi") (?φ . "phi") (?ϕ . "phi")
                 (?Χ . "Chi") (?χ . "chi") (?Ψ . "Psi") (?ψ . "psi")
                 (?Ω . "Omega") (?ω . "omega")))
        (logic '((?∈ . "in") (?∉ . "notin") (?∀ . "forall") (?∃ . "exists")
                 (?∅ . "{}") (?⊆ . "subseteq") (?⊂ . "subset")
                 (?⊇ . "supseteq") (?⊃ . "supset")
                 (?∪ . "union") (?∩ . "intersect") (?∧ . "/\\") (?∨ . "\\/")
                 (?⊢ . "|-") (?↦ . "|->") (?≡ . "==") (?≈ . "~=") (?∘ . "o")
                 (?∞ . "infinity") (?⊤ . "T") (?⊥ . "_|_")
                 (?⊏ . "&sqsub;") (?⊑ . "&sqsube;")
                 (?⊐ . "&sqsup;") (?⊒ . "&sqsupe;")
                 (?≤ . "<=") (?≥ . ">=") (?≠ . "!=") (?− . "-")
                 (?→ . "->") (?← . "<-") (?↔ . "<->")
                 (?⇒ . "=>") (?⇐ . "<=") (?⇔ . "<=>") (?⟨ . "<") (?⟩ . ">")))
        (punctuation '((?‘ . "'") (?’ . "'") (?‚ . "'")
                       (?′ . "'") (?″ . "''")
                       (?“ . "\"") (?” . "\"") (?„ . "\"")
                       (?‐ . "-") (?‑ . "-") (?– . "-") (?— . "--")
                       (?… . "...") (?• . "o") (?◦ . "o")
                       (?‣ . ">") (?▶ . ">") (?▸ . ">")
                       (?★ . "*") (?⚠ . "!") (?€ . "EUR") (?™ . "(TM)")
                       (?\N{ZERO WIDTH SPACE} . "")
                       (?\N{ZERO WIDTH NO-BREAK SPACE} . "")))
        (boxes '((?─ . "-") (?│ . "|")
                 (?┌ . "+") (?┐ . "+") (?└ . "+") (?┘ . "+")
                 (?├ . "+") (?┤ . "+") (?┬ . "+") (?┴ . "+") (?┼ . "+")
                 (?═ . "=") (?║ . "|")
                 (?╔ . "+") (?╗ . "+") (?╚ . "+") (?╝ . "+")
                 (?╠ . "+") (?╣ . "+") (?╦ . "+") (?╩ . "+") (?╬ . "+")
                 (?█ . "X")))
        (marks '((?✓ . "yes") (?✔ . "yes") (?✅ . "yes")
                 (?✗ . "no") (?✘ . "no") (?❌ . "no"))))
    (append greek logic punctuation boxes marks))
  "Replacements for characters that are not in Latin 1.
`ps-print' prints such characters as \"?\".  The replacements are the same
as in the shell function `iconv_latin_1' in ~/.aliases.")

(defun difffontify--latin-1 ()
  "Replace each character that is not in Latin 1 by an approximation, or \"?\".
The replacement has the same text properties as the character."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^\u0000-\u00ff]" nil t)
      (let* ((char (char-before))
             (props (text-properties-at (1- (point))))
             (replacement (or (alist-get char difffontify--latin-1-replacements)
                              (difffontify--decompose char)
                              "?")))
        (delete-char -1)
        (insert (apply #'propertize replacement props))))))

(defun difffontify--decompose (char)
  "Return CHAR's decomposition, without combining marks, if it is in Latin 1.
For example, \"ő\" becomes \"o\" and \"ﬁ\" becomes \"fi\".  Return nil if
CHAR has no such Unicode decomposition."
  (let ((chars (cl-remove-if
                (lambda (c)
                  (or (symbolp c)
                      (eq (get-char-code-property c 'general-category) 'Mn)))
                (get-char-code-property char 'decomposition))))
    (and chars
         (not (equal chars (list char)))
         (cl-every (lambda (c) (<= c #xff)) chars)
         (apply #'string chars))))


;;; Wrapping

(defun difffontify--line-width ()
  "Return the number of characters that fit on a printed line."
  (floor (- (nth 1 (assq difffontify--paper-type ps-page-dimensions-database))
            (* 2 difffontify--margin))
         ;; Each Courier character is 0.6 em wide.
         (* 0.6 difffontify--font-size)))

(defun difffontify--wrap-lines (width)
  "Wrap lines that are wider than WIDTH columns.
Each wrapped line ends with \"\\\", and each continuation line starts with
\"»\".  In a hunk, the continuation line starts with the original prefix
character, before the \"»\".  The marks are in the regular font, but on a
deleted line they are shaded."
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
          (insert (propertize "\\\n" 'face mark-faces))
          (when hunk-line
            (insert (propertize (string (char-after bol)) 'face prefix-faces)))
          (insert (propertize "»" 'face mark-faces))
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
  ;; At least 3 characters precede the break, so the continuation line, which
  ;; starts with at most 2 characters of marks, is shorter than this line.
  ;; Otherwise, a character too wide to fit after the marks would cause an
  ;; infinite loop.
  (let ((end (max (point) (min (+ bol 3) (line-end-position))))
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
  (let ((buffer (difffontify--prepare color))
        (ps (make-temp-file "difffontify-" nil ".ps")))
    (unwind-protect
        (progn
          (difffontify--write-ps buffer ps title color)
          (with-temp-buffer
            ;; The PostScript file does not specify a paper size, and
            ;; Ghostscript's default might differ.
            (unless (eql 0 (call-process
                            "ps2pdf" nil t nil
                            (format "-sPAPERSIZE=%s" difffontify--paper-type)
                            ps pdf))
              (when (file-exists-p pdf)
                (delete-file pdf))
              (error "Program ps2pdf failed: %s"
                     (string-trim (buffer-string))))))
      (kill-buffer buffer)
      (delete-file ps))))

(defun difffontify--write-ps (buffer ps title color)
  "Write BUFFER, prepared by `difffontify--prepare', to PostScript file PS.
TITLE is the page header.  Use color if COLOR is non-nil."
  (with-current-buffer buffer
    (let ((difffontify--page-header title)
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
          (ps-header-font-size 10)
          (ps-header-title-font-size 10)
          (ps-left-header '(difffontify--page-header))
          (ps-right-header '("/pagenumberstring load"))
          (ps-show-n-of-n t)
          (ps-selected-pages nil)
          (ps-even-or-odd-pages nil)
          (ps-banner-page-when-duplexing nil)
          (ps-user-defined-prologue nil)
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
        (ps-print-buffer-with-faces ps)))))

(provide 'difffontify)

;;; difffontify.el ends here
