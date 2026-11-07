;;; difffontify-tests.el --- Tests for difffontify  -*- lexical-binding: t -*-

;;; Commentary:

;; Run the tests with:
;;   emacs --batch -L . -l difffontify-tests.el -f ert-run-tests-batch-and-exit
;;
;; The test inputs are in difffontify-tests/.  langs.diff is the output of
;; "diff -u -b -r old new" in that directory, and nofiles.diff is the same
;; diff with the directory names changed to ones that do not exist.  git.diff
;; is the output of "git diff --no-index old/Foo.java new/Foo.java".  Each
;; .goal file holds the expected text of the buffer that
;; `difffontify--prepare' returns, with each run of text that has faces
;; written as ⟨FACES|TEXT⟩ (see `difffontify-tests--markup').
;;
;; To regenerate the .goal files, set the environment variable
;; DIFFFONTIFY_UPDATE_GOALS to a non-empty value and run the tests.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'difffontify)

(defconst difffontify-tests--dir
  (expand-file-name "difffontify-tests/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The directory that holds the test inputs and .goal files.")

(defface difffontify-tests--keyword-like '((t :inherit font-lock-keyword-face))
  "A face that inherits from `font-lock-keyword-face'."
  :group 'difffontify)


;;; Goal files

(defun difffontify-tests--face-name (face)
  "Return an abbreviation of the name of FACE."
  (string-remove-suffix "-face"
                        (string-remove-prefix "font-lock-" (symbol-name face))))

(defun difffontify-tests--markup ()
  "Return the current buffer's text, with runs of text that have faces marked.
A run with faces F1 and F2 is written as ⟨F1+F2|TEXT⟩, with abbreviated face
names.  A run is the longest text whose faces are `equal', and is split at
newlines, so each line of the result corresponds to a line of the buffer."
  (let ((pos (point-min))
        parts)
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'face nil (point-max)))
            (faces (get-text-property pos 'face)))
        (while (and (< next (point-max))
                    (equal (get-text-property next 'face) faces))
          (setq next (next-single-property-change next 'face nil (point-max))))
        (push (if (null faces)
                  (buffer-substring-no-properties pos next)
                (let ((names (mapconcat #'difffontify-tests--face-name
                                        faces "+")))
                  (mapconcat (lambda (line)
                               (if (string-empty-p line)
                                   ""
                                 (format "⟨%s|%s⟩" names line)))
                             (split-string
                              (buffer-substring-no-properties pos next) "\n")
                             "\n")))
              parts)
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun difffontify-tests--check-goal (diff-file goal-file color)
  "Check that preparing DIFF-FILE, with COLOR, yields the contents of GOAL-FILE.
Both names are relative to `difffontify-tests--dir'."
  (let* ((goal-file (expand-file-name goal-file difffontify-tests--dir))
         (diff-buffer (find-file-noselect
                       (expand-file-name diff-file difffontify-tests--dir)))
         (actual (with-current-buffer diff-buffer
                   (let ((buffer (difffontify--prepare color)))
                     (unwind-protect
                         (with-current-buffer buffer
                           (difffontify-tests--markup))
                       (kill-buffer buffer))))))
    (kill-buffer diff-buffer)
    (if (not (string-empty-p (or (getenv "DIFFFONTIFY_UPDATE_GOALS") "")))
        (with-temp-file goal-file
          (insert actual))
      (should (equal actual
                     (with-temp-buffer
                       (insert-file-contents goal-file)
                       (buffer-string)))))))

(ert-deftest difffontify-tests-langs ()
  "Highlighting using the real files, in black and white."
  (difffontify-tests--check-goal "langs.diff" "langs.goal" nil))

(ert-deftest difffontify-tests-langs-color ()
  "Highlighting using the real files, in color."
  (difffontify-tests--check-goal "langs.diff" "langs-color.goal" t))

(ert-deftest difffontify-tests-nofiles ()
  "Highlighting using only the hunk text, because the files do not exist."
  (difffontify-tests--check-goal "nofiles.diff" "nofiles.goal" nil))


;;; File names

(defun difffontify-tests--diff-buffer (old new)
  "Return a `*Diff*' buffer comparing OLD and NEW in `difffontify-tests--dir'.
The caller must kill the buffer."
  (let ((default-directory difffontify-tests--dir)
        (diff-switches '("-u" "-b" "-r")))
    (diff-no-select (expand-file-name old) (expand-file-name new) nil t
                    (generate-new-buffer "*Diff test*"))))

(ert-deftest difffontify-tests-pdf-file-name-diff-file ()
  "The PDF for a diff file is named after the diff file."
  (let ((buffer (find-file-noselect
                 (expand-file-name "langs.diff" difffontify-tests--dir))))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (difffontify--pdf-file-name)
                         (expand-file-name "langs.diff.pdf"
                                           difffontify-tests--dir)))
          (should (equal (difffontify--title (difffontify--pdf-file-name))
                         "langs.diff")))
      (kill-buffer buffer))))

(ert-deftest difffontify-tests-pdf-file-name-diff-buffer-files ()
  "The PDF for a `*Diff*' buffer comparing files is named after the new file."
  (let ((buffer (difffontify-tests--diff-buffer "old/Foo.java" "new/Foo.java")))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (difffontify--pdf-file-name)
                         (expand-file-name "Foo.java.diff.pdf"
                                           difffontify-tests--dir)))
          (should (equal (difffontify--title (difffontify--pdf-file-name))
                         "Foo.java.diff")))
      (kill-buffer buffer))))

(ert-deftest difffontify-tests-pdf-file-name-diff-buffer-directories ()
  "The PDF for a `*Diff*' buffer comparing directories is named after the new."
  (let ((buffer (difffontify-tests--diff-buffer "old" "new")))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (difffontify--pdf-file-name)
                         (expand-file-name "new.diff.pdf"
                                           difffontify-tests--dir))))
      (kill-buffer buffer))))

(ert-deftest difffontify-tests-diff-buffer-uses-files ()
  "In a `*Diff*' buffer, a hunk inside a Java block comment is a comment."
  (let ((buffer (difffontify-tests--diff-buffer "old/Foo.java" "new/Foo.java")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((prepared (difffontify--prepare nil)))
            (unwind-protect
                (with-current-buffer prepared
                  (goto-char (point-min))
                  (search-forward "License line 12, revised.")
                  (should (memq 'font-lock-comment-face
                                (get-text-property (match-beginning 0)
                                                   'face))))
              (kill-buffer prepared))))
      (kill-buffer buffer))))

(ert-deftest difffontify-tests-git-diff-uses-files ()
  "In a \"git diff\" file, the new file \"b/new/Foo.java\" is \"new/Foo.java\"."
  (let ((buffer (find-file-noselect
                 (expand-file-name "git.diff" difffontify-tests--dir))))
    (unwind-protect
        (with-current-buffer buffer
          (let ((prepared (difffontify--prepare nil)))
            (unwind-protect
                (with-current-buffer prepared
                  (goto-char (point-min))
                  (search-forward "License line 12, revised.")
                  (should (memq 'font-lock-comment-face
                                (get-text-property (match-beginning 0)
                                                   'face))))
              (kill-buffer prepared))))
      (kill-buffer buffer))))

(ert-deftest difffontify-tests-diff-finished ()
  "The \"Diff finished\" line at the end of a `*Diff*' buffer is omitted."
  (let ((buffer (difffontify-tests--diff-buffer "old/foo.sh" "new/foo.sh")))
    (unwind-protect
        (with-current-buffer buffer
          (should (string-match-p "\n\nDiff finished" (buffer-string)))
          (let ((prepared (difffontify--prepare nil)))
            (unwind-protect
                (with-current-buffer prepared
                  (should (string-suffix-p " A heredoc line.\n"
                                           (buffer-string))))
              (kill-buffer prepared))))
      (kill-buffer buffer))))


;;; Faces

(ert-deftest difffontify-tests-spec-face ()
  "A face maps to itself or to the nearest listed face it inherits from."
  (should (eq (difffontify--spec-face 'font-lock-keyword-face
                                      difffontify--mono-faces)
              'font-lock-keyword-face))
  (should (eq (difffontify--spec-face 'difffontify-tests--keyword-like
                                      difffontify--mono-faces)
              'font-lock-keyword-face))
  ;; `font-lock-function-call-face' inherits from
  ;; `font-lock-function-name-face', but is listed itself.
  (should (eq (difffontify--spec-face 'font-lock-function-call-face
                                      difffontify--mono-faces)
              'font-lock-function-call-face))
  (should (null (difffontify--spec-face 'font-lock-variable-name-face
                                        difffontify--mono-faces)))
  (should (null (difffontify--spec-face 'no-such-face
                                        difffontify--mono-faces))))

(ert-deftest difffontify-tests-spec-faces ()
  "A `face' property value maps to a list of listed faces."
  (should (equal (difffontify--spec-faces '(font-lock-keyword-face diff-removed)
                                          difffontify--mono-faces)
                 '(font-lock-keyword-face diff-removed)))
  (should (equal (difffontify--spec-faces
                  '(:inherit font-lock-comment-face :weight bold)
                  difffontify--mono-faces)
                 '(font-lock-comment-face)))
  (should (equal (difffontify--spec-faces '(foreground-color . "red")
                                          difffontify--mono-faces)
                 nil)))

(defun difffontify-tests--contrast (color)
  "Return the contrast ratio of COLOR, a \"#RRGGBB\" string, against white."
  (let* ((channels
          (mapcar (lambda (i)
                    (let ((c (/ (string-to-number
                                 (substring color i (+ i 2)) 16)
                                255.0)))
                      (if (<= c 0.03928)
                          (/ c 12.92)
                        (expt (/ (+ c 0.055) 1.055) 2.4))))
                  '(1 3 5)))
         (luminance (+ (* 0.2126 (nth 0 channels))
                       (* 0.7152 (nth 1 channels))
                       (* 0.0722 (nth 2 channels)))))
    (/ 1.05 (+ luminance 0.05))))

(ert-deftest difffontify-tests-color-contrast ()
  "Each foreground color has a contrast ratio of at least 7:1 against white."
  (dolist (spec difffontify--color-faces)
    (when (nth 1 spec)
      (should (>= (difffontify-tests--contrast (nth 1 spec)) 7)))))

(ert-deftest difffontify-tests-same-fonts ()
  "The color and black-and-white specifications give each face the same font."
  (dolist (spec difffontify--mono-faces)
    (should (equal (nthcdr 3 spec)
                   (nthcdr 3 (assq (car spec) difffontify--color-faces))))))


;;; Latin 1 and wrapping

(ert-deftest difffontify-tests-latin-1 ()
  "Characters outside Latin 1 are replaced, keeping their text properties."
  (with-temp-buffer
    (insert "é “λ” " (propertize "∀" 'face '(font-lock-keyword-face))
            " ő Ł λ 中 " (unibyte-char-to-multibyte #x81))
    (difffontify--latin-1)
    (should (equal (buffer-string) "é \"lambda\" forall o L lambda ? ?"))
    (goto-char (point-min))
    (search-forward "forall")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(font-lock-keyword-face)))
    (should (equal (get-text-property (1- (match-end 0)) 'face)
                   '(font-lock-keyword-face)))))

(ert-deftest difffontify-tests-wrap ()
  "Long lines are wrapped at a space, with marks, shaded on a deleted line."
  (with-temp-buffer
    (insert (propertize "-" 'face '(diff-indicator-removed))
            (propertize "aaa bbbb cccc dddd\n" 'face '(diff-removed))
            "header line that is long\n")
    (difffontify--wrap-lines 12)
    (should (equal (buffer-string)
                   (concat "-aaa bbbb  \\\n»cccc dddd\n"
                           "header     \\\n»line that \\\n»is long\n")))
    (goto-char (point-min))
    (search-forward "bbbb  \\")
    (should (equal (get-text-property (- (match-end 0) 2) 'face)
                   '(diff-removed)))
    (should (equal (get-text-property (1- (match-end 0)) 'face)
                   '(diff-removed)))
    (search-forward "»")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(diff-indicator-removed)))
    (search-forward "header     \\")
    (should (null (get-text-property (1- (match-end 0)) 'face)))))

(ert-deftest difffontify-tests-wrap-tab ()
  "A tab that extends past the wrap column is moved to the continuation line."
  (with-temp-buffer
    ;; The tab occupies columns 10 through 15.
    (insert "+abcdefghi\tjklmnop\n")
    (difffontify--wrap-lines 12)
    (should (equal (buffer-string) "+abcdefghi \\\n»\tjkl\\\n»mnop\n"))))


;;; Page headers

(defun difffontify-tests--call-prepared (function)
  "Call FUNCTION in the black-and-white prepared copy of the current buffer.
That is, in the buffer that `difffontify--prepare' returns.  Return the
result of FUNCTION, and kill the prepared buffer."
  (let ((buffer (difffontify--prepare nil)))
    (unwind-protect
        (with-current-buffer buffer
          (funcall function))
      (kill-buffer buffer))))

(defun difffontify-tests--locations ()
  "Return the `difffontify-location' properties in the current buffer.
The result is a list of elements (LINE . LOCATION), one for each line whose
location differs from the previous line's, where LINE is a line number such
as 1 for the first line.  LOCATION is a string such as \"src/Foo.java:
bar() {\" or \"src/Foo.java\", or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((previous 'none)
          result)
      (while (not (eobp))
        (let* ((property (get-text-property (point) 'difffontify-location))
               (location (if (cdr property)
                             (concat (car property) ":  " (cdr property))
                           (car property))))
          (unless (equal location previous)
            (push (cons (line-number-at-pos) location) result)
            (setq previous location)))
        (forward-line 1))
      (nreverse result))))

(defun difffontify-tests--text-locations (&rest lines)
  "Return the locations, as by `difffontify-tests--locations', for LINES.
LINES are the lines of a diff, without newlines.  The locations are those in
the buffer that `difffontify--prepare' returns for the diff."
  (with-temp-buffer
    (insert (mapconcat (lambda (line) (concat line "\n")) lines))
    (delay-mode-hooks (diff-mode))
    (difffontify-tests--call-prepared #'difffontify-tests--locations)))

(ert-deftest difffontify-tests-locations-git ()
  "Locations come from file headers, hunk headers, and definitions in hunks."
  (should (equal (difffontify-tests--text-locations
                  "diff --git a/Bar.java b/Bar.java"
                  "index 1111111..2222222 100644"
                  "--- a/Bar.java"
                  "+++ b/Bar.java"
                  "@@ -5,6 +5,7 @@ public class Bar {"
                  "   int x;"
                  " "
                  "   void first() {"
                  "+    x++;"
                  "   }"
                  " "
                  "   void second() {"
                  "diff --git a/Gone.py b/Gone.py"
                  "deleted file mode 100644"
                  "index 3333333..0000000"
                  "--- a/Gone.py"
                  "+++ /dev/null"
                  "@@ -1,2 +0,0 @@"
                  "-def gone():"
                  "-    pass")
                 '((1 . nil)
                   (5 . "Bar.java:  class Bar {")
                   (8 . "Bar.java:  first() {")
                   (12 . "Bar.java:  second() {")
                   (13 . nil)
                   (18 . "Gone.py")
                   (19 . "Gone.py:  gone():")))))

(ert-deftest difffontify-tests-locations-combined ()
  "A combined diff, as for a merge, has locations."
  (should (equal (difffontify-tests--text-locations
                  "diff --cc f.c"
                  "index 1111111,2222222..3333333"
                  "--- a/f.c"
                  "+++ b/f.c"
                  "@@@ -1,3 -1,3 +1,4 @@@ int main() {"
                  "  int a;"
                  "- int b;"
                  " -int c;"
                  "++int d;"
                  "  return 0;")
                 '((1 . nil)
                   (5 . "f.c:  main() {")))))

(ert-deftest difffontify-tests-locations-svn ()
  "The lines that precede a file's \"---\" line have no location."
  (should (equal (difffontify-tests--text-locations
                  "Index: a.c"
                  (make-string 67 ?=)
                  "--- a.c\t(revision 1)"
                  "+++ a.c\t(working copy)"
                  "@@ -1 +1 @@ void f() {"
                  "-  x = 1;"
                  "+  x = 2;"
                  "Index: b.c"
                  (make-string 67 ?=)
                  "--- b.c\t(revision 1)"
                  "+++ b.c\t(working copy)"
                  "@@ -1 +1 @@"
                  "-y"
                  "+z")
                 '((1 . nil)
                   (5 . "a.c:  f() {")
                   (8 . nil)
                   (12 . "b.c")))))

(ert-deftest difffontify-tests-locations-normal ()
  "A normal diff, which names its files only in the \"diff\" line."
  (should (equal (difffontify-tests--text-locations
                  "diff -r old/x.c new/x.c"
                  "5c5"
                  "< a"
                  "---"
                  "> b"
                  "Only in new: y.c")
                 '((1 . nil)
                   (2 . "new/x.c")
                   (6 . nil)))))

(ert-deftest difffontify-tests-locations-whitespace ()
  "In a location, a run of spaces and tabs is one space."
  (should (equal (difffontify-tests--text-locations
                  "--- a/m.c"
                  "+++ b/m.c"
                  "@@ -1,2 +1,2 @@ static\tint  g(void)"
                  " static int"
                  "-f(void)\t{"
                  "+h(void)\t{")
                 '((1 . nil)
                   (3 . "m.c:  g(void)")
                   (5 . "m.c:  f(void) {")
                   (6 . "m.c:  h(void) {")))))

(ert-deftest difffontify-tests-locations-langs ()
  "Locations in a diff of several files, including LaTeX environments."
  (let ((buffer (find-file-noselect
                 (expand-file-name "langs.diff" difffontify-tests--dir))))
    (unwind-protect
        (should (equal
                 (with-current-buffer buffer
                   (difffontify-tests--call-prepared
                    #'difffontify-tests--locations))
                 '((1 . nil)
                   (4 . "new/foo.el")
                   (7 . "new/foo.el:  (defun foo-double (n)")
                   (13 . nil)
                   (16 . "new/Foo.java")
                   (33 . "new/Foo.java:  count() {")
                   (38 . "new/Foo.java:  log() {")
                   (44 . nil)
                   (47 . "new/foo.py")
                   (57 . nil)
                   (60 . "new/foo.sh")
                   (69 . nil)
                   (72 . "new/foo.tex")
                   (73 . "new/foo.tex:  \\section{Introduction}")
                   (80 . nil)
                   (83 . "new/Makefile")
                   (87 . "new/Makefile:  all: foo")
                   (89 . "new/Makefile:  foo: foo.c"))))
      (kill-buffer buffer))))

(ert-deftest difffontify-tests-function-name ()
  "A function name omits annotations, modifiers, and the return type."
  (should (equal (difffontify--function-name
                  (concat "@Pure @SuppressWarnings(\"x\") public static"
                          " @Nullable Map<String, List<Integer>>"
                          " get(@NonNull String key, int n) {"))
                 "get(String key, int n) {"))
  (should (equal (difffontify--function-name "public <T> List<T> f(T x)")
                 "f(T x)"))
  (should (equal (difffontify--function-name "public Foo(int x) {")
                 "Foo(int x) {"))
  (should (equal (difffontify--function-name "public static class Inner {")
                 "class Inner {"))
  (should (equal (difffontify--function-name "std::string Foo::bar(int x)")
                 "Foo::bar(int x)"))
  (should (equal (difffontify--function-name "int *f(void)") "f(void)"))
  (should (equal (difffontify--function-name "class Foo(Base):")
                 "class Foo(Base):"))
  (should (equal (difffontify--function-name "(defun foo (n)")
                 "(defun foo (n)"))
  (should (equal (difffontify--function-name "\\section{Intro}")
                 "\\section{Intro}"))
  (should (null (difffontify--function-name "@Override"))))

(ert-deftest difffontify-tests-locations-share-strings ()
  "Lines with the same location share one string, and so one interval."
  (with-temp-buffer
    (insert "--- a/f.txt\n+++ b/f.txt\n@@ -1,3 +1,3 @@\n a\n-b\n+c\n d\n")
    (delay-mode-hooks (diff-mode))
    (difffontify-tests--call-prepared
     (lambda ()
       (goto-char (point-min))
       (search-forward "@@")
       (should (null (next-single-property-change
                      (point) 'difffontify-location)))))))

(ert-deftest difffontify-tests-wrap-location ()
  "A continuation line has the location of the line that was wrapped."
  (with-temp-buffer
    (insert (propertize "+aaa bbbb cccc dddd\n" 'difffontify-location "f.c"))
    (difffontify--wrap-lines 12)
    (goto-char (point-min))
    (search-forward "»")
    (should (equal (get-text-property (match-beginning 0) 'difffontify-location)
                   "f.c"))))

(defun difffontify-tests--headers (ps)
  "Return the parts of the left parts of the page headers in PostScript file PS.
Each element of the result is the text of a PostScript array of strings,
such as \"(x.diff      ) (f.c) (:  g\\() (int n\\)) \".  In PostScript, each
backslash and parenthesis in a string is preceded by a backslash."
  (with-temp-buffer
    (insert-file-contents ps)
    (search-forward "%%EndProlog")
    (let (headers)
      (while (re-search-forward "^/DifffontifyHeaderParts \\[\\(.*\\)\\] def$"
                                nil t)
        (push (match-string 1) headers))
      (nreverse headers))))

(ert-deftest difffontify-tests-headers ()
  "The first page header has the date, and others have the location."
  (let* ((dir (make-temp-file "difffontify-tests-" t))
         (ps (expand-file-name "out.ps" dir))
         (buffer (find-file-noselect
                  (expand-file-name "langs.diff" difffontify-tests--dir))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (difffontify-tests--call-prepared
             (lambda ()
               ;; Start elsewhere than the first page, as after preparing.
               (goto-char (point-max))
               (difffontify--write-ps (current-buffer) ps "langs.diff" nil))))
          (should (equal
                   (difffontify-tests--headers ps)
                   (list (concat "(langs.diff      "
                                 (format-time-string "%Y-%m-%d")
                                 ") () () () ")
                         (concat "(langs.diff      ) (new/foo.tex) "
                                 "(:  \\\\section{Introduction}) () ")))))
      (kill-buffer buffer)
      (delete-directory dir t))))

(ert-deftest difffontify-tests-header-parts ()
  "The formal parameters are separate from the function name."
  (let ((difffontify--header-title "x.diff"))
    (with-temp-buffer
      (insert "+first\n")
      (dolist (location '(("f.c" . "get(int n) {")
                          ("f.el" . "(defun foo (n)")
                          ("f.tex" . "\\section{Intro}")
                          ("f.c")))
        (let ((start (point)))
          (insert "+line\n")
          (put-text-property start (point) 'difffontify-location location)))
      (should (equal (cl-loop for line from 2 to 5
                              collect (progn (goto-char (point-min))
                                             (forward-line (1- line))
                                             (difffontify--header)))
                     '(("x.diff      " "f.c" ":  get" "(int n) {")
                       ("x.diff      " "f.el" ":  (defun foo (n)" "")
                       ("x.diff      " "f.tex" ":  \\section{Intro}" "")
                       ("x.diff      " "f.c" "" "")))))))

(defun difffontify-tests--rendered-header (file function)
  "Return the left part of the second page's header, as printed.
FILE and FUNCTION are the location of every line, and the header title is
\"x.diff\".  Each run of spaces in the result is one space."
  (let* ((dir (make-temp-file "difffontify-tests-" t))
         (ps (expand-file-name "out.ps" dir)))
    (unwind-protect
        (with-temp-buffer
          (dotimes (_ 100)
            (insert (propertize "+line\n" 'difffontify-location
                                (cons file function))))
          (difffontify--write-ps (current-buffer) ps "x.diff" nil)
          (erase-buffer)
          ;; Ghostscript reads standard input, because a security policy
          ;; might forbid it to read files in DIR.
          (call-process "gs" ps t nil "-q" "-dNOPAUSE" "-dBATCH"
                        "-sDEVICE=txtwrite" "-sOutputFile=-" "-")
          (goto-char (point-min))
          (and (re-search-forward "^ *\\(x\\.diff .*?\\) +2/2" nil t)
               (replace-regexp-in-string " +" " " (match-string 1))))
      (delete-directory dir t))))

(defconst difffontify-tests--parameters
  (concat "("
          (mapconcat (lambda (i) (format "String argument%d" i))
                     (number-sequence 1 12) ", ")
          ") {")
  "Formal parameters that are too long to fit in a page header.")

(ert-deftest difffontify-tests-header-fits ()
  "A page header that fits is not shortened."
  (skip-unless (executable-find "gs"))
  (should (equal (difffontify-tests--rendered-header "src/Foo.java"
                                                     "get(int n) {")
                 "x.diff src/Foo.java: get(int n) {")))

(ert-deftest difffontify-tests-header-parameters-shortened ()
  "To fit, a page header first shortens the formal parameters."
  (skip-unless (executable-find "gs"))
  (should (string-match-p
           (concat "\\`x\\.diff src/Foo\\.java: get(String argument1,"
                   " String argument2, String argument3, .*\\.\\.\\.\\'")
           (difffontify-tests--rendered-header
            "src/Foo.java" (concat "get" difffontify-tests--parameters)))))

(ert-deftest difffontify-tests-header-parameters-omitted ()
  "If too few formal parameters fit, a page header omits them all.
It never shows the \"(\" alone, as in \"get(...\"."
  (skip-unless (executable-find "gs"))
  (let (omitted)
    ;; Names of a range of lengths, because the length that leaves room for
    ;; the name but not for any parameters depends on the font.  The range
    ;; is wider than that room, which is about 3 characters.
    (dolist (length (number-sequence 40 90 2))
      (let* ((name (make-string length ?g))
             (header (difffontify-tests--rendered-header
                      "src/Foo.java"
                      (concat name difffontify-tests--parameters))))
        (should-not (string-search "(..." header))
        (when (equal header (concat "x.diff src/Foo.java: " name))
          (setq omitted t))))
    (should omitted)))

(ert-deftest difffontify-tests-header-file-shortened ()
  "To fit, a page header next shortens the start of the file name."
  (skip-unless (executable-find "gs"))
  (should (string-match-p
           (concat "\\`x\\.diff \\.\\.\\./\\(?:directory/\\)+last/Foo\\.java:"
                   " computeSomething\\'")
           (difffontify-tests--rendered-header
            (concat (mapconcat #'identity (make-list 30 "directory") "/")
                    "/last/Foo.java")
            (concat "computeSomething" difffontify-tests--parameters))))
  ;; Part of the last component, if the whole component does not fit.
  (should (string-match-p
           "\\`x\\.diff \\.\\.\\.f+\\.java: computeSomething\\'"
           (difffontify-tests--rendered-header
            (concat "src/" (make-string 300 ?f) ".java")
            (concat "computeSomething" difffontify-tests--parameters)))))

(ert-deftest difffontify-tests-header-truncated ()
  "A page header whose function name is too long to fit is truncated."
  (skip-unless (executable-find "gs"))
  (should (string-match-p
           "\\`x\\.diff src/Foo\\.java: g+\\.\\.\\.\\'"
           (difffontify-tests--rendered-header
            "src/Foo.java" (concat (make-string 300 ?g) "(int n)")))))

;;; PDF

(ert-deftest difffontify-tests-pdf ()
  "A PDF file is written, and no PostScript file is left behind."
  (skip-unless (executable-find "ps2pdf"))
  (let* ((temporary-file-directory (make-temp-file "difffontify-tests-" t))
         (pdf (expand-file-name "out.pdf" temporary-file-directory))
         (buffer (find-file-noselect
                  (expand-file-name "langs.diff" difffontify-tests--dir))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (difffontify--write-pdf pdf "langs.diff" nil))
          (should (equal (directory-files temporary-file-directory nil
                                          "\\`[^.]")
                         '("out.pdf")))
          (should (string-match-p "/MediaBox *\\[0 0 612 792\\]"
                                  (with-temp-buffer
                                    (insert-file-contents-literally pdf)
                                    (buffer-string))))
          (should (string-prefix-p "%PDF"
                                   (with-temp-buffer
                                     (insert-file-contents-literally
                                      pdf nil 0 4)
                                     (buffer-string)))))
      (kill-buffer buffer)
      (delete-directory temporary-file-directory t))))

(ert-deftest difffontify-tests-print-error ()
  "If printing fails before `lpr' runs, no temporary file is left behind."
  (let ((temporary-file-directory (make-temp-file "difffontify-tests-" t)))
    (unwind-protect
        (with-temp-buffer
          (should-error (difffontify-print) :type 'user-error)
          (should (null (directory-files temporary-file-directory nil
                                         "\\`[^.]"))))
      (delete-directory temporary-file-directory t))))

(provide 'difffontify-tests)

;;; difffontify-tests.el ends here
