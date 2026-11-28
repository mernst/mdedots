;;; diff-clean-tests.el --- Tests for diff-clean  -*- lexical-binding: t -*-

;;; Commentary:

;; Run the tests with:
;;   emacs --batch -L . -l diff-clean.el -l diff-clean-tests.el -f ert-run-tests-batch-and-exit
;;
;; The test inputs are the .diff files in diff-clean-tests/.  Each .goal file
;; holds the expected result of simplifying the .diff file of the same base
;; name.  A .diff file without a .goal file is one that the simplification
;; leaves unchanged.

;;; Code:

(require 'ert)
(require 'diff-clean)

(defconst diff-clean-tests--dir
  (expand-file-name "diff-clean-tests/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The directory that holds the test inputs and .goal files.")

(defun diff-clean-tests--file-contents (file)
  "Return the contents of FILE, a name relative to `diff-clean-tests--dir'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file diff-clean-tests--dir))
    (buffer-string)))

(defun diff-clean-tests--apply-to-string (function text &optional times)
  "Return TEXT, a diff, after calling FUNCTION on it TIMES times.
TIMES defaults to 1."
  (with-temp-buffer
    (insert text)
    (dotimes (_ (or times 1))
      (funcall function))
    (buffer-string)))

(defun diff-clean-tests--apply (function file &optional times)
  "Return the contents of FILE after calling FUNCTION on them TIMES times.
FILE is a name relative to `diff-clean-tests--dir'.  TIMES defaults to 1."
  (diff-clean-tests--apply-to-string
   function (diff-clean-tests--file-contents file) times))


;;; diff-clean-regroup-interleaved

(ert-deftest diff-clean-tests-regroup-interleaved ()
  "Changes interleaved with short context lines after \"/**\" are regrouped."
  (should (equal (diff-clean-tests--apply #'diff-clean-regroup-interleaved
                                          "interleaved.diff")
                 (diff-clean-tests--file-contents "interleaved.goal"))))

(ert-deftest diff-clean-tests-regroup-not-interleaved ()
  "A change after \"/**\" that no context line interrupts is unchanged."
  (should (equal (diff-clean-tests--apply #'diff-clean-regroup-interleaved
                                          "not-interleaved.diff")
                 (diff-clean-tests--file-contents "not-interleaved.diff"))))

(ert-deftest diff-clean-tests-meaning-preserving-regroups ()
  "`diff-clean-meaning-preserving' regroups, and running it again changes nothing.
In particular, `diff-clean-prefix-suffix' does not turn the \"/**\" lines at
the start of the two blocks into a context line."
  (let ((goal (diff-clean-tests--file-contents "interleaved.goal")))
    (should (equal (diff-clean-tests--apply #'diff-clean-meaning-preserving
                                            "interleaved.diff")
                   goal))
    (should (equal (diff-clean-tests--apply #'diff-clean-meaning-preserving
                                            "interleaved.diff" 2)
                   goal))))

(ert-deftest diff-clean-tests-regroup-stops-at-file-header ()
  "Regrouping does not take in the header of the next file's diff."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-regroup-interleaved
                  (concat "--- a/Foo.java\n+++ b/Foo.java\n@@ -1,3 +1,3 @@\n"
                          " /**\n-x\n }\n+y\n"
                          "--- a/Bar.java\n+++ b/Bar.java\n@@ -1 +1 @@\n"
                          "-p\n+q\n"))
                 (concat "--- a/Foo.java\n+++ b/Foo.java\n@@ -1,3 +1,3 @@\n"
                         "-/**\n-x\n-}\n+/**\n+}\n+y\n"
                         "--- a/Bar.java\n+++ b/Bar.java\n@@ -1 +1 @@\n"
                         "-p\n+q\n"))))

(ert-deftest diff-clean-tests-regroup-stops-at-patch-signature ()
  "Regrouping does not take in the \"-- \" line before a patch's signature."
  (let ((text (concat "@@ -1,3 +1,4 @@\n   /**\n+   * New doc.\n    */\n }\n"
                      "-- \n2.43.0\n")))
    (should (equal (diff-clean-tests--apply-to-string
                    #'diff-clean-regroup-interleaved text)
                   text))))

(ert-deftest diff-clean-tests-regroup-no-newline-marker ()
  "A \"\\ No newline at end of file\" line stays with the line before it."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-regroup-interleaved
                  (concat "@@ -1,3 +1,4 @@\n"
                          " /**\n+a\n }\n-}\n\\ No newline at end of file\n+}\n"))
                 (concat "@@ -1,3 +1,4 @@\n"
                         "-/**\n-}\n-}\n\\ No newline at end of file\n"
                         "+/**\n+a\n+}\n+}\n"))))

(ert-deftest diff-clean-tests-meaning-preserving-same-last-line ()
  "Running `diff-clean-meaning-preserving' twice is the same as running it once.
The regrouped old text and new text end with the same line, which
`diff-clean-prefix-suffix' leaves alone."
  (let* ((text "@@ -1,3 +1,3 @@\n /**\n+a\n }\n-}\n")
         (once (diff-clean-tests--apply-to-string
                #'diff-clean-meaning-preserving text)))
    (should (equal once "@@ -1,3 +1,3 @@\n-/**\n-}\n-}\n+/**\n+a\n+}\n"))
    (should (equal (diff-clean-tests--apply-to-string
                    #'diff-clean-meaning-preserving text 2)
                   once))))

(ert-deftest diff-clean-tests-regroup-skips-combined-diff ()
  "Regrouping leaves alone a hunk of a combined diff."
  (let ((text "@@@ -1,2 -1,2 +1,4 @@@\n  /**\n+ * a\n  */\n+ }\n"))
    (should (equal (diff-clean-tests--apply-to-string
                    #'diff-clean-regroup-interleaved text)
                   text))))


;;; diff-clean-prefix-suffix

(ert-deftest diff-clean-tests-prefix-suffix-match-before-replacement ()
  "`diff-clean-prefix-suffix' makes a replacement that an earlier one enables.
Turning \"-x\" ... \"+x\" into a context line makes \"-y\" adjacent to the
added lines, which include \"+y\"."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-prefix-suffix "-y\n-x\n+w\n+y\n+x\n")
                 "+w\n y\n x\n")))

(ert-deftest diff-clean-tests-prefix-suffix-merges-javadoc-start ()
  "`diff-clean-prefix-suffix' merges \"/**\" lines that no regrouping created."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-prefix-suffix "-  /**\n+  /**\n-   * a\n+   * b\n")
                 "   /**\n-   * a\n+   * b\n"))
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-prefix-suffix "-  /**\n-   * a\n+  /**\n+   * b\n")
                 "   /**\n-   * a\n+   * b\n")))

;;; diff-clean-imports

(ert-deftest diff-clean-tests-imports-package-and-blank-lines ()
  "`diff-clean-imports' removes package and import lines and the blank lines below them."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-imports
                  (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                          "@@ -1,8 +1,10 @@\n"
                          "-package a;\n-\n+package b;\n+\n"
                          " import c.D;\n"
                          "-import e.F;\n-\n"
                          "+import g.H;\n+import i.J;\n+  \n+\n"
                          " class Foo {\n"
                          "-  int x;\n+  int y;\n"
                          " }\n"))
                 (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                         "@@ -3,4 +3,4 @@\n"
                         " import c.D;\n"
                         " class Foo {\n"
                         "-  int x;\n+  int y;\n"
                         " }\n"))))

(ert-deftest diff-clean-tests-imports-keeps-other-blank-lines ()
  "`diff-clean-imports' keeps a blank line that a removed import line does not precede.
A blank line with the opposite indicator character, or one that follows a
context line, is retained."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-imports
                  (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                          "@@ -1,3 +1,4 @@\n"
                          "-import e.F;\n+\n"
                          " class Foo {\n"
                          "+\n"
                          " }\n"))
                 (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                         "@@ -2,2 +1,4 @@\n"
                         "+\n"
                         " class Foo {\n"
                         "+\n"
                         " }\n"))))

(ert-deftest diff-clean-tests-imports-no-newline-marker ()
  "`diff-clean-imports' deletes the \"\\ No newline\" line of a deleted line."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-imports
                  (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                          "@@ -1,3 +1,1 @@\n"
                          "-class Foo {}\n+class Bar {}\n"
                          "-import a.B;\n-\n"
                          "\\ No newline at end of file\n"))
                 (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                         "@@ -1,1 +1,1 @@\n"
                         "-class Foo {}\n+class Bar {}\n"))))

(ert-deftest diff-clean-tests-imports-all-old-lines-deleted ()
  "`diff-clean-imports' numbers a side with no remaining lines by the line before."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-imports
                  (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                          "@@ -4,2 +4,3 @@\n"
                          "-import a.B;\n-import c.D;\n"
                          "+import e.F;\n+\n+class X {}\n"))
                 (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                         "@@ -5,0 +6,1 @@\n"
                         "+class X {}\n"))))

(ert-deftest diff-clean-tests-imports-crlf ()
  "`diff-clean-imports' cleans a diff whose lines end with CRLF."
  (should (equal (diff-clean-tests--apply-to-string
                  #'diff-clean-imports
                  (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                          "@@ -1,3 +1,3 @@\n"
                          "-import a.B;\r\n-\r\n+import c.D;\r\n"
                          " class Foo {\r\n"
                          "+  int y;\r\n"))
                 (concat "--- a/Foo.java\n+++ b/Foo.java\n"
                         "@@ -3,1 +2,2 @@\n"
                         " class Foo {\r\n"
                         "+  int y;\r\n"))))

(provide 'diff-clean-tests)

;;; diff-clean-tests.el ends here
