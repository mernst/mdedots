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

(defun diff-clean-tests--apply (function file &optional times)
  "Return the contents of FILE after calling FUNCTION on them TIMES times.
FILE is a name relative to `diff-clean-tests--dir'.  TIMES defaults to 1."
  (with-temp-buffer
    (insert (diff-clean-tests--file-contents file))
    (dotimes (_ (or times 1))
      (funcall function))
    (buffer-string)))


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

(provide 'diff-clean-tests)

;;; diff-clean-tests.el ends here
