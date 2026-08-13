;;; -*- lexical-binding: t -*-

;;; regress.el --- simple regression tests for Emacs

;;; Commentary:

;; Run these tests with:
;;   make test
;; or, equivalently:
;;   emacs --batch -L . -l regress.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(defmacro regress-with-test-buffer (&rest body)
  "Execute BODY in a temporary buffer containing 10 characters."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert "abcdefghij")
     ,@body))

(ert-deftest regress-char-before-and-after ()
  "`char-before' and `char-after' return the same value whether or
not they are given an argument, even at buffer minima and maxima."
  (regress-with-test-buffer
    (dolist (pos (list (point-min) 5 (point-max)))
      (goto-char pos)
      (should (equal (char-before) (char-before (point))))
      (should (equal (char-after) (char-after (point)))))))

(ert-deftest regress-char-before-and-after-narrowed ()
  "`char-before' and `char-after' agree with their one-argument forms
at the minima and maxima of a narrowed buffer."
  (regress-with-test-buffer
    (save-restriction
      (narrow-to-region (+ (point-min) 3) (- (point-max) 3))
      (dolist (pos (list (point-min) (point-max)))
        (goto-char pos)
        (should (equal (char-before) (char-before (point))))
        (should (equal (char-after) (char-after (point))))))))

(ert-deftest regress-expand-file-name-in-root ()
  "Expanding a relative file name in \"/\" does not double the slash."
  (should (equal (expand-file-name "foo" "/") "/foo")))

(provide 'regress)

;;; regress.el ends here
