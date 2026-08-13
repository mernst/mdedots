;;; -*- lexical-binding: t -*-

;;; Tests for replace-paragraphs.el.  Run them with:
;;;   emacs --batch -L . -l replace-paragraphs-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'replace-paragraphs)

(defconst replace-paragraphs-tests--text
  "alpha one\nalpha two\n\nbeta one\n\ngamma\n"
  "A buffer of three paragraphs, beginning at positions 1, 22, and 32.")

(defun replace-paragraphs-tests--apply (fn text position regexp)
  "Return the result of calling FN with REGEXP in a buffer containing TEXT.
Point is at POSITION when FN is called."
  (with-temp-buffer
    (text-mode)
    (insert text)
    (goto-char position)
    (funcall fn regexp)
    (buffer-string)))

(ert-deftest replace-paragraphs-tests-keep-from-point-min ()
  (let ((text replace-paragraphs-tests--text))
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 1 "gamma")
                   "gamma\n"))
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 1 "alpha")
                   "alpha one\nalpha two\n\n"))
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 1 "one")
                   "alpha one\nalpha two\n\nbeta one\n\n"))
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 1 "nomatch")
                   ""))))

(ert-deftest replace-paragraphs-tests-keep-does-not-preserve-paragraph-at-point ()
  "A paragraph that begins at point is deleted if it does not match."
  (let ((text replace-paragraphs-tests--text))
    ;; Position 22 is the first character of the second paragraph, and
    ;; position 21 is the separator that precedes it.
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 22 "gamma")
                   "alpha one\nalpha two\n\ngamma\n"))
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 21 "gamma")
                   "alpha one\nalpha two\n\ngamma\n"))))

(ert-deftest replace-paragraphs-tests-keep-preserves-paragraph-point-is-within ()
  (let ((text replace-paragraphs-tests--text))
    ;; Position 11 is within the first paragraph, whose text does not match.
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 11 "gamma")
                   "alpha one\nalpha two\n\ngamma\n"))
    (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs text 11 "nomatch")
                   "alpha one\nalpha two\n\n"))))

(ert-deftest replace-paragraphs-tests-keep-in-region ()
  (let ((text replace-paragraphs-tests--text))
    ;; The third paragraph is outside the region, and the second one is only
    ;; partly within it, so neither is deleted.
    (with-temp-buffer
      (text-mode)
      (insert text)
      (keep-paragraphs "nomatch" 1 25)
      (should (equal (buffer-string) "beta one\n\ngamma\n")))
    (with-temp-buffer
      (text-mode)
      (insert text)
      (keep-paragraphs "beta" (point-min) (point-max))
      (should (equal (buffer-string) "beta one\n\n")))))

(ert-deftest replace-paragraphs-tests-flush ()
  (let ((text replace-paragraphs-tests--text))
    (should (equal (replace-paragraphs-tests--apply #'flush-paragraphs text 1 "beta")
                   "alpha one\nalpha two\n\ngamma\n"))
    (should (equal (replace-paragraphs-tests--apply #'flush-paragraphs text 1 "one")
                   "gamma\n"))
    (should (equal (replace-paragraphs-tests--apply #'flush-paragraphs text 1 "nomatch")
                   text))
    ;; Only matches after point count.
    (should (equal (replace-paragraphs-tests--apply #'flush-paragraphs text 22 "one")
                   "alpha one\nalpha two\n\ngamma\n"))))

(ert-deftest replace-paragraphs-tests-flush-returns-count ()
  (with-temp-buffer
    (text-mode)
    (insert replace-paragraphs-tests--text)
    (goto-char (point-min))
    (should (equal (flush-paragraphs "one") 2))
    (should (equal (flush-paragraphs "nomatch") 0))))

(ert-deftest replace-paragraphs-tests-degenerate-buffers ()
  (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs "" 1 "zzz") ""))
  (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs "single" 1 "zzz") ""))
  (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs "single" 1 "sing")
                 "single"))
  ;; A paragraph with neither a preceding nor a following newline, and
  ;; multiple separator lines.
  (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs "one\n\n\n\ntwo" 1 "two")
                 "two"))
  (should (equal (replace-paragraphs-tests--apply #'keep-paragraphs "one\n\n\n\ntwo" 1 "one")
                 "one\n\n\n\n")))

(ert-deftest replace-paragraphs-tests-agrees-with-keep-lines ()
  "Paragraphs of a single line each behave like the lines of `keep-lines'."
  (let ((paragraphs "aaa\n\nbbb\n\naaa bbb\n\nccc\n")
        (lines "aaa\nbbb\naaa bbb\nccc\n"))
    (dolist (regexp '("aaa" "bbb" "ccc" "b" "zzz" ""))
      (should (equal (replace-regexp-in-string
                      "\n\n" "\n"
                      (replace-paragraphs-tests--apply #'keep-paragraphs paragraphs 1 regexp))
                     (replace-paragraphs-tests--apply #'keep-lines lines 1 regexp)))
      (should (equal (replace-regexp-in-string
                      "\n\n" "\n"
                      (replace-paragraphs-tests--apply #'flush-paragraphs paragraphs 1 regexp))
                     (replace-paragraphs-tests--apply #'flush-lines lines 1 regexp))))))

(provide 'replace-paragraphs-tests)
