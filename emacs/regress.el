;;; -*- lexical-binding: t -*-

;;; regress.el:  simple regression tests for Emacs

;; char-{after,before} should return the same value whether given an
;; argument or not, even at buffer minima/maxima
(cl-assert (equal (char-before) (char-before (point))))
(cl-assert (equal (char-after) (char-after (point))))
(cl-assert (save-excursion
	  (goto-char (point-min))
	  (equal (char-before) (char-before (point)))))
(cl-assert (save-excursion
	  (goto-char (point-max))
	  (equal (char-after) (char-after (point)))))
(cl-assert (save-excursion
	  (save-restriction
	    (narrow-to-region (+ (point-min) 3) (- (point-max) 3))
	    (goto-char (point-min))
	    (equal (char-before) (char-before (point))))))
(cl-assert (save-excursion
	  (save-restriction
	    (narrow-to-region (+ (point-min) 3) (- (point-max) 3))
	    (goto-char (point-max))
	    (equal (char-after) (char-after (point))))))

(cl-assert (equal (expand-file-name "foo" "/") "/foo"))
