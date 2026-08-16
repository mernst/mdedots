;;; -*- lexical-binding: t -*-

;; C-x = (`what-cursor-position') reports the character, the position, and
;; the column, but not the line number.  This file appends the line number,
;; so `what-line' is not needed.
;;
;; M-= is already bound to `count-words-region', which counts lines, words,
;; and characters, so no binding is needed for it.
;;
;; This file defines no key bindings; it is a library, and a `require' of it
;; should not change the global keymap.  To use it, put this in your .emacs:
;;   (require 'count)
;;   (define-key ctl-x-map "=" #'what-cursor-position-and-line)

(defun what-cursor-position-and-line (&optional detail)
  "Print info on cursor position (on screen and within buffer).
Like `what-cursor-position', but also print the line number.
The line number ignores any narrowing.
With prefix argument DETAIL, display detailed information about the
character, as `what-cursor-position' does."
  (interactive "P")
  (let ((line (line-number-at-pos nil t)))
    (what-cursor-position detail)
    ;; With DETAIL, `what-cursor-position' pops up a *Help* buffer rather
    ;; than printing a one-line message to append the line number to.
    (unless detail
      (message "%s  line %d" (current-message) line))))

(provide 'count)
