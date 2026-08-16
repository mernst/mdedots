;;; -*- lexical-binding: t -*-

;;; It would be good to offer this to the Emacs maintainers, but probably
;;; via generalizing flush-lines and redefining both it and this function
;;; in terms of the generalization.
;; `keep-paragraphs' and `flush-paragraphs' are `keep-lines' and
;; `flush-lines' with each line operation replaced by the corresponding
;; paragraph operation from `replace-paragraphs--*' below.
;; An alternate technique that works but is ugly:
;;   (goto-char (point-min))
;;   (replace-string "\C-j\C-j" "<<<PARBREAK>>>")
;;   (goto-char (point-min))
;;   (replace-string "\C-j" "<<<LINEBREAK>>>")
;;   (goto-char (point-min))
;;   (replace-string "<<<PARBREAK>>>" "\C-j")
;;   (goto-char (point-min))
;;   (delete-non-matching-lines regexp)
;;   (goto-char (point-min))
;;   (replace-string "\C-j" "\C-j\C-j")
;;   (goto-char (point-min))
;;   (replace-string "<<<LINEBREAK>>>" "\C-j")

;; A paragraph begins at the first character of its text and ends after the
;; paragraph separator that follows the text, just as a line begins after a
;; newline and ends after its own newline.  Deleting such an extent leaves
;; neither a stray blank line nor two paragraphs run together.
;;
;; These functions exist because the paragraph motion commands are not
;; analogous to the line motions that `keep-lines' and `flush-lines' use.
;; In particular, `backward-paragraph' moves to the separator preceding the
;; paragraph rather than to the paragraph's text, and it moves to the
;; previous paragraph when point is already at the beginning of a paragraph,
;; whereas `(forward-line 0)' is idempotent.

(defun replace-paragraphs--skip-separator ()
  "Move point forward over paragraph-separating lines.
Point must be at the beginning of a line or at the end of the buffer."
  (while (and (not (eobp)) (bolp) (looking-at-p paragraph-separate))
    (forward-line 1)))

(defun replace-paragraphs--paragraph-beginning-position ()
  "Return the position of the beginning of the paragraph containing point.
The paragraph containing point is the one whose text or whose trailing
separator point is in.  If point precedes the first paragraph's text,
return the beginning of that text.  This is the paragraph analogue of
`line-beginning-position'."
  (save-excursion
    (let ((text-beginning (save-excursion
                            (forward-paragraph 1)
                            (backward-paragraph 1)
                            (replace-paragraphs--skip-separator)
                            (point))))
      (if (<= text-beginning (point))
          text-beginning
        ;; Point is in a separator, which belongs to the preceding paragraph.
        (backward-paragraph 1)
        (replace-paragraphs--skip-separator)
        (point)))))

(defun replace-paragraphs--forward-paragraph ()
  "Move point to the beginning of the paragraph after the one containing point.
Move to the end of the buffer if there is no such paragraph.  This is the
paragraph analogue of `(forward-line 1)'."
  (goto-char (replace-paragraphs--paragraph-beginning-position))
  (forward-paragraph 1)
  (replace-paragraphs--skip-separator))

(defalias 'delete-non-matching-paragraphs 'keep-paragraphs)
(defalias 'delete-matching-paragraphs 'flush-paragraphs)
(defun keep-paragraphs (regexp &optional rstart rend interactive)
  "Delete all paragraphs except those containing matches for REGEXP.
A match split across paragraphs preserves all the paragraphs it lies in.
When called from Lisp (and usually interactively as well, see below)
applies to all paragraphs starting after point.  If point is in the middle
of a paragraph, that paragraph is preserved.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
This command operates on (the accessible part of) all paragraphs whose
accessible part is entirely contained in the region determined by RSTART
and REND.  (The paragraph separator that follows a paragraph counts as
part of that paragraph.)  If RSTART is non-nil, REND also has to be given.

Interactively, in Transient Mark mode when the mark is active, operate
on all paragraphs whose accessible part is entirely contained in the region.
Otherwise, the command applies to all paragraphs starting after point.
When calling this function from Lisp, you can pretend that it was
called interactively by passing a non-nil INTERACTIVE argument.

This function starts looking for the next match from the end of
the previous match.  Hence, it ignores matches that overlap
a previously found match."

  (interactive
   (progn
     (offer-to-change-if-read-only)
     (keep-lines-read-args "Keep paragraphs (containing match for regexp)")))
  (if rstart
      (progn
        (goto-char (min rstart rend))
        (setq rend
              (progn
                (save-excursion
                  (goto-char (max rstart rend))
                  (unless (eobp)
                    ;; Do not delete a paragraph that extends past the region.
                    (goto-char (min (point)
                                    (replace-paragraphs--paragraph-beginning-position))))
                  (point-marker)))))
    (if (and interactive (use-region-p))
        (setq rstart (region-beginning)
              rend (progn
                     (goto-char (region-end))
                     (unless (eobp)
                       (goto-char (min (point)
                                       (replace-paragraphs--paragraph-beginning-position))))
                     (point-marker)))
      (setq rstart (point)
            rend (point-max-marker)))
    (goto-char rstart))
  (save-excursion
    ;; Move to the beginning of a paragraph, preserving a paragraph that point
    ;; is in the middle of.
    (let ((beginning (replace-paragraphs--paragraph-beginning-position)))
      (if (< beginning (point))
          (replace-paragraphs--forward-paragraph)
        (goto-char beginning)))
    (let ((start (point))
          (case-fold-search
           (if (and case-fold-search search-upper-case)
               (isearch-no-upper-case-p regexp t)
             case-fold-search)))
      (while (< (point) rend)
        ;; Start is first char not preserved by previous match.
        (if (not (re-search-forward regexp rend 'move))
            (delete-region start rend)
          (let ((end (save-excursion
                       (goto-char (match-beginning 0))
                       (replace-paragraphs--paragraph-beginning-position))))
            ;; Now end is first char preserved by the new match.
            (if (< start end)
                (delete-region start end))))

        (setq start (save-excursion
                      (replace-paragraphs--forward-paragraph)
                      (point)))
        ;; If the match was empty, avoid matching again at same place.
        (and (< (point) rend)
             (= (match-beginning 0) (match-end 0))
             (forward-char 1)))))
  (set-marker rend nil)
  nil)
(defun flush-paragraphs (regexp &optional rstart rend interactive)
 "Delete paragraphs containing matches for REGEXP.
When called from Lisp (and usually when called interactively as
well, see below), applies to the part of the buffer after point.
The paragraph point is in is deleted if and only if it contains a
match for regexp starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\')
and `search-upper-case' is non-nil, the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
Paragraphs partially contained in this region are deleted if and only if
they contain a match entirely contained in it.

Interactively, in Transient Mark mode when the mark is active, operate
on the contents of the region.  Otherwise, operate from point to the
end of (the accessible portion of) the buffer.  When calling this function
from Lisp, you can pretend that it was called interactively by passing
a non-nil INTERACTIVE argument.

If a match is split across paragraphs, all paragraphs it lies in are deleted.
They are deleted _before_ looking for the next match.  Hence, a match
starting on the same paragraph at which another match ended is ignored.

Return the number of deleted matching paragraphs.  When called
interactively, also print the number."

  (interactive
   (progn
     (offer-to-change-if-read-only)
     (keep-lines-read-args "Flush paragraphs containing match for regexp")))
  (if rstart
      (progn
        (goto-char (min rstart rend))
        (setq rend (copy-marker (max rstart rend))))
    (if (and interactive (use-region-p))
        (setq rstart (region-beginning)
              rend (copy-marker (region-end)))
      (setq rstart (point)
            rend (point-max-marker)))
    (goto-char rstart))
  (let ((count 0)
        (case-fold-search
         (if (and case-fold-search search-upper-case)
             (isearch-no-upper-case-p regexp t)
           case-fold-search)))
    (save-excursion
      (while (and (< (point) rend)
                  (re-search-forward regexp rend t))
        (delete-region (save-excursion
                         (goto-char (match-beginning 0))
                         (replace-paragraphs--paragraph-beginning-position))
                       (progn (replace-paragraphs--forward-paragraph) (point)))
        (setq count (1+ count))))
    (set-marker rend nil)
    (when interactive (message (ngettext "Deleted %d matching paragraph"
                                         "Deleted %d matching paragraphs"
                                         count)
                               count))
    count))

(defun offer-to-change-if-read-only ()
  "Offer to make the buffer not read-only."
  (if (and buffer-read-only
           this-command ; nil if in startup
           )
      (if (y-or-n-p (format "Buffer %s is read-only.  Make buffer modifiable? " (current-buffer)))
          (setq buffer-read-only nil)))
  ;; Do not call barf-if-buffer-read-only, because this might be in advice to that function.
  )

(provide 'replace-paragraphs)
