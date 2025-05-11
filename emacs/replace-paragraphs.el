;;; -*- lexical-binding: t -*-

;;; It would be good to offer this to the Emacs maintainers, but probably
;;; via generalizing flush-lines and redefining both it and this function
;;; in terms of the generalization.
;; The defaliases and defun are copied verbatim from flush-lines, then
;; replace each instance of "(forward-line 0)" by "(backward-paragraph)"
;; and then "line" by "paragraph".  Not well tested.  One known bug:
;; keep-paragraphs always seems to keep the first paragraph even if it
;; doesn't contain the regexp.
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
(defalias 'delete-non-matching-paragraphs 'keep-paragraphs)
(defalias 'delete-matching-paragraphs 'flush-paragraphs)
(defun keep-paragraphs (regexp &optional rstart rend interactive)
  "Delete all paragraphs except those containing matches for REGEXP.
A match split across paragraphs preserves all the paragraphs it lies in.
When called from Lisp (and usually interactively as well, see below)
applies to all paragraphs starting after point.

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

Second and third arg RSTART and REND specify the region to operate on.
This command operates on (the accessible part of) all paragraphs whose
accessible part is entirely contained in the region determined by RSTART
and REND.  (A newparagraph ending a paragraph counts as part of that paragraph.)

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
                  (unless (or (bolp) (eobp))
                    (backward-paragraph))
                  (point-marker)))))
    (if (and interactive transient-mark-mode mark-active)
        (setq rstart (region-beginning)
              rend (progn
                     (goto-char (region-end))
                     (unless (or (bolp) (eobp))
                       (backward-paragraph))
                     (point-marker)))
      (setq rstart (point)
            rend (point-max-marker)))
    (goto-char rstart))
  (save-excursion
    ;; MDE CHANGE:  was "(or (bolp)"
    (or (bobp) (forward-paragraph 1))
    (let ((start (point))
          (case-fold-search  (and case-fold-search
                                  (isearch-no-upper-case-p regexp t))))
      (while (< (point) rend)
        ;; Start is first char not preserved by previous match.
        (if (not (re-search-forward regexp rend 'move))
            (delete-region start rend)
          (let ((end (save-excursion (goto-char (match-beginning 0))
                                     (backward-paragraph)
                                     (point))))
            ;; Now end is first char preserved by the new match.
            (if (< start end)
                (delete-region start end))))

        (setq start (save-excursion
                      (forward-paragraph 1)
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

If REGEXP contains upper case characters (excluding those preceded by `\\'),
the matching is case-sensitive.

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
starting on the same paragraph at which another match ended is ignored."

  (interactive
   (progn
     (offer-to-change-if-read-only)
     (keep-lines-read-args "Flush paragraphs containing match for regexp")))
  (if rstart
      (progn
        (goto-char (min rstart rend))
        (setq rend (copy-marker (max rstart rend))))
    (if (and interactive transient-mark-mode mark-active)
        (setq rstart (region-beginning)
              rend (copy-marker (region-end)))
      (setq rstart (point)
            rend (point-max-marker)))
    (goto-char rstart))
  (let ((case-fold-search (and case-fold-search
                               (isearch-no-upper-case-p regexp t))))
    (save-excursion
      (while (and (< (point) rend)
                  (re-search-forward regexp rend t))
        (delete-region (save-excursion (goto-char (match-beginning 0))
                                       (backward-paragraph)
                                       (point))
                       (progn (forward-paragraph 1) (point))))))
  (set-marker rend nil)
  nil)

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
