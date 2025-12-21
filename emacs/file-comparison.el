;;; -*- lexical-binding: t -*-

(eval-when-compile
  '(require 'etags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ediff
;;;

;;; Experimentally commented out, 2025-04-06.
;; ;;; I sent mail to Michael Kifer on Nov 10, 2017, but never got a response.
;; ;; Problem with ediff merging:  if both files use CRLF (DOS) eol-type, but
;; ;; the most-preferred coding system does not use DOS eol-type, then the
;; ;; ediff merge tool creates a non-DOS merged file!  That results in a
;; ;; needless change to every line, which creates very large diffs and
;; ;; destroys the version control history.
;; ;; The problem is in ediff-setup, where it does
;; ;;          (with-current-buffer buffer-C
;; ;;            (insert-buffer-substring buf)
;; ;; and buffer-C's eol-type is retained.
;; ;; The fix should go in or near `ediff-choose-syntax-table', which has a
;; ;; similar purpose.
;; ;; For a test case, see file ediff-merge-test.zip .
;; (defadvice ediff-choose-syntax-table (after set-eol-type activate)
;;   "Set the coding-system for the merged file."
;;   (let* ((eol-type-a (coding-system-eol-type
;;                       (with-current-buffer ediff-buffer-A
;;                         buffer-file-coding-system)))
;;          (eol-type-b (coding-system-eol-type
;;                       (with-current-buffer ediff-buffer-B
;;                         buffer-file-coding-system)))
;;          (ancestor-buffer (ediff-buffer-live-p ediff-ancestor-buffer))
;;          (eol-type-ancestor (and ancestor-buffer
;;                                  (with-current-buffer ancestor-buffer
;;                                    buffer-file-coding-system)))
;;          (coding-system-c (and ediff-buffer-C
;;                                (with-current-buffer ediff-buffer-C
;;                                  buffer-file-coding-system)))
;;          (eol-type-c (and coding-system-c
;;                           (coding-system-eol-type coding-system-c))))
;;     (if (equal eol-type-a eol-type-b)
;;         ;; The two children have the same line endings, use it.
;;         (setq eol-type-c eol-type-a)
;;       (progn
;;         ;; The two children have different line endings;
;;         ;; use the ancestor's line endings, but warn.
;;         (message "Warning: ediff is merging files with different eol-type:\n  %s %s\n  %s %s"
;;                  eol-type-a ediff-buffer-A
;;                  eol-type-b ediff-buffer-B)
;;         (setq eol-type-c eol-type-ancestor)))
;;     (if (and eol-type-c ediff-buffer-C)
;;         (with-current-buffer ediff-buffer-C
;;           (set-buffer-file-coding-system
;;            (coding-system-change-eol-conversion
;;             coding-system-c eol-type-c))))))


(provide 'file-comparison)
