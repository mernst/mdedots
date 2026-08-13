;;; -*- lexical-binding: t -*-

;;; util-cl-mde.el
;;; Michael Ernst <mernst@csail.mit.edu>

;;; Implementations of
;;;  * Common Lisp functions not in cl-lib.el
;;;  * Functions that use those
;;;  * A few basic cl-lib.el functions, to forego loading the entire package

;; This file is intended to be lean, not to provide full Common Lisp
;; compatibility; efforts have been made to exclude the extraneous.
;; This file is self-contained; it does not require that you load cl-lib.el
;; also.  An effort has been made, however, to make it compatible with
;; cl-lib.el, so that either can be loaded after the other.
;; The following cl-lib.el functions are compatibly redefined here:
;;   [none]
;; The following Emacs built-in functions are compatibly redefined here:
;;   sort


(provide 'util-clmde)
(provide 'util-cl-mde)

(eval-when-compile (require 'cl-lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug fixes
;;;

;; Commented out, 2026-02-03.
;; ;; cl-lib.el uses copy-vector but doesn't define it.
;; (defun copy-vector (vector)
;;   (copy-sequence vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12. Numbers
;;;

;;; 5.2 Trigonometric and related functions

(defun signum (x)
  (cond ((plusp x)
         1)
        ((zerop x)
         0)
        ((minusp x)
         -1)
        (t
         (error "What kind of number is this in signum?"))))
;; ;; Actually correct implementation (which is probably too expensive) is:
;; (defun signum (x)
;;   (if (zerop x)
;;       x
;;     (/ x (abs x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14. Sequences
;;;

;;; 1. Simple sequence functions

;;; 2. Concatenating, mapping, and reducing sequences

;; This seems to trounce the cl-lib.el definition; I assume that's intentional.

;; Which is more efficient:  calling this with first argument nil or
;; calling mapcar and throwing away the result?
(defun map-one-sequence (result-type function sequence)
  "Return a sequence of type RESULT-TYPE whose elements are the results of
applying FUNCTION to each element of SEQUENCE.  The length of the returned
sequence is the same as that of SEQUENCE, unless RESULT-TYPE is nil, in
which case nil is returned."
  (let ((index 0)
        (maxindex (length sequence))
        result-list)
    (if (not result-type)
        ;; If the sequence is a list, the "elt" calls may be expensive; is
        ;; it more efficient to do this or to call mapcar and throw away
        ;; the result?
        (progn
          (while (< index maxindex)
            (funcall function (elt sequence index))
            (setq index (1+ index)))
          nil)
      (progn
        (if (listp sequence)
            (setq result-list (mapcar function sequence))
          (progn
            (while (< index maxindex)
              (setq result-list
                    (cons (funcall function (elt sequence index)) result-list))
              (setq index (1+ index)))
            (setq result-list (nreverse result-list))))
        (cond ((eq 'list result-type)
               result-list)
              ((eq 'vector result-type)
               (vconcat result-list))
              ((eq 'string result-type)
               (concat result-list))
              (t
               (error "Unrecognized result-type %s" result-type)))))))

;;; 3. Modifying sequences

;;; Incompatible with cl-lib.el's version, which accepts optional arguments.
;; ;; I should generalize this.
;; (defun substitute (newitem olditem sequence)
;;   "Substitute NEWITEM for OLDITEM in SEQUENCE.  Only works on strings for now."
;;   (if (and (stringp sequence) (characterp newitem) (characterp olditem))
;;       (string-substitute newitem olditem sequence)
;;     (error "substitute isn't that good yet.")))

(defmacro string-substitute (newchar oldchar string)
  "Substitute NEWCHAR for instances of OLDCHAR in STRING.
NEWCHAR and OLDCHAR are characters."
  `(string-substitute-opt ,newchar
                          (regexp-quote (char-to-string ,oldchar))
                          ,string))

;; Optimized version.  oldchar-regexp should only match one-character strings.
(defun string-substitute-opt (newchar oldchar-regexp string)
  (let ((i -1)
        (case-fold-search nil))
    (while (setq i (string-match oldchar-regexp string (1+ i) 'inhibit-modify))
      (aset string i newchar))))


;;; 4.  Searching sequences for items

;;; Incompatible with cl-lib.el's version, which accepts optional arguments.
;; ;; Actually could use cl-lib.el's member-if for this.
;; (defun find-if (test sequence)
;;   "Return the first element satisfying TEST in SEQUENCE, or nil if none do."
;;   (let ((limit (length sequence))
;;      result
;;      (index 0))
;;     (while (< index limit)
;;       (if (funcall test (elt sequence index))
;;        (setq result (elt sequence index)
;;              index limit)
;;      (setq index (1+ index))))
;;     result))

(defun mdecl-position (item sequence)
  "Return the first index of ITEM in SEQUENCE, or nil; tests with `equal'."
  (let ((limit (length sequence))
        result
        (index 0))
    (while (< index limit)
      (if (equal item (elt sequence index))
          (setq result index
                index limit)
        (setq index (1+ index))))
    result))

;;; Incompatible with cl-lib.el's version, which accepts optional arguments.
;; (defun count (item sequence)
;;   "Return the number of times that ITEM appears in SEQUENCE; test with `equal'."
;;   (let ((limit (length sequence))
;;      (result 0)
;;      (index 0))
;;     (while (< index limit)
;;       (if (equal item (elt sequence index))
;;        (setq result (1+ result)))
;;       (setq index (1+ index)))
;;     result))



;;; 5. Sorting and merging

;;; Both versions of this cause rmail-sort-by-date to err after (require
;;; 'util-clmde), even though it is sorting a list and the new code
;;; shouldn't even be invoked.  I don't understand why; for now, comment it
;;; out (and find out where I depend on this functionality...)  1/22/99.

;; (if (not (fboundp 'mdecl-old-sort))
;;     (fset 'mdecl-old-sort (symbol-function 'sort)))
;; (defun sort (sequence predicate)
;;   "Sort SEQUENCE, stably, comparing elements using PREDICATE.
;; Return the sorted sequence, which is modified by side effects.
;; PREDICATE is called with two elements of LIST, and should return T
;; if the first element is \"less\" than the second."
;;   (if (listp sequence)
;;       (mdecl-old-sort sequence predicate)
;;     (let ((sorted-seq-as-list (mdecl-old-sort (map-one-sequence
;;                                       'list (function identity) sequence)
;;                                      predicate))
;;        (index 0)
;;        (maxindex (length sequence)))
;;       (while (< index maxindex)
;;      (aset sequence index (car sorted-seq-as-list))
;;      (setq sorted-seq-as-list (cdr sorted-seq-as-list)))
;;       sequence)))

;; (defadvice sort (around any-sequence activate)
;;   "Permit sorting of any sequence, not just lists."
;;   (if (listp (ad-get-arg 0))
;;       ad-do-it
;;     (let* ((orig-seq (ad-get-arg 0))
;;         (orig-seq-as-list (map-one-sequence
;;                            'list (function identity) orig-seq))
;;         (result-as-list (progn
;;                           (ad-set-arg 0 orig-seq-as-list)
;;                           ad-do-it)))
;;       ;; set the elements of the original sequence
;;       (let ((index 0)
;;          (maxindex (length sequence)))
;;      (while (< index maxindex)
;;        (aset sequence index (car sorted-seq-as-list))
;;        (setq sorted-seq-as-list (cdr sorted-seq-as-list)))
;;      orig-seq))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15. Lists
;;;

;;; 2. Lists

(defun butlast (list &optional n)
  "Return a list with the same elements as LIST, excepting the last N elements.
N defaults to 1.  If LIST has fewer than N elements, NIL is returned."
  (let ((copied-elts (- (length list) (or n 1)))
        result)
    (while (and list (plusp copied-elts))
      (setq result (cons (car list) result)
            copied-elts (1- copied-elts)
            list (cdr list)))
    (nreverse result)))

;;; 5. Using lists as sets

;; Originally lifted from gnus.

;;; Incompatible with cl-lib.el's version, which accepts optional arguments.
;; (defun set-difference (list1 list2)
;;   "Return an in-order list of elements of LIST1 that do not appear in LIST2."
;;   (let ((result (copy-sequence list1)))
;;     (while list2
;;       (setq result (delq (car list2) result))
;;       (setq list2 (cdr list2)))
;;     result
;;     ))

;;; Incompatible with cl-lib.el's version, which accepts optional arguments.
;; (defun intersection (list1 list2)
;;   "Return a list of elements that appear in both LIST1 and LIST2."
;;   (let (result)
;;     (while list2
;;       (if (memq (car list2) list1)
;;        (setq result (cons (car list2) result)))
;;       (setq list2 (cdr list2)))
;;     result
;;     ))

(defun set-difference-equal (list1 list2)
  "Return an in-order list of elements of LIST1 that do not appear in LIST2.
Uses equal for the comparison."
  (let (result)
    (while list1
      (if (not (member (car list1) list2))
          (setq result (cons (car list1) result)))
      (setq list1 (cdr list1)))
    (nreverse result)
    ))

(defun intersection-equal (list1 list2)
  "Return a list of elements that appear in both LIST1 and LIST2.
Uses equal for the comparison."
  (let (result)
    (while list2
      (if (member (car list2) list1)
          (setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 18. Strings
;;;

;; Return a regexp that matches zero or more characters in charseq, a
;; sequence of characters appropriate for inclusion in brackets in a regexp.
;; If some characters in charseq have special meaning (eg ^,-,[), they will
;; not necessarily be literally matched.
(defun chars->regexp (charseq)
  (if (listp charseq)
      (setq charseq (funcall (function concat) charseq)))
  (concat "[" charseq "]*"))

;;; These functions clobber the match-data.
;; Their comments about CHARSEQ should be more explicit.

;; defsubsts should be defined before they are used, to permit them to be
;; inlined.

(defsubst string-left-trim-regexp (regexp string)
  (if (string-match (concat "^" regexp) string)
      (substring string (match-end 0))
    string))

(defsubst string-left-trim (charseq string)
  "Return a substring, with characters in CHARSEQ removed from the
beginning, of STRING."
  (string-left-trim-regexp (chars->regexp charseq) string))

(defsubst string-right-trim-regexp (regexp string)
  (if (string-match (concat regexp "$") string)
      (substring string 0 (match-beginning 0))
    string))

(defsubst string-right-trim (charseq string)
  "Return a substring, with characters in CHARSEQ removed from the
end, of STRING."
  (string-right-trim-regexp (chars->regexp charseq) string))

(defsubst string-trim-chars (charseq string)
  "Return a substring, with characters in CHARSEQ removed from the
beginning and end, of STRING."
  (let ((regexp (chars->regexp charseq)))
    (string-left-trim-regexp regexp (string-right-trim-regexp regexp string))))

;; Perhaps all the string-trim functions should be defined more like this one.
(defsubst string-trim-whitespace (string)
  (if (string-match "^\\s *\\(.*[^ \t\n]\\)\\s *$" string)
      (match-string 1 string)
    ""))

(defun string-remove (char string)
  "Return a string with CHAR removed, but otherwise like STRING."
  (string-remove-regexp (concat "[" char "]") string))

(defun string-remove-regexp (regexp string)
  "Return a string with (nonoverlapping) matches for REGEXP removed from STRING."
  (let ((result ""))
    (while (string-match regexp string)
      (setq result (concat result (substring string 0 (match-beginning 0)))
            string (substring string (match-end 0))))
    (concat result string)))


;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Non-CL utilities which depend on these functions follow.
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;

(defsubst blank-string-p (string)
  "Return non-nil if STRING contains no non-whitespace characters."
  (string-match "^[ \t\n]*$" string nil 'inhibit-modify))

(defsubst blank-string-or-nil-p (string-or-nil)
  "Return non-nil if STRING is nil or contains no non-whitespace characters."
  (or (not string-or-nil)
      (blank-string-p string-or-nil)))
