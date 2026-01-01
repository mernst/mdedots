;;; -*- lexical-binding: t -*-

;; buffer-menu-mde.el -- enhancements to buffer-menu

;;; Commentary:

;; This file enhances the buffer-menu command in several ways:
;;  * marks uninteresting buffers unmodified
;;  * abbreviates file names:
;;     * it uses "~" instead of the full home directory pathname,
;;     * it permits arbitrary other replacements
;;  * omits temporary buffers
;;  * erases the read-only marks, which I find distracting
;;  * puts the cursor in the buffer-menu
;;  * updates the "*Buffer List*" buffer whenever `save-some-buffers' executes

;; To use, add to your .emacs:
;;   (with-eval-after-load "buff-menu" (load "buffer-menu-mde"))
;; You may also wish to set some of the variables that appear in this file.



;;; Code:

(eval-when-compile
  (require 'cl-lib)			; for `cl-assert'
  (require 'dired)			; for `dired-re-mark'
  )

(defvar debug-buffer-menu-mde t)

(autoload 'save-buffer-if-modified "util-mde"
  "Save buffer if it exists and is modified." t)

;; This needs to come first to avoid ":around" clobbering other advice.
(defun buffer-menu--pop-to-buffer (_buffer-menu-function &optional arg)
  "Use `pop-to-buffer' instead of `switch-to-buffer'."
  (pop-to-buffer (list-buffers-noselect arg))
  (buffer-menu--display-help))
(advice-add 'buffer-menu :around #'buffer-menu--pop-to-buffer)


;;;
;;; Unbind very dangerous keystroke that's too easy to type
;;;

;; was:  (define-key Buffer-menu-mode-map "g" 'Buffer-menu-revert)
(define-key Buffer-menu-mode-map "g" nil)


;;;
;;; Mark uninteresting buffers unmodified
;;;

(defun buffer-menu--set-some-buffers-unmodified (&optional _arg)
  "Set buffers that need not be saved as unmodified."
  (set-some-buffers-unmodified "^\\(\\*\\|RMAIL-summary\\)"
                               nil ; was '(ange-ftp-shell-mode webster-mode)
                               (concat "^" (regexp-opt
                                            '("*mail"
                                              "*VM-mail*"
                                              "*post-news*"
                                              "*news on"
                                              "*followup to"
                                              "*reply to"
                                              "*cvs-commit-message*")))))
(advice-add 'buffer-menu :before #'buffer-menu--set-some-buffers-unmodified)

(defun set-some-buffers-unmodified (name-regexp major-modes &optional exceptions-regexp exceptions-modes)
  "Clear the modification flag of certain buffers.
Buffers whose names match NAME-REGEXP, or whose major mode is a member of
MAJOR-MODES, are set unmodified.  Either or both of the arguments may be nil.
Buffers whose names match optional third argument EXCEPTIONS-REGEXP
or whose mode is in EXCEPTIONS-MODES are never set unmodified.
Also sets dired buffer modification flags."
  (let ((blist (buffer-list)))
    (while blist
      (with-current-buffer (car blist)
        (setq blist (cdr blist))
        ;; Don't do the work unless the buffer is marked modified.
        (if (buffer-modified-p)
            (progn
              (if (and (or (and name-regexp
                                (string-match name-regexp (buffer-name)))
                           (memq major-mode major-modes))
                       (not (or (and exceptions-regexp
                                     (string-match exceptions-regexp (buffer-name)))
                                (memq major-mode exceptions-modes))))
                  (set-buffer-modified-p nil))
              ;; This special-casing is sort of cheating.  But hey, it works.
              ;; It's OK for this to be in the progn because adding a mark
              ;; will always flag the buffer modified; that is, even though
              ;; this code only changes the modification flag from t to
              ;; nil, it does the right thing.
              (if (eq major-mode 'dired-mode)
                  (set-buffer-modified-p (dired-pending-marks-p)))))))))

(defun dired-pending-marks-p ()
  "Return non-nil if this Dired buffer contains any marks."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward dired-re-mark nil t)))


;;;
;;; Abbreviate file names
;;;

(defun buffer-menu--do-buffer-menu-replacements (&optional _arg)
  (with-current-buffer "*Buffer List*"
    (let ((buffer-read-only nil))
      (do-buffer-menu-replacements))
    (set-buffer-modified-p nil)))
(advice-add 'buffer-menu :after #'buffer-menu--do-buffer-menu-replacements)

(defvar buffer-menu-replacement-alist nil
  "Association list of directories and their abbreviations for the buffer menu.
Regular expression replacement is performed for all of these elements in turn.
Each will be anchored at the front to the beginning of a filename (actually,
to whitespace).

You need not include a mapping of your home directory to \"~\", as that is
hard-coded in.  However, list-buffers does not respect `directory-abbrev-alist'.
(Though as of version 30, maybe it does.)

Here is an example setting:
    (setq buffer-menu-replacement-alist
          \\='(
            (\"/mernst@cs.rice.edu:/home/mernst/\" . \"rice:~/\")
            (\"/projects/cecil9/mernst/vortex/Cecil/src/links/\" . \"$links/\")
            ;; ;; `list-buffers' doesn't respect `directory-abbrev-alist'.
            ;; (\"/usr/spool/ftp\" . \"~ftp\")
            ))
")

(defun do-buffer-menu-replacements ()
  "Abbreviate file names in current buffer."
  ;; Original idea from Edward Nieters <nieters@crd.ge.com>.
  ;; In Emacs 19, I should use directory-abbrev-alist here.  (No, that's
  ;; already done for us.)
  (let ((repl-alist (cons (cons (getenv "HOME") "~")
                          buffer-menu-replacement-alist))
        from to)
    (while repl-alist
      (setq from (concat "[ \t\n]" (car (car repl-alist)))
            to (cdr (car repl-alist))
            repl-alist (cdr repl-alist))
      (goto-char (point-min))
      (while (re-search-forward from nil t)
        ;; This nonsense is because I don't want to throw off any groups
        ;; in buffer-menu-replacement-alist; I can't introduce a new
        ;; group before ones mentioned there.
        (replace-match (concat (buffer-substring (match-beginning 0)
                                                 (1+ (match-beginning 0)))
                               to))))))

;;;; TODO: instead, advise one of these functions:
;;;; Buffer-menu--pretty-name
;;;; Buffer-menu--pretty-file-name
;;; Set variable directory-abbrev-alist instead.
;;; This does not seem to be used.
;; ;; What I really want is an unsubstitute-in-file-name or
;; ;; abbreviate-file-name-for-display that is used for display and other
;; ;; contexts when we are sure that any input will be passed through
;; ;; substitute-in-file-name first.  This might require going through the
;; ;; Emacs source moderately carefully, which would be a pain.  For instance,
;; ;; tags-loop-continue doesn't even call abbreviate-file-name, which is a
;; ;; smidgen annoying.
;;
;; ;; helper function
;; (defun Buffer-menu-abbreviate-file-name (filename)
;;   (let ((replacements buffer-menu-replacement-alist))
;;     (while replacements
;;       (let* ((pattern (concat "^" (car (car replacements))))
;;           (replacement (cdr (car replacements))))
;;      (if (string-match pattern filename)
;;          (setq filename (replace-match replacement nil nil filename))))
;;       (setq replacements (cdr replacements))))
;;   filename)

(abbreviate-file-name "/home/mernst/research/version-control/LLMerge-paper")
(Buffer-menu--pretty-file-name "/home/mernst/research/version-control/LLMerge-paper")

;;;
;;; Omit temporary buffers
;;;

;; Made this a separate variable to avoid the mapconcat per invocation.
(defvar Buffer-menu-kill-regexp
  (concat (regexp-opt
           ;; list of strings (buffer prefixes), NOT regular expressions.
           '("*Buffer List*"
             "*Directory*"             ; for M-x revert-buffer
             "*Completions*"
             "*TeX background*"
             "*Definitions in: "
             "*info-perl*"
             "*ff-paths-locate*"
             "info dir"
             "sent reply to "
             "sent forward of "
             "sent resend of "
             ;; I think I don't need these
             ;; "*ispell*"
             ;; "*ispell choices*"
             ".newsrc-dribble"
             ".type-break"
             "#.type-break#"
             ))
          ;; This is a list of regular expressions
          "\\|"
          (mapconcat (function identity)
                     (list
                      ;; VM buffers, such as "INBOX Summary"
                      ".* Summary"
                      ".* Presentation"
                      )
                     "\\|"))
  "Regular expression matching lines to kill from buffer listings.
The regular expressions are implicitly anchored at the front.")

(defun buffer-menu--kill-some-lines (&optional _arg)
  (with-current-buffer "*Buffer List*"
    (let ((buffer-read-only nil))
      ;; Don't want "*" modified mark before "*Buffer List*", which is modified
      ;; by (list-buffers), so set-some-buffers-unmodified does no good.
      ;;   (goto-char (point-min))
      ;;   (replace-string "*  *Buffer List*" "   *Buffer List*")
      ;; Instead, don't show the "*Buffer List*" buffer at all.
      (Buffer-menu-kill-some-lines Buffer-menu-kill-regexp))
    (set-buffer-modified-p nil)))
(advice-add 'buffer-menu :after #'buffer-menu--kill-some-lines)

(defun Buffer-menu-kill-some-lines (regexp)
  "Delete lines in the buffer menu whose buffer name matches REGEXP."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat "^...[ \"]\\(" regexp "\\)\"?") nil t)
      (beginning-of-line)
      (delete-region (point) (progn (end-of-line) (point)))
      (if (bobp)
          (delete-char 1)
        (progn
          (backward-char 1)
          (delete-char 1))))))

;;;
;;; Erase the read-only marks and the "current" mark
;;;

;; `Buffer-menu-save', `Buffer-menu-execute', and probably other
;; functions assume that the modified mark is in the third column.
;; So don't move the columns.

(defun buffer-menu--erase-read-only-marks (&optional _arg)
  "Erase the read-only marks in the buffer menu.  They are a distraction."
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))

      ;; First, fix the header line (which is part of the text in Emacs 21).
      ;; TODO.  It may be a bit tricky because of all the text properties.

      ;; Now, fix each line.
      (while (not (eobp))
        (if (not (looking-at "[C .]"))
	    (error "Expected regex %s" "[C .]"))
        (replace-char-and-inherit " ")
        (if (not (looking-at "[R %]"))
	    (error "Expected regex %s" "[R %]"))
        (replace-char-and-inherit " ")
        (forward-line 1)))))
(advice-add 'buffer-menu :after #'buffer-menu--erase-read-only-marks)


(defun replace-char-and-inherit (newchar)
  "Replace the character at point by `newchar', but inherit its properties."
  ;; Gross implementation
  (forward-char 1)
  (insert-and-inherit newchar)
  (backward-char 2)
  (delete-char 1)
  (forward-char 1))


;;;
;;; Update the "*Buffer List*" buffer after `save-some-buffers'
;;;

;; TODO: Also see Global Auto Revert mode and
;; `global-auto-revert-non-file-buffers'.

;; I could also invert the sense of the argument.
(defun save-some-buffers--regenerate-buffer-menu (&optional _arg _pred)
  "Regenerate the buffer menu (\"*Buffer List*\" buffer), if it is visible."
  (save-window-excursion
    (walk-windows
     (function (lambda (window)
                 (if (equal "*Buffer List*" (buffer-name (window-buffer window)))
                     (buffer-menu nil))))
     'no-minibuffer)))
(advice-add 'save-some-buffers :after #'save-some-buffers--regenerate-buffer-menu)

(defun save-some-buffers--save-type-break (&optional _arg _pred)
  "Unconditionally save buffer\".type-break\"."
  (save-buffer-if-modified (get-buffer ".type-break")))
(advice-add 'save-some-buffers :before #'save-some-buffers--save-type-break)


;;;
;;; Put the cursor in the buffer menu
;;;

;; TODO: Instead of
;;   (switch-to-buffer (list-buffers-noselect arg))
;; as in original buffer-menu, I want to do
;;   (list-buffers arg)
;;   (pop-to-buffer "*Buffer List*")


(provide 'buffer-menu-mde)

;;; buffer-menu-mde.el ends here
