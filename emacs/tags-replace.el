;;; -*- lexical-binding: t -*-

;;; tags-replace.el -- defines `tags-replace' to complement `tags-query-replace'


;; The code in this file is `tags-query-replace' and `fileloop-initialize-replace'
;; from Emacs 30.1, slightly modified to not query the user.

;; Your .emacs file must autoload `tags-replace':
;; 

(defun tags-replace (from to &optional delimited files)
  "Do `replace-regexp' of FROM with TO on all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[fileloop-continue].

As each match is found, the user must type a character saying
what to do with it.  Type SPC or `y' to replace the match,
DEL or `n' to skip and go to the next match.  For more directions,
type \\[help-command] at that time.

For non-interactive use, this is superseded by `fileloop-initialize-replace'."
  (declare (advertised-calling-convention (from to &optional delimited) "27.1"))
  (interactive (query-replace-read-args "Tags query replace (regexp)" t t))
  (fileloop-initialize-replace-noquery
   from to
   (tags--compat-files (or files t))
   (if (equal from (downcase from)) nil 'default)
   delimited)
  (fileloop-continue))

(defun fileloop-initialize-replace-noquery (from to files case-fold &optional delimited)
  "Initialize a new round of query&replace on several files.
FROM is a regexp and TO is the replacement to use.
FILES describes the files, as in `fileloop-initialize'.
CASE-FOLD can be t, nil, or `default':
  if it is nil, matching of FROM is case-sensitive.
  if it is t, matching of FROM is case-insensitive, except
     when `search-upper-case' is non-nil and FROM includes
     upper-case letters.
  if it is `default', the function uses the value of
     `case-fold-search' instead.
DELIMITED if non-nil means replace only word-delimited matches."
  ;; FIXME: Not sure how the delimited-flag interacts with the regexp-flag in
  ;; `perform-replace', so I just try to mimic the old code.
  (let ((mstart (make-hash-table :test 'eq)))
    (fileloop-initialize
     files
     (lambda ()
       (let ((case-fold-search (fileloop--case-fold from case-fold)))
         (when (re-search-forward from nil t)
           ;; When we find a match, save its beginning for
           ;; `perform-replace' (we used to just set point, but this
           ;; is unreliable in the face of
           ;; `switch-to-buffer-preserve-window-point').
           (puthash (current-buffer) (match-beginning 0) mstart))))
     (lambda ()
       (let ((case-fold-search (fileloop--case-fold from case-fold)))
         (perform-replace from to t t delimited nil multi-query-replace-map
                          (gethash (current-buffer) mstart (point-min))
                          (point-max)))))))


(provide 'tags-replace)
