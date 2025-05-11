;;; -*- lexical-binding: t -*-

;;; sendmail-mde.el --- Mike's sendmail customizations

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'sendmail)
  (require 'mail-utils)
  )
(require 'mail-utils-mde)
(condition-case _ignore
    (require 'vm-autoloads nil 'noerror)    ; for vm-mime-attach-file and others
  (error nil))	; custom-autoload doesn't exist in older versions of custom
(autoload 'mail-text "sendmail")

(setq mail-setup-with-from nil)
(setq mail-self-blind t)		; force bcc to myself
(defadvice sendmail-user-agent-compose (before set-mail-self-blind activate)
  "Set `mail-self-blind' to t.
For some reason, this occasionally gets reset to nil,
probably by loading some package; I haven't tracked it down."
  (setq mail-self-blind t))

;; vm-reply-ignored-addresses is set in rmail-mde.el

(defadvice vm-save-killed-message-hook (around dont-do-it activate)
  "Disable 'vm-save-killed-message-hook'."
  )

;; login.csail.mit.edu doesn't have ispell installed
(eval-when-compile
 (condition-case _ignore
     (require 'ispell)
   (file-error (defun ispell-message () nil))))
(setq ispell-message-limit (* 500 80))
(setq ispell-message-cite-regexp "^\\(   \\|\t\\| >\\)")


;; (with-eval-after-load "ispell"
;;   (setq ispell-skip-region-alist
;; 	 (cons '("^[ \t]+-Michael Ernst" .
;; 		 "^[ \t]+\\(mernst@\\(cs.washington.edu\\|\\(alum.\\|lcs.\\|csail.\\)?mit.edu\\|cf-web.org\\)\\|sn-oversight@geyer.lcs.mit.edu\\)")
;; 	       ispell-skip-region-alist)))


(defvar ispell-message-suppress nil
  "True if messages should not be spell-checked.")

(defadvice ispell-message (around check-for-suppression activate)
  "If `ispell-message-suppress' is set, do nothing."
  (if (not ispell-message-suppress)
      ad-do-it))


(add-hook 'mail-mode-hook 'mde-mail-mode-hook)

;; I had put mde-mail-abbrevs-setup in mail-setup-hook (per the
;; documentation), but mail-setup-hook isn't executed by rmail-forward and
;; other ill-mannered functions; beware using it, as it is really only for
;; massaging the contents of the buffer.  I think this should work just as
;; well.
(eval-when-compile (require 'mailabbrev))
(eval-when-compile (require 'sendmail))
(defun mde-mail-mode-hook ()
  "Mike's mail mode hook."
  ;; It would be cleaner to do all this once, not every time.
  ;; (define-key mail-mode-map "\C-c\C-a" 'mail-interactive-insert-alias)
  (define-key mail-mode-map "\C-c\C-a" 'mde-mail-interactive-insert-alias)
  (if (fboundp 'vm-mail-send-and-exit)
      (define-key mail-mode-map "\C-c\C-c" 'vm-mail-send-and-exit)) ; handles MIME attachments

  (let* ((extra-paragraph-delimiters
	 "[ \t]*-\\(\\(Mike\\|Michael\\)\\( Ernst\\)?\\|Crusher\\)$\\|[ \t]*\\(Thanks\\|Cheers\\),$\\|")
	 ;; (delim-len (length extra-paragraph-delimiters))
         )
    (if (not (string-prefix-p extra-paragraph-delimiters paragraph-start))
	(progn
	  (setq paragraph-start (concat extra-paragraph-delimiters paragraph-start))
	  (setq paragraph-separate (concat extra-paragraph-delimiters paragraph-separate)))))

  (mde-mail-abbrevs-setup)
  (setq fill-column 70)			; some mail readers re-fill in an ugly way (mostly an issue if my email gets forwarded by someone who uses such a mail reader)
)

(add-hook
 'mail-setup-hook
 #'(lambda ()
    (substitute-key-definition 'next-line 'mail-abbrev-next-line
			       mail-mode-map global-map)
    (substitute-key-definition 'end-of-buffer 'mail-abbrev-end-of-buffer
			       mail-mode-map global-map)))

(defun mde-mail-alias (name definition)
  ;; Should this use define-mail-alias or define-mail-abbrev instead?
  (define-abbrev mail-abbrevs name definition 'mail-abbrev-expand-hook))

(defun mde-mail-abbrevs-setup ()
  "Read mail abbreviations.
This version attempts compatibility with various Emacs implementations."
  (if (and (or (not (boundp 'mail-abbrevs))
	       (not mail-abbrevs))
	   (file-exists-p "~/.mailrc.el"))
      (condition-case load-error
	  (load "~/.mailrc.el" nil t nil)
	;; Without this, there's no indication which file had a problem,
	;; because of the NOMESSAGE argument to load.
	(error (error "Problem loading ~/.mailrc.el: %s" load-error)))
    (progn
      ;; These lines come from the top of .mailrc.el.
      ;; They principally define abbrev-hacking-next-line.
      (require 'mailabbrev)
      ;; used by 19 only, but indicates this file has been read
      (define-abbrev-table 'mail-abbrevs '())))
  ;; Do this even if mail-abbrevs is correctly set, as we want this
  ;; to take effect in the *mail* buffer (which might have been deleted
  ;; and just now recreated).
  (mail-abbrevs-setup))

(defadvice mail-yank-original (around toggle-mail-yank-prefix activate)
  "With \\[universal-argument] \\[universal-argument] argument, toggle the value of `mail-yank-prefix'.
That is, toggle between indenting or inserting `mail-yank-prefix'
\(default \"> \") before each line."
  (let* ((double-universal-prefix (and (consp arg) (equal (car arg) 16)))
	 (mail-yank-prefix (if double-universal-prefix
			       (if mail-yank-prefix
				   nil
				 "> ")
			     mail-yank-prefix)))
    ;; mail-yank-original treats any sequence of C-u just like single one,
    ;; so make it look like there was no prefix argument at all.
    (if double-universal-prefix
	(ad-set-arg 0 nil))
    ad-do-it))


;; TODO:  This code should have mail-send-hook set mail-send-actions.
(defadvice mail-send (after set-mode-line activate)
  (mail-sent-mode-line))

(defun mail-sent-mode-line ()
  "Mail mode line containing string \"sent\"."
  ;; Really, shoudl jsut add ", sent" without making other changes.
  (setq mode-line-format
        "-%1*%1*- %b      %f %[(%m, sent)%]--%3p--%M%-"))


;; I'm trying out not using this.  -MDE 1/2/2006
(defun mail-send-and-exit-nicely ()
  "Send message like `mail-send', then, if no errors, exit from mail buffer."
  (interactive)
  (mail-send)
  (bury-buffer (current-buffer))
  (if (eq (next-window (selected-window)) (selected-window))
      (switch-to-buffer (other-buffer (current-buffer)))
    (delete-window)))

(defun mail-recipients (&optional primary-only)
  "Return a string of comma-delimited email addresses, or nil if no recipients.
Optional argument PRIMARY-ONLY means use only To: field, not also Cc: field."
  (require 'mail-utils)
  (save-excursion
    (save-restriction
      ;; Narrow to header, which is required by mail-fetch-field
      (goto-char (point-min))
      (re-search-forward (concat
			  "^" (regexp-quote mail-header-separator) "\n"))
      (narrow-to-region (point-min) (match-beginning 0))
      (let ((recipients (mail-fetch-field "To" nil t))
	    (cc-recipients (and (not primary-only)
				(mail-fetch-field "Cc" nil t))))
	(if (and recipients cc-recipients)
	    (concat recipients ", " cc-recipients)
	  (or recipients cc-recipients))))))

;; Also see rmail-message-recipients-p.
(defun mail-recipient-p (recipient &optional primary-only)
  "Return non-nil if RECIPIENT is a recipient of the mail message.
Optional argument PRIMARY-ONLY means only check To: field, not also Cc: field."
  (let ((recipients (mail-recipients primary-only)))
    (and recipients
	 (string-match recipient recipients))))

(defun empty-draft-body-p ()
  "Return whether the draft in the current buffer has an empty body."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward mail-header-separator nil t)
	 (looking-at "\n*\\'"))))

(defun query-if-empty-draft-body ()
  (if (and (empty-draft-body-p)
	   (not (y-or-n-p "Message has empty body. Send anyway? ")))
      (error "Message has empty body. Not sent.")))

(add-hook 'mail-send-hook 'mde-mail-send-hook)
(add-hook 'mail-send-hook 'ispell-message)

(defun mde-mail-send-hook ()
  "Michael Ernst's mail send hook.
For Mew, put on `mew-make-message-hook'."

  ;; Check subject line
  (check-empty-subject-line)
  (check-cvs-subject-line)

  ;; Check addresses
  (check-from-address)
  (check-bcc-vs-dcc)
  (mail-to-support-add-please)
  (mail-to-professional-no-crusher)
  (steven-lyubomirsky-spelling)

  ;; Check other headers
  (check-for-blank-line-in-header)

  ;; Check body
  (check-emacs-patches-for-changelog)
  (query-if-empty-draft-body)
  ;; Handled separately for Mew and other systems: (ispell-message)
  ;; (love22-mail-to-eit)
  )

;; Is this necessary any more?
;; (defun clean-local-addresses ()
;;   "Shorten local email addresses (for terseness in header).
;; Replaces \"foo@bar.cs.washington.edu\" by \"foo@cs.washington.edu\"."
;;   (cse					; could generalize to other sites
;;     (save-excursion
;;       (goto-char (point-min))
;;       (if (search-forward mail-header-separator nil t)
;; 	  (let ((end (point)))
;; 	    (goto-char (point-min))
;; 	    (while (re-search-forward "@\\([a-z]+\\.\\)?cs\\.washington\\.edu\\>" end t)
;; 	      (if (save-match-data
;; 		    (save-excursion
;; 		      (beginning-of-line)
;; 		      (while (looking-at "[ \t]")
;; 			(forward-line -1))
;; 		      (looking-at "\\(Resent-\\)?\\(To\\|Cc\\):")))
;; 		  ;; This looks less cluttered to the recipient, but it messes
;; 		  ;; up my bbdb database, so don't use it.
;; 		  ;; (replace-match "@cs")
;; 		  (replace-match "@cs.washington.edu")))
;; 	    ;;; Old implementation
;; 	    ;; 	    (while (and (re-search-forward "^\\(\\(To\\|Cc\\):.*\\)@cs\\.washington\\.edu\\([,\n>]\\)" end t)
;; 	    ;; 	      (replace-match "\\1@cs\\3")))
;; 	    )))))

;; (defun love22-mail-to-eit ()
;;   "If sending mail to eit, offer to love22 it first."
;;   ;; Quick and very dirty determination of destination.  (Isn't there
;;   ;; a better way to do this in Emacs 19?)
;;   (if (and (mail-recipient-p "\\<eit@mit\\.edu")
;; 	   (y-or-n-p "Love22 the message? "))
;;       (progn
;; 	(require 'love22)
;; 	(mail-text)
;; 	(love22-region (point) (point-max))
;; 	(redraw-display)
;; 	(if (y-or-n-p "Recursive edit? ")
;; 	    (progn
;; 	      ;; do I need a "widen" here ???
;; 	      (message "M-C-c to exit recursive edit and send.")
;; 	      (recursive-edit))))))

(defun mail-to-support-add-please ()
  "If sending mail to support, check for the word \"please\".
This helps a lot (especially integrated over time)."
  (if (and (mail-recipient-p (regexp-quote "\\<support\\>"))
	   (save-excursion
	     (not (and (progn (mail-text)
			      (search-forward "\\<please\\>" nil t))
		       (progn (mail-text)
			      (search-forward "\\<thank\\(s\\|[ \n]you\\)\\>" nil t)))))
	   (not (y-or-n-p "Missing \"please\" or \"thanks\".  Send to support anyway? ")))
      (error "Message to support without \"please\" or \"thanks\"")))


(defun steven-lyubomirsky-spelling ()
  "If sending mail to Steven Lyubomirsky, spell his name right."
  ;; Quick and very dirty determination of destination.  (Isn't there
  ;; a better way to do this in Emacs 19?)
  (if (and (mail-recipient-p "\\<sslyu@")
	   (save-excursion
	     (mail-text)
	     (search-forward "Stephen" nil t))
	   (y-or-n-p "Mail contains \"Stephen\". Query replace? "))
      (save-excursion
	(mail-text)
	(query-replace-regexp "Stephen" "Steven"))))

(defun mail-to-professional-no-crusher ()
  "Don't send mail to professional addresses signed \"Crusher\"."
  (if (and (or ;; (mail-recipient-p "@\\(cs\\.\\|owlnet\\.\\)?rice\\.edu")
	    (mail-recipient-p "[@.]\\(\\(csail\\|lcs\\)\\.mit\\|rice\\|washington\\)\\.edu")
	    (and (memq system-site '(csail cse))
		 (or (mail-recipient-p "@cs[, \n]")
		     (and (mail-recipients)
			  (not (mail-recipient-p "@"))))))
	   (save-excursion (progn (mail-text)
				  (search-forward "-Crusher" nil t)))
	   (not (y-or-n-p "Message to CSAIL, UW, or Rice signed \"Crusher\".  OK? ")))
      (error "Message to CSAIL, UW, or Rice signed \"Crusher\"")))

(defun check-for-blank-line-in-header ()
  "Warn if mail contains a blank line in its header."
  (save-excursion
    (goto-char (point-min))
    (search-forward (concat "\n" mail-header-separator "\n"))
    (if (re-search-backward "\n[ \t]*\n" nil t)
	(error "Blank line in mail header"))))

(defun check-emacs-patches-for-changelog ()
  "Warn if mail containing an Emacs patch is malformed.
It is malformed if it doesn't contain a Changelog,
or if the patch is not in context diff format."
  (if (or (mail-recipient-p (regexp-quote "bug-gnu-emacs@\\(gnu.org\\|prep.ai.mit.edu\\)"))
	  (and (mail-recipient-p (regexp-quote "rms@ai.mit.edu"))
	       (mail-recipient-p (regexp-quote "kwzh@gnu.ai.mit.edu"))))
      (progn
	(if (progn (mail-text)
		   (and (or (search-forward "diff -u" nil t)
			    (re-search-forward "\n--- .*\n\\+\\+\\+ " nil t))
			(not (y-or-n-p "Should use context diff format for patches sent to Stallman; send anyway? "))))
	    (error "Use context diff format for patches sent to Stallman"))
	(if (and (progn (mail-text)
			(or (search-forward "diff -c" nil t)
			    (re-search-forward "\n\\*\\*\\* .*\n--- " nil t)))
		 (not (progn (mail-text)
			     (search-forward "ChangeLog" nil t)))
		 (not (y-or-n-p "No ChangeLog entry included.  Send anyway? ")))
	    (error "No ChangeLog entry included with patches")))))

(defvar check-empty-subject-line-disable nil
  "Non-nil if the `check-empty-subject-line' check should be disabled.")

(defun check-empty-subject-line ()
  "Warn if sending mail with empty subject."
  ;; I don't understand how check-empty-subject-line-disable can be unbound
  ;; even when this file is loaded, but that somehow happens, so check.
  (if (or (not (boundp 'check-empty-subject-line-disable))
	  (not check-empty-subject-line-disable))
      (let ((subject (mail-fetch-field "subject")))
	(require 'rmail)		; for rmail-reply-regexp
	(if (and subject
		 (string-match (concat "^\\(" rmail-reply-regexp "\\)?$")
			       subject)
		 (not (y-or-n-p "Send mail with empty subject line? ")))
	    (error "Message has empty subject line")))))

(defun check-cvs-subject-line ()
  "Warn if sending mail with non-informative subject \"Re: CVS checkin ...\"."
  (let ((subject (mail-fetch-field "subject")))
    (require 'rmail)			; for rmail-reply-regexp
    (if (and subject
	     (string-match (concat "^\\(" rmail-reply-regexp "\\)"
				   ;; ends with a four-digit year
				   "CVS \\(checkin\\|update\\|commit\\)")
			   subject)
	     (not (y-or-n-p "Send mail with \"Re: CVS checkin\" subject line? ")))
	(error "Message has \"Re: CVS checkin\" Subject line"))))

(defvar check-from-address-disable nil
  "If non-nil, disable `check-from-address':  don't warn about \"From:\" line.")

(defun check-from-address ()
  "Warn if sending mail with wrong \"From:\" line."
  (if (not check-from-address-disable)
      (let* ((from (mail-fetch-field "from"))
	     ;; (to (mail-fetch-field "to"))
	     (goal-from
	      ;; lifted from `rmail-output-read-rmail-file-name'
	      (let (answer tail)
		(setq tail sendmail-from-address-alist)
		;; Suggest a from address based on a pattern match.
		(while (and tail (not answer))
		  (save-excursion
		    (if (eq major-mode 'rmail-summary-mode)
			(set-buffer rmail-buffer))
		    (goto-char (point-min))
		    (if (re-search-forward (car (car tail)) nil t)
			(setq answer (eval (cdr (car tail)))))
		    (setq tail (cdr tail))))
		answer)))
	(if (and goal-from (not (equal from goal-from)))
	    ;; These two questions might get annoying.
	    (cond ((y-or-n-p (concat "Change From line to " goal-from " ? "))
		   (mail-set-field "From" goal-from t)
		   (mail-set-field "Dcc" goal-from t)
		   )
		  ((not (y-or-n-p (concat "Send with "
					  (if from "existing" "no explicit")
					  " From line? ")))
		   (error "Unexpected From line")))))))

(defvar sendmail-from-address-alist nil
  "*Alist matching regexps to \"From:\" lines.
This is a list of elements of the form (REGEXP . FROM-ADDRESS).")
;; set sendmail-from-address-alist
(require 'rmail-addresses-mde)

(defun check-bcc-vs-dcc ()
  "Warn if sending mail with bcc vs dcc."
  (if (or (not (boundp 'check-empty-subject-line-disable))
	  (not check-empty-subject-line-disable))
      (let ((bcc (mail-fetch-field "bcc")))
	(if (and bcc
		 (not (y-or-n-p "Send mail with bcc (you might prefer dcc)? ")))
	    (error "Message has bcc field; change to dcc")))))


(defvar disable-warn-when-kill-mail nil)
(defadvice kill-buffer (before warn-when-kill-mail activate)
  "Verify when a modified mail buffer is killed.
This advice is disabled if variable `disable-warn-when-kill-mail' is non-nil."
  (if (not disable-warn-when-kill-mail)
      (let ((buf (ad-get-arg 0)))
	(if (or (not buf) (buffer-live-p buf))
	    (save-excursion
	      (if buf
		  (set-buffer buf))
	      (if (and (eq major-mode 'mail-mode)
		       (buffer-modified-p)
		       (not
			;; empty body
			(save-excursion
			  (goto-char (point-min))
			  (and (search-forward mail-header-separator nil t)
			       (looking-at "\n\\'"))))
		       (not (y-or-n-p (format "Mail buffer %s modified; really kill? "
					      (current-buffer)))))
		  (error "Tried to kill modified mail buffer %s" (current-buffer))))))))

(defadvice mail-strip-quoted-names (after add-post-comma-space activate)
  "Add spaces after commas."
  (while (string-match ",\\([^ \t]\\)" ad-return-value)
    (setq ad-return-value (replace-match ", \\1" nil nil ad-return-value))))
;; Testing:
;; (mail-strip-quoted-names "David Notkin <notkin@cs.washington.edu>, Cat Howell <scath@bu.edu>,Michael Ernst <mernst@csail.mit.edu>")


(defun delete-uninteresting-sendmail-headers ()
  "Remove distracting fields from a message being composed."
  (mail-delete-field "X-Mailer")
  (mail-delete-field "Message-ID")
  )

(add-hook 'mail-setup-hook 'delete-uninteresting-sendmail-headers)

(defun protect-mail-auto-save-files ()
  "Make my autosaved message files unreadable by others.
Does not work on AFS."
  (let ((files (directory-files "~/" 'full-pathname "^#\\*mail\\*#"))
	file modes)
    (while files
      (setq file (car files)
	    modes (file-modes file)
	    files (cdr files))
      (if (not (zerop (mod modes 64)))
	  ;; Others or group can read or write or execute, because one of
	  ;; the lower two octal digits was non-zero.  Zero them.
	  (set-file-modes file (- modes (mod modes 64)))))))

;; This is non-ideal, because it runs BEFORE, not after, the auto-save happens.
;; But the right thing will happen the next time that auto-save occurs.
(add-hook 'auto-save-hook 'protect-mail-auto-save-files)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remote mail sending
;;;

;; From http://emacswiki.org/cgi-bin/wiki/SendingMail :

;;(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
;;(setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
;;(setq smtpmail-default-smtp-server "YOUR SMTP HOST")
;;(setq smtpmail-local-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-sendto-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-debug-info t) ; only to debug problems
;;(setq smtpmail-auth-credentials  ; or use ~/.authinfo
;;      '(("YOUR SMTP HOST" 25 "username" "password")))
;;(setq smtpmail-starttls-credentials
;;      '(("YOUR SMTP HOST" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))
;; Where the 25 equals the value of `smtpmail-smtp-service', it can be an
;; integer or a string, just as long as they match (eq).

;; Sometimes, it can be useful to just queue the mails and send them later all
;; at once (dialup users, etc.), then just add:
;;   (setq smtpmail-queue-mail t)
;; When your network connection is ready, just do
;    M-x smtpmail-send-queued-mail RET


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

(provide 'sendmail-mde)

;;; sendmail-mde.el ends here
