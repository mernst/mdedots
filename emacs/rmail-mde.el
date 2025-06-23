;;; -*- lexical-binding: t -*-

;;; rmail-mde.el --- Michael Ernst's setup file for mail reading (VM, RMAIL)

;;; Commentary:

;; For sending mail, see sendmail-mde.el, not this file.

;; I use Mew, not Rmail or VM.

;;; Code:

(eval-when-compile
  (condition-case _err
      (require 'vm)
    (require 'vm-message)		; for vm-deleted-flag macro
    (require 'vm-rfaddons)		; for vm-fill-paragraphs-containing-long-lines advice
    (error nil))
  (require 'sendmail)			; for mail-bcc, etc.; should such
					;   code go in sendmail-mde.el?
  (require 'sendmail-mde)
  (require 'mailalias)			; for expand-mail-aliases; doesn't work?
  )

;; VM
;; (setq vm-enable-addons nil)		; trying to debug troubles
                                        ; This seems to be badly broken:  I get the whole INBOX file quoted.
                                        ; (setq vm-reply-include-presentation t)

;; (require 'vm-autoloads)
(add-to-list 'Info-default-directory-list
	     (expand-file-name "~/emacs/vm-8.0.11-581/info"))

(require 'mail-utils-mde)	; Utility procedures for manipulating mail.
(require 'mail-simplify)

(setq vm-delete-after-saving t)
(setq vm-delete-after-bursting t)
(setq vm-confirm-new-folders t)
(require 'rmail-addresses-mde)

(setq vm-confirm-quit nil)		; don't require confirmation
(setq vm-preview-lines nil)		; see whole message
(setq vm-preview-read-messages nil)	; preview messages that are marked as read
(setq vm-move-after-deleting t)
(setq vm-auto-center-summary nil)	; default t (for auto-centering)

(setq vm-mutable-frames nil)		; don't create/destroy frames
(setq vm-use-toolbar nil)

(setq vm-jump-to-new-messages nil) ; jump to any unread message, not just new ones

(setq vm-search-using-regexps t)

;; Get new mail when VM is invoked (not asynchronously nor requiring explicit command).
(setq vm-auto-get-new-mail t)
;; Note that if new mail is found, it is not retrieved.  The
;; buffer local variable `vm-spooled-mail-waiting' is set non-nil in
;; the buffers of those folders that have mail waiting.
(setq vm-mail-check-interval nil)


;; Note that VM disables highlight-headers (by dynamically binding
;; highlight-headers-mark-urls to nil) because it uses its own URL highlighting.
(setq vm-url-browser		; default w3-fetch
      (if (eq window-system 'x)
	  ;; See comment below regarding why I don't use vm-mouse-send-url-to-netscape
	  ;; 'vm-mouse-send-url-to-netscape
	  'browse-url-and-echo
	'w3m))
(setq vm-netscape-program "firefox")
;; (setq vm-highlight-url-face 'default)
(setq vm-url-search-limit 100000)	; default 12000

;; When I run Emacs via ssh from my laptop, then when Emacs sends a URL, it
;; is displayed on the local browser running on my laptop.  It takes a
;; while for the page to display (probably due to interprocess
;; communication).  The standard vm-mouse-send-url hangs during that time
;; (I'm not sure why), but browse-url does not.
(defun browse-url-and-echo (url)
  (browse-url url)
  (message "Browsing %s" url))

;; Use W3M to read HTML email, as it is faster than W3
(if (locate-library "w3m-load")
    (progn
      (require 'w3m-load)
      (setq vm-mime-use-w3-for-text/html nil)
      (with-eval-after-load "vm-vars"
	(load "vm-w3m") ;; Might be obsoleted?
	(setq w3m-input-coding-system 'utf-8
	      w3m-output-coding-system 'utf-8))))


;;
;; Extra spool files (for delayed/autofiled mail)
;;

(defvar vm-extra-spool-files
  ;; These are in the opposite order from what one might expect so that the
  ;; easier ones are presented first; after addressing those, I can do more
  ;; sophisticated sorting on the remaining ones.
  (reverse '("hourly"
	     "daily" "jml" "seajug" "talks" "daily-403"
	     "weekly" "hkn"
	     "monthly"
             ;; "later"
	     )))

(defun uw-imap-spool-file (folder)
  "Returns a spool file descriptor for the given folder."
  ;; Does not support cram-md5 authentication.
  ;; CSE IMAP
  ;; (concat "imap:mernst.mail.cs.washington.edu:143:" folder ":login:mernst:*")
  ;; CSE IMAP via ssl tunneling (encrypts message bodies as well as password)
  ;; (concat "imap-ssl:mernst.mail.cs.washington.edu:993:" folder ":login:mernst:*")
  (concat "imap-ssl:imap.gmail.com:993:" folder ":login:mernst@cs.washington.edu:*")
  )

(defun gmail-imap-spool-file (folder)
  "Returns a spool file descriptor for the given folder."
  (concat "imap-ssl:imap.gmail.com:993:" folder ":login:michael.ernst:*"))

(defun csail-imap-spool-file (folder)
  "Returns a spool file descriptor for the given folder."
  ;; CSAIL IMAP
  ;; (concat "imap:imap.csail.mit.edu:143:" folder ":cram-md5:mernst:*")
  ;; CSAIL IMAP via ssl tunneling (encrypts message bodies as well as password)
  (concat "imap-ssl:imap.csail.mit.edu:993:" folder ":cram-md5:mernst:*")
  )

(defun vm-extra-spool-file (drop)
  "Returns either a spool file descriptor or nil.
DROP is \"hourly\", \"daily\", etc."
  (let ((time-to-read-file (substitute-in-file-name
			    (concat "$HOME/.mail/.time-to-read-" drop))))
    (if (file-exists-p time-to-read-file)
	(progn
	  (delete-file time-to-read-file)
	  ;; return the descriptor
	  (csail-imap-spool-file (concat "inbox.delay-" drop))))))

(defadvice vm-get-spooled-mail (around add-extra-spool-files activate)
  (let ((vm-spool-files
	 (append vm-spool-files
		 (remove nil
			 (mapcar (function vm-extra-spool-file)
				 vm-extra-spool-files)))))
    ad-do-it))

(defun disable-add-extra-spool-files ()
  "Disable the `add-extra-spool-files' advice of `vm-get-spooled-mail'."
  (ad-disable-advice 'vm-get-spooled-mail 'around 'add-extra-spool-files)
  (ad-activate 'vm-get-spooled-mail))
(defun enable-add-extra-spool-files ()
  "Enable the `add-extra-spool-files' advice of `vm-get-spooled-mail'."
  (ad-enable-advice 'vm-get-spooled-mail 'around 'add-extra-spool-files)
  (ad-activate 'vm-get-spooled-mail))

;;
;; Headers
;;

;; Make headers less gaudy.
(setq vm-display-xfaces nil)
(setq highlight-headers-hack-x-face-p nil)
(with-eval-after-load "highlight-headers"
  (copy-face 'default 'message-headers)
  (copy-face 'default 'message-header-contents)
  (copy-face 'default 'message-cited-text)

  (setq highlight-headers-follow-url-function 'highlight-headers-follow-url-netscape)

  (copy-face 'default 'message-url)
  )

;; Need to set vm-visible-headers and perhaps vm-invisible-header-regexp.
;;   If you change the value of either `vm-visible-headers' or
;;   `vm-invisible-header-regexp' in the middle of a VM session the effects
;;   will not be immediate.  You will need to use the command
;;   `vm-discard-cached-data' on each message (bound to `j' by default) to
;;   force VM to rearrange the message headers.  A good way to do this is
;;   to mark all the messages in the folder and apply
;;   `vm-discard-cached-data' to the marked messages.

;;; This isn't quite what I want:  I only want "Reply-to:" to be visible if
;;; it is different from "From:" and/or "Sender:".
;; Add "Reply-to:" to vm-visible-headers, before "To:"
(with-eval-after-load "vm-vars"
  (if (not (member "Reply-to:" vm-visible-headers))
      (let ((to-part (member "To:" vm-visible-headers)))
	(if (not to-part)
	    (error "Didn't find \"To:\" in vm-visible-headers")
	  (setcdr to-part (cons (car to-part) (cdr to-part)))
	  (setcar to-part "Reply-to:")))))

;;
;; Key bindings
;;

(with-eval-after-load "vm-vars"
  ;; `vm-fill-paragraphs-containing-long-lines' is not an interactive function.
  (define-key vm-mode-map "\eq" 'vm-fill-message)
  (define-key vm-mode-map "H" 'vm-output-as-not-spam) ; "H" for "ham"
  (define-key vm-mode-map "S" 'vm-output-as-missed-spam) ; "S" for "spam"

  ;; RMAIL-like bindings
  (define-key vm-mode-map "R" 'vm-followup) ; was vm-reply-include-text
  (define-key vm-mode-map "f" 'vm-forward-message) ; was vm-followup
  (define-key vm-mode-map "o" 'vm-save-message)    ; was unbound
  (define-key vm-mode-map "x" 'vm-expunge-folder) ; was vm-quit-no-change
  ;; Instead of doing this, I could just get used to expunging by hand.  Nah.
  (define-key vm-mode-map "q" 'vm-quit-rmail-style) ; was vm-quit

  ;; For compatility with mail-mode
  (define-key vm-mail-mode-map "\C-c\C-a" 'mde-mail-interactive-insert-alias) ; was vm-mime-attach-file

  (setq vm-auto-displayed-mime-content-types
	(delete "image" vm-auto-displayed-mime-content-types))

     ;;; A problem with putting "text/html" in the exceptions list is that
     ;;; it causes quoting to yank the HTML instead of the displayed yersion.
  ;; ;; Require an affirmative action on HTML messages.  This is somewhat
  ;; ;; annoying, but it prevents info that would otherwise be sent back to
  ;; ;; spammers when I auto-viewing HTML messages (and displaying those
  ;; ;; spam messages can also be somewhat slow).  So I need to balance
  ;; ;; those two things.  Best would be to forbid retrieving remote
  ;; ;; content when viewing an email message, but I don't know how to do
  ;; ;; that.
  ;; ;; I can always use `browse-url-of-region' to view a region, though if
  ;; ;; it is MIME-encoded I probably have to save to a file first.
  ;; (setq vm-auto-displayed-mime-content-type-exceptions '("text/html"))

  (add-to-list 'vm-mime-type-converter-alist
	       '("text/html" "text/plain"
                 ;; to display in an external browser:
                 ;; "sendurl"
		 "lynx -force_html -dump -stdin"
		 ))

  (setq vm-mime-external-content-types-alist
	'(
	  ("text/html" 	"firefox")
	  ("image/gif" 	"xv")
	  ("image/jpeg" 	"xv")
	  ("video/mpeg" 	"mpeg_play")
	  ("video" 	"xanim")
          ;; ("application/pdf" "acroread")
	  ("application/pdf" "papers" "evince")
	  ("application/msword" office-program)
	  ("application/vnd.ms-powerpoint" office-program)
	  ("application/vnd.ms-excel" office-program)
	  )
	)

  (setq vm-mime-delete-viewer-processes nil)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup and shutdown
;;;

;; Avoid killing the buffer, which necessitates rereading the file every
;; time I want to get new mail.
(defun vm-quit-rmail-style ()
  "Expunge and save VM, and bury (don't kill) buffers."
  (interactive)
  (vm-expunge-folder)
  (vm-save-folder)
  (vm-quit-just-bury))

;; Advice doesn't work:  the message doesn't get printed.
;; So redefine it instead.
(with-eval-after-load "vm-folder"
  (defun vm-emit-totals-blurb ()
    "Just output number of new messgaes: not total, unread, or deleted."
    (save-excursion
      (vm-select-folder-buffer)
      (if (not (equal (nth 0 vm-totals) vm-modification-counter))
	  (vm-compute-totals))
      (let ((new-messages (nth 2 vm-totals)))
	(if (= new-messages 0)
	    (message "No new messages.")
	  (message "%d new message%s."
		   new-messages (if (= new-messages 1) "" "s")))))))


;; For certain messages (notably those from Karen Shirer), the headers are
;; not displayed (the window starts at the message body).  I can fix this
;; by adding "(sit-for 0)" to the body of vm-show-current-message, after
;; the call to vm-decode-mime-message but before the save-excursion
;; containing "(widen) (narrow-to-region ...)".  This advice effectively
;; does that.  It can cause a touch of flickering for messages that it
;; corrects, and I still don't fully understand the underlying problem, but
;; it seems like a satisfactory solution for now.
(defadvice vm-decode-mime-message (after redisplay activate)
  (sit-for 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mail reading
;;;

(setq vm-primary-inbox (expand-file-name "~/.mail/INBOX"))
;; Could set vm-crash-box too, but the default is probably fine.
(setq vm-spool-files (list
		      ;; CSAIL IMAP
		      (csail-imap-spool-file "inbox")
		      ;; With the sent-mail box, I get "vm-imap-select-mailbox: server said NO to SELECT".  So use bcc instead.
		      ;; (csail-imap-spool-file "sent-mail")
		      ;; UW IMAP: I'm not using it as there's no Sieve support
		      ;; (uw-imap-spool-file "inbox")
		      ))
;; This shoudln't be necessary if path is set correctly (?).
;; (cond ((file-exists-p "/usr/sbin/stunnel")
;;        (setq vm-stunnel-program "/usr/sbin/stunnel"))
;;       ((file-exists-p "/usr/bin/stunnel")
;;        (setq vm-stunnel-program "/usr/bin/stunnel")))

(if (file-exists-p '"/usr/bin/stunnel4")
    (setq vm-stunnel-program '"/usr/bin/stunnel4"))


;; to use, run: vm-visit-imap-folder
(setq vm-imap-server-list
      (list
       (uw-imap-spool-file "inbox")
       ;; (gmail-imap-spool-file "inbox")
       )
      )
;; (call-interactively 'vm-visit-imap-folder)

;; I don't like the way VM interacts with IMAP, so I am not using it.
(setq vm-imap-folder-cache-directory "~/.mail/imap-cache")

;; Bug in vm-imap-select-mailbox when the mailbox contains a space:  a
;; SELECT command is sent without quoting the space, and the prefix of the
;; mailbox name up to the space does.
(with-eval-after-load "vm-imap"
  (defun vm-imap-select-mailbox (process mailbox &optional just-examine)
    (let ((imap-buffer (current-buffer))
	  (command (if just-examine "EXAMINE" "SELECT"))
	  tok response p
	  (flags nil)
	  (permanent-flags nil)
	  (msg-count nil)
	  (uid-validity nil)
	  (read-write (not just-examine))
	  (can-delete t)
	  (need-ok t))
      ;; QUOTES ADDED BY MDE
      (vm-imap-send-command process (format "%s \"%s\"" command mailbox))
      (while need-ok
        (setq response (vm-imap-read-response process))
        (if (vm-imap-response-matches response 'VM 'NO)
	    (error "server said NO to %s" command))
        (if (vm-imap-response-matches response 'VM 'BAD)
	    (vm-imap-protocol-error "server said BAD to %s" command))
        (cond ((vm-imap-response-matches response '* 'OK 'vector)
	       (setq p (cdr (nth 2 response)))
	       (cond ((vm-imap-response-matches p 'UIDVALIDITY 'atom)
		      (setq tok (nth 1 p))
		      (setq uid-validity (buffer-substring (nth 1 tok)
							   (nth 2 tok))))
		     ((vm-imap-response-matches p 'PERMANENTFLAGS 'list)
		      (setq permanent-flags (nth 1 p)))))
	      ((vm-imap-response-matches response '* 'FLAGS 'list)
	       (setq flags (nth 2 response)))
	      ((vm-imap-response-matches response '* 'atom 'EXISTS)
	       (setq tok (nth 1 response))
	       (goto-char (nth 1 tok))
	       (setq msg-count (read imap-buffer)))
	      ((vm-imap-response-matches response 'VM 'OK '(vector READ-WRITE))
	       (setq need-ok nil read-write t))
	      ((vm-imap-response-matches response 'VM 'OK '(vector READ-ONLY))
	       (setq need-ok nil read-write t))
	      ((vm-imap-response-matches response 'VM 'OK)
	       (setq need-ok nil))))
      (if (null flags)
	  (vm-imap-protocol-error "FLAGS missing from SELECT responses"))
      (if (null msg-count)
	  (vm-imap-protocol-error "EXISTS missing from SELECT responses"))
      (if (null uid-validity)
	  (vm-imap-protocol-error "UIDVALIDITY missing from SELECT responses"))
      (setq can-delete (vm-imap-scan-list-for-flag flags "\\Deleted"))
      (list msg-count uid-validity read-write can-delete permanent-flags) ))
  )

(setq vm-auto-get-new-mail nil)


;;
;; Filling messages
;;

;; VM doesn't have a command for refilling; you must rely on function
;; `vm-fill-paragraphs-containing-long-lines', which is automatically
;; invoked if variable `vm-fill-paragraphs-containing-long-lines' is set.
;; That variable affects display, not on-disk version.
;; Equally badly, VM doesn't have a command for unfilling, so it's
;; inconvenient to undo a poor filling decision that VM makes.

;; Setting the variable to 80 fills too many paragraphs.
;; Setting the variable larger, such as 100, fills fewer paragraphs.
;; (setq vm-fill-paragraphs-containing-long-lines nil)
(setq vm-fill-paragraphs-containing-long-lines-faster nil) ;; Defined in vm-rfaddons.el

;; vm-rfaddons-better-filling contains a bug.
;; The specification of vm-fill-paragraphs-containing-long-lines is that it:
;;  * fills every paragraph that contains a line
;;    of length > vm-fill-paragraphs-containing-long-lines
;;  * fills those paragraphs to vm-paragraph-fill-column
;; But vm-rfaddons-better-filling uses the same variable both to determine
;; whether to fill, and to determine to what column to fill.
;; So, disable the bug.
(with-eval-after-load "vm-rfaddons"
  (if (ad-is-advised 'vm-fill-paragraphs-containing-long-lines)
      (progn
	(ad-disable-advice 'vm-fill-paragraphs-containing-long-lines 'around 'vm-rfaddons-better-filling)
	(ad-activate 'vm-fill-paragraphs-containing-long-lines))))



(defun vm-fill-message (&optional all-paragraphs)
  "Fill long paragraphs in the message, or all paragraphs with optional arg.
To unfill, move off the message and then back on, which causes redisplay."
  (interactive "P")
  (vm-follow-summary-cursor)
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-empty)
  (let ((vm-fill-paragraphs-containing-long-lines (if all-paragraphs 1 100)))
    (vm-preview-current-message)))


;;
;; Spam and bulk mail
;;

;; No longer revelant; I don't use VM.
(defun get-bulk-mail ()
  "Move my bulk mail from IMAP to local file ~/.mail/.maildrop-bulk.
After running this, run from the shell:  print-mail bulk."
  (interactive)
  (disable-add-extra-spool-files)
  (let ((vm-primary-inbox (expand-file-name "~/.mail/.maildrop-bulk"))
	(vm-spool-files (list (csail-imap-spool-file "inbox.delay-bulk"))))
    (vm)
    (vm-save-folder))
  (enable-add-extra-spool-files))


(defun get-nonstandard-mail (spoolfile localfile)
  "Move mail from an IMAP spool to a local file."
  (disable-add-extra-spool-files)
  (let ((vm-primary-inbox (expand-file-name localfile))
	(vm-spool-files (list spoolfile))
	(large-file-warning-threshold 100000000))
    (setq vm-auto-decode-mime-messages nil)
    (vm))
  (enable-add-extra-spool-files))

;; After running this, should probably start a new Emacs.
(defun get-spam-mail (&optional mailbox)
  "Move my spam (junk) mail from IMAP to local file spam.mail."
  (interactive)
  (if (file-exists-p "~/.mail/spam.mail")
      (error "File ~/.mail/spam.mail already exists; delete it first."))
  (if (not mailbox)
      (setq mailbox "inbox.Spam"))
  (get-nonstandard-mail (csail-imap-spool-file mailbox) "~/.mail/spam.mail")
  (vm-sort-messages "subject" nil)
  (vm-sort-messages "spam-score" nil))

(defun get-spam-mail-mde ()
  "Move my personalized spam (junk) mail from IMAP to local file spam.mail."
  (interactive)
  (get-spam-mail "inbox.Spam-mde-rules")
  (vm-sort-messages "date" nil)
  (vm-sort-messages "subject" nil))

(defun get-google-mail ()
  "Move my Google mail from IMAP to local file gmail.mail."
  (interactive)
  (get-nonstandard-mail (gmail-imap-spool-file "inbox") "~/.mail/gmail.mail"))


;; There are better ways to do this.
(defun vm-output-as-missed-spam ()
  "Output this message as spam."
  (interactive)
  (let ((check-empty-subject-line-disable t)
	(check-from-address-disable t))
    (vm-resend-message-to-maildrop "MissedSpam")))

;; There are better ways to do this.
(defun vm-output-as-not-spam ()
  "Output this message as non-spam."
  (interactive)
  (let ((check-empty-subject-line-disable t)
	(check-from-address-disable t))
    (vm-resend-message-to-maildrop "NotSpam")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Summary
;;;

;;
;; VM
;;

(setq vm-mouse-track-summary nil)	; don't highlight summary lines under mouse

(setq vm-summary-uninteresting-senders "^mernst@")

;; default vm-summary-format: "%n %*%a %-17.17F %-3.3m %2d %4l/%-5c %I\"%s\"\n"

;; The summary format need not be one line per message, but it must end
;; with a newline.
(defvar vm-summary-format-rmail-style
  "%*%n%Ud %2d-%-3.3m  %-25.25F  %s\n"
  "Value for `vm-summary-format' that makes it look like RMAIL's summary format.
This adds a mark field at the beginning, which is missing in RMAIL.")
(setq vm-summary-format vm-summary-format-rmail-style)

(defun vm-summary-function-d (message)
                                        ; checkdoc-params: (message)
  "First character of `vm-su-attribute-indicators': deleted, new, unread, or read."
  (cond ((vm-deleted-flag message) "D")
	((vm-new-flag message) "N")
	((vm-unread-flag message) "U")
	(t " ")))


;;; I don't understand why this isn't fixing my problem (unless I evaluate
;;; it explicitly).  But I've reported this to the debian maintainers on
;;; July 24, 2005, so maybe I won't need this fix for long.
;; Bug in VM version 7.19 (dies on ~/bin/bugs/bug-vm-q-encode.mail).
;; vm-mime-qp-encode-region can reduce the size of the region, making "end"
;; an illegal index.  Thus, I changed an instance of "end" to "(min end
;; (point-max))".
(defun vm-mime-Q-encode-region (start end)
  (let ((buffer-read-only nil)
	(val))
    (setq val (vm-mime-qp-encode-region start end t))
    (subst-char-in-region start (min end (point-max)) (string-to-char " ") ?_ t)
    val ))
;; Also eval-after-load in case it gets redefined by vm-mime.
(with-eval-after-load "vm-mime"
  (defun vm-mime-Q-encode-region (start end)
    (let ((buffer-read-only nil)
	  (val))
      (setq val (vm-mime-qp-encode-region start end t))
      (subst-char-in-region start (min end (point-max)) (string-to-char " ") ?_ t)
      val )))


(defun vm-get-in-message-buffer ()
  (cond
   ((eq major-mode 'vm-summary-mode)
    (vm-follow-summary-cursor)
    (vm-select-folder-buffer)
    (vm-check-for-killed-summary)
    (vm-check-for-killed-presentation)
    (vm-error-if-folder-empty)
    (set-buffer vm-presentation-buffer))
   ((eq major-mode 'vm-presentation-mode)
    ;; nothing to do
    )
   ((eq major-mode 'vm-mode)
    ;; nothing to do?
    )
   (t
    (error "Not in a VM mode.")))
  ;; redisplay
  (sit-for 0))


(defun process-acm-toc ()
  "Given a buffer containing an ACM TOC Service announcement, create a HTML
page containing all the abstracts for all the papers in the issue.
It does this by calling script acm-dl-abstracts ."
  (interactive)

  (error "Extract HTML body; run acm-dl-abstracts acm.html; see file acm-abstracts-????.html")

  ;; This block isn't working.  Make sure the raw HTML is shown in the
  ;; buffer before invoking this Lisp function.
  (vm-get-in-message-buffer)
  (while vm-mime-decoded
    (vm-decode-mime-message)
    (vm-get-in-message-buffer))
  (sit-for 0)

  (re-search-forward "http://portal[0-9]*.acm.org:80/citation.cfm\\?id=\\(3D\\)?\\([0-9]*\\)&")
  (let* ((id (match-string 2))
	 (url (concat "http://portal.acm.org/toc.cfm?id=" id))
	 (outfile (concat "$HOME/synchronized/acm-toc-" id ".html")))
    (start-process-shell-command "acm-dl-abstracts-process" "acm-dl-abstracts-process-buffer" "acm-dl-abstracts" url ">" outfile)
    (message "Be patient.  Eventually, HTML output will appear in %s" outfile)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Replying
;;;


;; (defadvice vm-mail-internal (around no-duplicate-mail-buffers activate)
;;   (let* ((buffer-name (ad-get-arg 0))
;; 	 (buffer (and buffer-name (get-buffer buffer-name)))
;; 	 (erase (and (bufferp buffer)
;; 		     (progn
;; 		       (pop-to-buffer buffer)
;; 		       (y-or-n-p "Unsent message being composed; erase it? "))))) ;
;;     (if (and (bufferp buffer)
;; 	     erase)
;; 	;; Two possibilities:
;; 	;; 1. Rename.
;; 	;; (with-current-buffer buffer
;; 	;;   (rename-buffer (generate-new-buffer-name (concat " deferred: " buffer-name))))
;;         ;; 2. Kill
;; 	(let ((disable-warn-when-kill-mail t))
;; 	  (kill-buffer buffer))
;;       )
;;     (if (and (bufferp buffer)
;; 	     (not erase))
;;  	(progn
;;  	  (pop-to-buffer buffer)
;;  	  (message "Reusing existing mail buffer"))
;;        ad-do-it)))


;;
;; From address
;;

;; Other similar implementations are "Personality Crisis for VM" and
;; "vm-multdom.el".  So, I should get rid of mine!  Or, just have separate
;; mailboxes and I don't need this...

(defvar nonstandard-from-address-regexp-alist
  '(("\\bmernst@alum\\.mit\\.edu\\b" . "Michael Ernst <mernst@alum.mit.edu>")
    ("\\bm\\.?ernst@mpi-sws\\.mpg\\.de\\b" . "Michael Ernst <mernst@mpi-sws.mpg.de>")
    ;; ("\\bmernst@cf-web\\.org\\b" . "Michael Ernst <mernst@cf-web.org>")
    ;; ("\\bsn-\\(oversight\\|green\\|red\\)@geyer\\.lcs\\.mit\\.edu\\b" . "Michael Ernst <sn-oversight@geyer.lcs.mit.edu>")
    ;; ("@adtran\\.com\\b" . "Michael Ernst <sn-oversight@geyer.lcs.mit.edu>")
    ;; ("\\bcf-web@cf-web\\.org\\b" . "CF-WEB <cf-web@cf-web.org>")
    )
  "List of (REGEXP . FROM-ADDRESS) pairs.  When replying to mail sent to an
address matching the REGEXP, the reply uses the corresponding address in
its From: line.")
(if (member system-site '(csail))
    (setq nonstandard-from-address-regexp-alist
	  (append
	   '(("\\bmernst@\\(csail\\.\\)?mit\\.edu\\b" . "Michael Ernst <mernst@csail.mit.edu>")
	     ("\\bmernst@lcs\\.mit\\.edu\\b" . "Michael Ernst <mernst@csail.mit.edu>")
	     ;; ("\\bmernst@\\(cs\\|u\\)\\.washington\\.edu\\b" . "Michael Ernst <mernst@cs.washington.edu>")
	     )
	   nonstandard-from-address-regexp-alist)))


(defun get-nonstandard-from-address (to-field cc-field)
  (let (result)
    (if (or to-field cc-field)
	(let ((from-addrs nonstandard-from-address-regexp-alist))
	  (while from-addrs
	    (if (or (and to-field
			 (string-match (car (cl-first from-addrs)) to-field))
		    (and cc-field
			 (string-match (car (cl-first from-addrs)) cc-field)))
		(setq result (cdr (cl-first from-addrs))
		      from-addrs nil)
	      (setq from-addrs (cdr from-addrs))))))
    result))

(defadvice vm-do-reply (around set-nonstandard-from-address activate)
  (let* ((mp (car (vm-select-marked-or-prefixed-messages 1))) ; not strictly right
	 (to-field (vm-get-header-contents mp "To:"))
	 (cc-field (vm-get-header-contents mp "Cc:"))
	 (nonstandard-from-address
	  (get-nonstandard-from-address to-field cc-field))
	 (mail-default-headers
	  (if (not nonstandard-from-address)
	      mail-default-headers
	    (concat "From: " nonstandard-from-address "\n" mail-default-headers))))
    ad-do-it))

(defun address-sans-human-name (addr)
  (if (string-match "<\\([^< ]+@[^< ]+\\)>$" addr)
      (match-string 1 addr)
    addr))
;; (address-sans-human-name "foo@bar")
;; (address-sans-human-name "John Doe <foo@bar>")

(defadvice vm-do-reply (before warn-if-reply-to-is-set activate)
  (if (not (ad-get-arg 0))
      ;; "to-all" flag is not set
      (let* ((mp (car (vm-select-marked-or-prefixed-messages 1))) ; not strictly right
	     (from-field (vm-get-header-contents mp "From:"))
	     (reply-to-field (vm-get-header-contents mp "Reply-To:")))
        (if (and reply-to-field
	         ;; reply-to exists
	         (not (or
		       ;; OK if reply-to but equals from-field
		       (equal from-field reply-to-field)
		       ;; OK if reply-to is a substring of from-field
		       ;;   From: Adam Kiezun <akiezun@csail.mit.edu>
		       ;;   Reply-To:  akiezun@csail.mit.edu
		       (string-match (regexp-quote reply-to-field) from-field)
		       ;; OK if reply-to is a pseudo-substring of from-field
		       ;;   From: Adam Kiezun <akiezun@csail.mit.edu>
		       ;;   Reply-To:  akiezun@mit.edu
		       (let ((reply-parts (split-string reply-to-field "@")))
		         (string-match (concat (regexp-quote (cl-first reply-parts))
					       "@[^ ]*"
					       (regexp-quote (cl-second reply-parts)))
				       from-field))
		       ))
	         (not (y-or-n-p "Reply to other than from/sender? ")))
	    (error "Avoid redirected reply.")))))



;;
;; Signature expansion
;;

;; TODO: Set this up for VM.

(defadvice sendmail-user-agent-compose (after use-nonst-sig-exp activate)
  (use-nonstandard-sig-expansion))

;; Perhaps this should be an assoc list which also specifies the
;; corresponding From: line.
;; These are used when replying.
;; The regexps are matched against the address the original email was sent to.
(defvar nonstandard-sig-expansion-regexp-alist
  '(
    ;; The sn-* entries come first because earlier entries in this list
    ;; override later ones.
    ;; ("\\bmernst@alum\\.mit\\.edu\\b"
    ;;  . "                                    -Michael Ernst
    ;;                                      mernst@alum.mit.edu")
    ;; ("\\bmernst@\\(csail\\.\\|lcs\\.\\)?\\.edu\\b"
    ;;  . "                                    -Michael Ernst
    ;;                                      mernst@csail.mit.edu")
    ;; ("\\bmernst@mit\\.edu\\b"
    ;;   . "                                    -Michael Ernst
    ;;                                      mernst@mit.edu")
    ;; ("\\bsn-\\(oversight\\|green\\|red\\)@geyer\\.lcs\\.mit\\.edu\\b"
    ;;  . "                                    -Michael Ernst
    ;;                                      sn-oversight@geyer.lcs.mit.edu")
    ;; ("\\b\\(mernst\\|cf-web\\)@cf-web\\.org\\b"
    ;;  . "                                    -Michael Ernst
    ;;                                      mernst@cf-web.org")
    ))

(defvar nonstandard-sig-expansion nil
  "Non-nil if the message about to be composed should use a special signature.")

(defun use-nonstandard-sig-expansion ()
  "Examine the value of `nonstandard-sig-expansion' and act if it is set."
  ;; Do NOT use local-abbrev-table, because sendmail-pre-abbrev-expand-hook
  ;; bashes it.
  (if (not (boundp 'mail-mode-abbrev-table))
      (setq mail-mode-abbrev-table (make-abbrev-table)))
  (define-abbrev mail-mode-abbrev-table "mesx" nonstandard-sig-expansion)
  (setq nonstandard-sig-expansion nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yanking
;;;

;; For general mail writing customizations, see sendmail-mde.el

(autoload 'regexp-remove-alternative "startup-functions-mde")

;; These are in error, as they should make sure that there isn't a SPECIFIC
;; Subject: or To: line that's being ignored.

;; A problem with my settings:  any leading lines that are matched by
;; function vm-match-header get stripped by vm-reorder-message-headers
;; (which is called by vm-mail-yank-default).  Example:  A message whose
;; first line is one of these:
;;   2-2:30 in the Atrium (well, I have class at 2:30, so until 2:25).
;;   Mike:  I've changed Michael Bayne from 'hold' to 'accept.'

(with-eval-after-load "sendmail" ; sendmail.el defines mail-yank-ignored-headers
  ;; Include Subject: and To: fields when yanking.
  (setq mail-yank-ignored-headers (regexp-remove-alternative "^subject:" mail-yank-ignored-headers))
  (setq mail-yank-ignored-headers (regexp-remove-alternative "^to:" mail-yank-ignored-headers))
  (setq mail-yank-ignored-headers
	(concat mail-yank-ignored-headers
		"\\|^Reply-to: "
		"\\|^To: mernst-fair@ai.mit.edu$"
		"\\|^Subject: FAIR WWW page comment$"
		"\\|^Sender: www@ai.mit.edu$"
		"\\|^Subject: FAIR WWW page comment (unparseable name)$"
		"\\|^From: Unparseable name <anon@anon.anon>$"
		"\\|^organization:"
		"\\|^references:")))

;; Order is significant
(setq vm-included-text-headers
      '("From:"
	"To:"
	"Cc:"
	"Subject:"
	"Date:"))

(setq mail-yank-prefix "> ")
(setq vm-included-text-prefix "> ")
(setq vm-included-text-attribution-format nil)


;;; Instead of the following advice, I hacked my version of VM and
;;; submitted a patch.  But it was never incorporated, so reinstate.
;;; This code is up-to-date as of VM 8.0.12.
(setq vm-yank-message-always-prompt-p nil)
;; I have to do all this because if advice supplies an "interactive"
;; specification, it must supply the whole thing.  I wish some of this had
;; been abstracted out into a function.
(defadvice vm-yank-message (before dont-prompt-by-default activate)
  "Don't prompt for message number unless a prefix argument was given."
  (interactive
   (list
    ;; What we really want for the first argument is a message struct,
    ;; but if called interactively, we let the user type in a message
    ;; number instead.
    (let (mp default
             (result 0)
             prompt
             (last-command last-command)
             (this-command this-command))
      (save-excursion
	(vm-select-folder-buffer)
	(setq default (and vm-message-pointer
			   (vm-number-of (car vm-message-pointer)))
	      prompt (if default
			 (format "Yank message number: (default %s) "
				 default)
		       "Yank message number: "))
	;; ADDED BY MDE ...
	;; Don't prompt if either no prefix arg or prefix arg isn't '(4).
	(cond ((not current-prefix-arg)
	       (setq result (string-to-number default)))
	      ((not (equal current-prefix-arg '(4)))
	       (setq result (prefix-numeric-value current-prefix-arg))))
	;; ... END OF ADDED BY MDE
	(while (zerop result)
	  (setq result (read-string prompt))
	  (and (string= result "") default (setq result default))
	  (setq result (string-to-number result)))
	(if (null (setq mp (nthcdr (1- result) vm-message-list)))
	    (error "No such message.")))
      (car mp)))))
;; To disable:
;;  (ad-disable-advice 'vm-yank-message 'before 'dont-prompt-by-default)
;;  (ad-activate 'vm-yank-message)


(defvar vm-mail-yank-default-replacements
  '(("" "`")
    ("" "'")
    ("" "\"")
    ("" "\"")
    ("’" "'")
    ("“" "\"")
    ("”" "\"")
    ("–" "-")
    (" " " ")))

(defadvice vm-mail-yank-default (before remove-text-properties activate)
  (let ((beg (min (point) (mark)))
	(end (max (point) (mark))))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(set-text-properties beg end nil (current-buffer))
	(dolist (from-and-to vm-mail-yank-default-replacements)
	  (goto-char beg)
	  (replace-regexp-noninteractive (cl-first from-and-to) (cl-second from-and-to)))
	))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forwarding
;;;


(setq vm-forwarding-digest-type nil)
(setq vm-forwarding-subject-format "Fwd: %s")

(setq vm-reply-subject-prefix "Re: ")
(setq vm-reply-ignored-addresses '("\\(^\\|[ \<]\\)mernst@\\(cs.washington.edu\\|csail.mit.edu\\|alum.mit.edu\\|mpi-sws.mpg.de\\)"))

;; "re: " and relatives
(defvar re-regexp "\\[?\\(re\\|fwd?\\)\\(([0-9]+)\\|\\[[0-9]+\\]\\|\\^[0-9]+\\)?: *")


;; Resetting this variable does not have an immediate effect, becaues the
;; sortable-subject field is cached in the local mbox file.  A hack is to
;; temporarily comment out the first "or" clause in
;; `vm-so-sortable-subject'; I don't know a better way to reset the cache
;; for all messages in a folder.
;; Default: "^\\(re: *\\)+".
;; The "+" at the end iterates as many times as needed.
(setq vm-subject-ignored-prefix
      (concat
       "\\`\\("
       re-regexp
       "\\|"
       "\\[[^\]]*\\] "
       ;; "\\[6\\.170 TAs\\] *"
       "\\)+"))


;; ;; I'm not sure whether this is desirable, but try it and see how it works.
;; (defadvice vm-forward-message (around use-mime-maybe activate)
;;   "If message is MIME, forward it that way."
;;   (vm-follow-summary-cursor)
;;   (vm-select-folder-buffer)
;;   (vm-check-for-killed-summary)
;;   (vm-error-if-folder-empty)
;;   (let* ((mp (car (vm-select-marked-or-prefixed-messages 1)))
;; 	 (content-type (vm-get-header-contents mp "Content-Type:"))
;; 	 (vm-forwarding-digest-type
;; 	     ;; If multipart/alternate, then the user (or this code!)
;; 	     ;; should perhaps manually cut and paste from the presentation
;; 	     ;; buffer.  (Don't try to edit the pasted part with all the
;; 	     ;; MIME formatting such as equal signs, which is a pain and is
;; 	     ;; error-prone).
;; 	     (if (and content-type
;; 		      (string-match "^multipart/\\(mixed\\|alternative\\)"
;; 				    content-type))
;; 		 "mime"
;; 	       vm-forwarding-digest-type)))
;;     ad-do-it
;;     (make-local-variable 'vm-send-using-mime)
;;     (setq vm-send-using-mime (equal vm-forwarding-digest-type "mime"))))


;; I should instead just put it in the IMAP folder
;; (defun vm-resend-message-to-maildrop (&optional maildrop)
;;   (require 'sendmail)			; for variable mail-send-hook
;;   (if (not maildrop)
;;       (setq maildrop (completing-read
;; 		      "Select a maildrop: "
;; 		      '(("hourly") ("daily") ("weekly") ("monthly") ("bulk")))))
;;   (let (
;; 	;; This doesn't work if one of the hooks itself calls
;; 	;; ispell-message, or for other invocations of resend-message.
;; 	(mail-send-hook (remove 'ispell-message mail-send-hook))
;; 	(vm-mail-send-hook (remove 'vm-mail-check-for-empty-subject vm-mail-send-hook))
;; 	(ispell-message-suppress t)
;; 	(mail-self-blind nil)	; no bcc
;; 	)
;;     (vm-resend-message)
;;     (insert user-mail-address)	; cursor is in "Resent-To:" field
;;     (insert "\nResent-Comment: maildrop-" maildrop)
;;     (if (mail-fetch-field "FCC")
;; 	(progn
;; 	  (mail-position-on-field "FCC")
;; 	  (beginning-of-line)
;; 	  (kill-line)))
;;     (vm-mail-send-and-exit)		; bound to C-c C-c
;;     (vm-delete-message 1)
;;     (message (concat "Resent to me with \"Resent-Comment: maildrop-" maildrop "\"."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

(provide 'rmail-mde)

;;; rmail-mde.el ends here
