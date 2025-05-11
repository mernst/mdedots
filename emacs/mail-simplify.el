;;; -*- lexical-binding: t -*-

;;; mail-simplify.el -- remove headers and uninteresting text from mail/news

;;; Commentary:

;; Seems to only work for Rmail and VM, so maybe I'm not using this any longer.

;;; Code:
(require 'mail-utils-mde)
(autoload 'second "cl-lib")
(eval-when-compile
  (require 'cl-lib)				; for second
  (require 'mail-utils)
  (require 'mail-utils-mde)
)


;;;
;;; Ignoring information
;;;



;;; Ignored headers

(defvar rmail-ignored-headers-original rmail-ignored-headers
  "Saved version of `rmail-ignored-headers'.")


;; (This didn't work as a macro.)
;; Similar (not identical) to mde-gnus-add-ignored-headers.
(defun mde-rmail-add-ignored-headers (old-ignored-headers list-of-headers)
  "Add a new alternative to a regular expression.
OLD-IGNORED-HEADERS is a variable.  LIST-OF-HEADERS is a list of regexps
like \"Organization:\"; each will be preceded by ^."
  ;; Can't use regexp-opt, as elements may contain regexps
  (let ((addendum (concat "^\\("
			  (mapconcat (function identity) list-of-headers "\\|")
			  "\\)")))
    (if old-ignored-headers
	(concat old-ignored-headers "\\|" addendum)
      addendum)))

;; The headers are cleared when the message is added to the RMAIL file, but
;; not thereafter unless the user does something special.  So just adding
;; to this variable won't change the RMAIL file; I have to do, say,
;; rmail-toggle-header twice.  (I think rmail-show-message, called by
;; rmail-next-deleted-message and the like, only calls
;; rmail-reformat-message for new messages.)
(setq rmail-ignored-headers
      (mde-rmail-add-ignored-headers
       rmail-ignored-headers
       '("Accept-Language:"
	 "acceptlanguage:"
	 "Address: 545 Technology Square;  Room 6..;  Cambridge, MA 02139"
	 "Address: Microsoft Research; 1 Microsoft Way; Redmond, WA 98052"
	 "Address: Dept. of CSE, FR-35; Univ. of Washington; Seattle, WA 98195"
	 "Address: Dept. of CSE; Univ. of Washington; Box 352350; Seattle, WA 98195-2350"
	 "Approved:"
	 "apparently-to:" "approved-by:" "Autoforwarded: FALSE"
	 "Comments: Hyperbole mail buttons accepted"
	 "Comments: To: theory-a@vm1.nodak.edu"
	 "content-class:"
	 "Content-Disposition: inline"
	 "content-identifier:"
	 "Content-Language: en"
	 "content-length:" "Content-MD5:"
	 "content-transfer-encoding:" "content-type:"
	 "conversion:"
	 "Date-warning: Date header was inserted by Post-Office.UH.EDU"
	 "day-phone:"
	 "Delivered-By-The-Graces-Of:" "disclaimer:" "Disorganization:"
	 "Delivery-Date:"
	 "distribution: world"
	 "DKIM-Signature:"
	 "DomainKey-Signature:"
	 ;; "Encoding: \\(6\\|7\\|49\\|103\\|179\\|772\\|816\\|1067\\) text$"
	 "Encoding: [0-9]+ text$"
	 "Errors-to:"
	 "fax:"
	 "From  0 00"			; lossage on nishin
	 "importance:" "internet:" "irc:"
	 "lines:"
	 "List-\\(Software\\|Subscribe\\|Unsubscribe\\):"
	 "mail-system-version:" "\\(x-\\)?mailer:"
	 "mime-version:"
	 ;; Want to omit this from news, but not from mail.
	 ;; "newsgroups:"
	 "nntp-posting-host:"
	 "old-resent-from:" "organi[zs]ation:" "originating-client:"
	 "Originator-info: login-id=fwilmer; server=mail.oberlin.edu"
	 "path:" "phone:"
	 "posted-date:" "precedence:" "priority:"
	 "repository:"
	 ;; "resent-[a-zA-Z]+:"
	 "Resent-Message-Id:"
	 ;; Should only include any of the following 4 if all are present.
	 "Resent-From: mernst@csail.mit.edu"
	 "Resent-To: mernst@csail.mit.edu"
	 "Resent-Comment: maildrop-"
	 "Resent-Sender: Michael Ernst <mernst@manioc.csail.mit.edu>"
	 "return-path:" "return-receipt-to:"
	 "Sender: Theory-A - TheoryNet World-Wide Events <THEORY-A@VM1.NoDak.EDU>"
	 "^Sensitivity: $"
	 "Sent-On:"
	 "Sent-Via: \\(Michael Ernst <\\)?mernst@theory.lcs.mit.edu>?"
	 "temporary-moderator:"
	 "Thread-Index:" "Thread-Topic:"
	 "User-agent:"
	 "uucp:"
	 "Www: http://www.research.microsoft.com/research/analysts/ellens/"
 	 "Www: http://www.ai.mit.edu/people/ellens/"
	 "x400-.*:"
	 ;; Do I ever want to see any "X" fields?
	 "X-Accept-Language: en"
	 "X-Address:" "X-Attribution:"
	 "X-Authenticated: fwilmer by mail.oberlin.edu"
	 "X-Authentication: none"
	 "X-Authentication-Warning: [a-z]+.\\(\\(csail\\|lcs\\).mit\\|cs.washington\\).edu: [a-z]+ owned process doing -bs"
	 "X-Authentication-Warning: .* set sender to .* using -f"
	 "X-Authentication-Warning: .* didn't use HELO protocol"
	 "X-AWAD-URL:" "X-Charset:" "X-Char-Esc:"
	 "X-Body-Checksum-\\(Sum\\|MD5\\):"
	 "X-Comment:  Main CPSR Announcement List"
	 "X-courtesy-of:"
	 "X-Dispatcher: imput version 980219"
	 "X-Dispatcher: imput version 990905(IM130)"
	 "X-Eff_Membership_Queries_To: membership@eff.org"
	 "X-Eff_General_Info: info@eff.org"
	 "X-EFL-Spamscore:"
	 "X-ELNK-Trace:"
	 "X-EM-Version:" "X-EM-Registration:"
	 "X-Email-Warning: As a cautionary note, there have been recent instances of"
	 ;; 	people forging mail addresses. If you have any reason to believe
	 ;; 	this message is not authentic, please contact the listed sender or
	 ;; 	System Computing Services at (702) 895-4585."
	 "X-Exmh-Isig-CompType: unknown"
	 "X-Exmh-Isig-Folder: inbox"
	 "x-external-networks:" "x-face:" "X-FirstClass:"
	 "X-Gateway" "X-Geek-Code" "x-gnu-emacs-version:"
	 "X-Genie-Qk-From:" "X-Genie-Qk-Id:" "X-Genie-Gateway-Id:"
	 "X-Hashcash:" "X-Payment: hashcash"
	 "X-HoloGate:"
	 "X-Hosted-By:"
	 "X-HPLC-MailScanner"
	 "X-I-was-looking-back-to-see:"
	 "X-Juno-Att: 0" "X-Juno-Line-Breaks:" "X-Juno-RefParts: 0"
	 "X-Licensed-To:"
	 "X-List\\(processor\\|server\\)-version:"
	 "X-loop:"
	 "X-Lotus-Fromdomain:"
	 "X-Mailbot:"
	 "x-mailer-key-[0-9][0-9]:"
	 "X-Mailman-Approved-At:"
	 ;; "X-MimeOLE: Produced By Microsoft MimeOLE"
	 "X-MDRemoteIP:"
	 "X-MDaemon-Deliver-To: mernst@csail.mit.edu"
	 "X-MDaemon-Deliver-To: mernst@lcs.mit.edu"
	 "X-MIME-Autoconverted:"
	 "X-MIT-WebMail-Sender: $" "X-MIT-WebMail-User-Browser:"
	 "X-MimeOLE:"
	 "X-MS-Has-Attach:" "X-MS-TNEF-Correlator:"
	 "X-Msmail" "X-Msxmtid:" "X-Mts:"
	 "X-Newsreader:"
	 "X-Notice: The site \"wrl.epi.com\" is now known as \"entropic.com\""
	 "X-Nsa\\(-Fodder\\)?:" "X-Ns-Transport-Id:"
	 "X-Oblique-Strategy:"
	 "X-Openmail-Hops:" "X-Orgs:" "X-OriginalArrivalTime:" "X-Originating-IP:"
	 "X-Phone:" "X-Pmrqc:" "X-Priority:" "X-Proccessed-By: mail2list"
	 "X-pstn-"
	 "X-Qotd-Incoming:"
	 "X-Received:"
	 "X-Scanned-By: MIMEDefang "
	 "X-Scheme-Mailer:" "X-Shopping-List:"
	 "X-Score-Level: [0-9.]+$"
	 "X-Sieve: CMU Sieve 2.2"
	 "X-SpamBouncer:" "X-SBClass:"
	 "X-Spam-Filtered-CSAIL:"
	 "X-Spam-Rating: mail.mc.net 1.6.1 0/1000/N"
	 "X-Spam-Prev-Content-Transfer-Encoding:"
	 "X-Spam-Prev-Content-Type:"
	 "X-Spam-Report:"
	 "X-Spam-Score:"
	 "X-Spam-Flag:" "X-Spam-Checker-Version:" "X-Spam-Status:" "X-Spam-Level:" ; SpamAssassin
	 "X-Sun-Charset:"
	 "X-Switzerland:"
	 "X-To:         CYSTIC-L%YALEVM.BITNET@uga.cc.uga.edu"
	 "X-UIDL:"
	 "X-url: http://pdos.lcs.mit.edu/~kaashoek/"
	 "X-Uwcse-Spam-Status:" "X-Uwcse-Spam-Checker-Version:" "X-Uwcse-Spam-Flag:" "X-Uwcse-Spam-Report:" "X-Uwcse-Spam-Level:"
	 "X-Virus-Scanned:"
	 "X-VM-v5-Data:"
	 "X-VM-Summary-Format:"
	 "X-VM-Labels:"
	 "X-VM-VHeader:"
	 "X-VM-Last-Modified:"
	 "X-VM-IMAP-Retrieved:"
	 "X-VM-POP-Retrieved:"
	 "X-VM-Bookmark:"
	 "X-VMS-To:" "X-Windows:"
	 "X-Y-Zippy:" "X-Zippy-Says:"
	 )))
(setq vm-unforwarded-header-regexp
      (mde-rmail-add-ignored-headers rmail-ignored-headers '("In-reply-to:")))



;;; I need to convert all this to be usable by VM as well.

;; This is called only when the message is first received or display of the
;; headers is toggled off.
;;; Which of these comments is true?
;; As of 19.22, called with region narrowed to the message, including headers.
;; As of 19.22, despite the documentation, this isn't really called with
;; buffer narrowed to headers.
(defun mde-rmail-message-filter ()
  (rmail-simplify-headers)
  ;; (rmail-unforward)
  ;; (rmail-delete-junk-mail)
  (rmail-trim-uninteresting-mail)
  (rmail-unquote-from-lines)
  ;; (rmail-warn-of-repository-addition)
  ;; Not sure this is a great idea.
  ;; Commented out as of 9/10/99, due to potential dangers with MIME.
  ;; (rmail-splice-lines)
  )

(setq rmail-message-filter 'mde-rmail-message-filter)


(defvar system-name-sans-site
  (and (string-match "\\." (system-name))
       (substring (system-name) (match-end 0)))
  "The name of the current site, such as \"cs.washington.edu\".")

(defun rmail-simplify-headers-sender-from-subtest (sender from)
  "Return non-nil if SENDER and FROM represent the same address."
  (or (equal from sender)
      (string-match (concat "^" (regexp-quote sender) " (") from)
      (string-match (concat " <" (regexp-quote sender) ">$") from)))

(defun rmail-simplify-headers-sender-from-test (sender from)
  "Return non-nil if SENDER and FROM represent the same address.
Also tries to simplify the sender by eliminating its first component,
as in   vass@truecod.cs.washington.edu => vass@cs.washington.edu."
  (or (rmail-simplify-headers-sender-from-subtest sender from)
      (and (string-match "@[^.]+\\." sender)
	   (let ((sender
		  (concat (substring sender 0 (1+ (match-beginning 0)))
			  (substring sender (match-end 0)))))
	     (rmail-simplify-headers-sender-from-subtest sender from)))))

(defun rmail-simplify-headers ()
  "Remove uninteresting/redundant headers from RMAIL messages."
  (save-excursion
   (if (search-forward "\n\n" nil t)
       (let ((case-fold-search t)
	     originator sender from reply-to to cc bcc
	     resent-from resent-to resent-comment resent-date
	     reply-to in-reply-to references
	     x-envelope-to x-mdaemon-deliver-to x-originating-email
	     x-return-path x-sender x-vms-mail-to)
	 ;; `mail-fetch-field' needs to have buffer narrowed to headers,
	 ;; but don't narrow the buffer for the body of this function, because
	 ;; we might call `rmail-add-label' or some other function which edits
	 ;; the RMAIL file and explicitly corrects the current restriction.
	 ;; Another solution is to call rmail-show-message at the end of this
	 ;; function; it recomputes the visible region but is wasteful here.
	 (save-restriction
	   (narrow-to-region (point-min) (point))
	   (setq bcc (mail-fetch-field "bcc")
		 cc (mail-fetch-field "cc")
		 from (mail-fetch-field "from")
		 in-reply-to (mail-fetch-field "in-reply-to")
		 originator (mail-fetch-field "originator")
		 references (mail-fetch-field "references")
		 reply-to (mail-fetch-field "reply-to")
		 resent-comment (mail-fetch-field "resent-comment")
		 resent-from (mail-fetch-field "resent-from")
		 resent-to (mail-fetch-field "resent-to")
		 resent-date (mail-fetch-field "resent-date")
		 sender (mail-fetch-field "sender")
		 to (mail-fetch-field "to")
		 x-envelope-to (mail-fetch-field "x-envelope-to")
		 x-mdaemon-deliver-to (mail-fetch-field "x-mdaemon-deliver-to")
		 x-originating-email (mail-fetch-field "x-originating-email")
		 x-return-path (mail-fetch-field "x-return-path")
		 x-sender (mail-fetch-field "x-sender")
		 x-vms-mail-to (mail-fetch-field "x-vms-mail-to")))
	 ;; Watch out for calls to "equal", as case might differ between
	 ;; (say) x-sender and from.

	 (let ((buffer-read-only nil))
	   (if (and x-sender (string-match " (Unverified)$" x-sender))
	       (setq x-sender (substring x-sender 0 (match-beginning 0))))
	   (if (and resent-from resent-to
		    (equal resent-from "Michael Ernst <mernst@cs.washington.edu>")
		    (or (equal resent-to "mernst@csail.mit.edu")
			(equal resent-to "mernst@lcs.mit.edu")))
	       (progn (mail-remove-field "resent-from")
		      (setq resent-from nil)
		      (mail-remove-field "resent-to")
		      (setq resent-to nil)))
	   (if (and resent-from resent-to
		    (or (string-equal (downcase resent-from) "mernst@csail.mit.edu")
			(string-equal (downcase resent-from) "mernst@lcs.mit.edu"))
		    (or (string-equal (downcase resent-to) "mernst@csail.mit.edu")
			(string-equal (downcase resent-to) "mernst@lcs.mit.edu")))
	       (progn (mail-remove-field "resent-from")
		      (setq resent-from nil)
		      (mail-remove-field "resent-to")
		      (setq resent-to nil)
		      (mail-remove-field "resent-comment")
		      (setq resent-comment nil)
		      (mail-remove-field "resent-date")
		      (setq resent-date nil)))
	   (if (and reply-to from (string-match (regexp-quote reply-to) from))
	       (progn (mail-remove-field "reply-to")
		      (setq reply-to nil)))
	   (if (and to x-envelope-to (equal to x-envelope-to))
	       (progn (mail-remove-field "x-envelope-to")
		      (setq x-envelope-to nil)))
	   (if (and to x-mdaemon-deliver-to (string-match (regexp-quote x-mdaemon-deliver-to) to))
	       (progn (mail-remove-field "x-mdaemon-deliver-to")
		      (setq x-mdaemon-deliver-to nil)))
	   (if (and to x-vms-mail-to
		    (equal x-vms-mail-to (concat "uucp%\"" to "\"")))
	       (progn (mail-remove-field "x-vms-mail-to")
		      (setq x-vms-mail-to nil)))
	   (if (and in-reply-to references (equal in-reply-to references))
	       (progn (mail-remove-field "references")
		      (setq references nil)))
	   (if (and cc (equal cc ""))
	       (progn (mail-remove-field "cc")
		      (setq cc nil)))
	   ;; empty bcc header seems to be inserted by rice-cs, when the
	   ;; bcc is a local rice-cs address
	   (if (and bcc (equal bcc ""))
	       (progn (mail-remove-field "bcc")
		      (setq bcc nil)))
	   (if (and reply-to (equal reply-to "jugglers@u.washington.edu"))
	       (progn (mail-remove-field "reply-to")
		      (setq reply-to nil)))
	   ;; sender, originator, x-sender
	   (if (and originator sender (equal originator sender))
	       (progn (mail-remove-field "originator")
		      (setq originator nil)))
	   (if (and sender to (string-match (regexp-quote sender) to))
	       (progn (mail-remove-field "sender")
		      (setq sender nil)))
	   (if (and sender from
		    (rmail-simplify-headers-sender-from-test sender from))
	       (progn (mail-remove-field "sender")
		      (setq sender nil)))
	   (if (and x-sender from
		    (rmail-simplify-headers-sender-from-test x-sender from))
	       (progn (mail-remove-field "x-sender")
		      (setq x-sender nil)))
	   ;; Maybe fold this into rmail-simplify-headers-sender-from-test
 	   ;; From: <vass@truecod>, X-Sender: vass@truecod.cs.washington.edu
 	   (if (and from x-sender
 		    (string-match
 		     (concat "\\." (regexp-quote system-name-sans-site) "$")
 		     x-sender)
 		    (string-match
 		     (concat "<" (regexp-quote
 				  (substring x-sender 0 (match-beginning 0)))
 			     ">")
 		     from))
 	       (progn (mail-remove-field "x-sender")
 		      (setq x-sender nil)
 		      (string-match "@" from)
 		      (setq from (concat (substring from 0 (match-end 0))
 					 system-name-sans-site
 					 ">"))
 		      (mail-set-field "from" from)))
	   (if (and from reply-to
		    (let ((reply-to-atsign-pos (string-match "@" reply-to)))
		      (and
		       reply-to-atsign-pos
		       (string-match
			(concat "\\b"
				(substring reply-to 0 (1+ reply-to-atsign-pos))
				"\\([-a-zA-Z.]+\\.\\)"
				(substring reply-to (1+ reply-to-atsign-pos)))
			from))))
	       ;; Do from before reply-to, as we need the match data
	       (progn (mail-remove-field "reply-to")
		      (setq reply-to nil)
		      (setq from (concat (substring from 0 (match-beginning 1))
					 (substring from (match-end 1))))
		      (mail-set-field "from" from)))
	   (if (and from x-return-path
		    (string-match (regexp-quote (concat "<" x-return-path ">"))
				  from))
	       (progn (mail-remove-field "x-return-path")
		      (setq x-return-path nil)))
	   (if (and from x-originating-email
		    (and (string-match "^\\[\\(.*\\)\\]$" x-originating-email)
			 (string-match (regexp-quote (concat "<" (substring x-originating-email 1 (- (length x-originating-email) 1)) ">"))
				       from)))
	       (progn (mail-remove-field "x-originating-email")
		      (setq x-originating-email nil)))
	   )))))


(defvar rmail-uninteresting-texts nil
  "List of (start-re start-offset end-re header-re) lists.
END-RE and HEADER-RE are optional.
START-OFFSET tells how far from message body start (end, if negative)
to look for START-RE (actually, START-RE must end before that point);
if nil, entire message is searched (usually a bad idea).")

(setq rmail-uninteresting-texts
      (list
       ;;
       (list
	(regexp-quote "--
Danny Dig's homepage: http://netfiles.uiuc.edu/dig/www

Motto: \"Success is not for the chosen few but for the few who choose\"")
	-150)
       ;; Communigate
       (list
	(regexp-quote "*This message was transferred with a trial version of CommuniGate(tm) Pro*\n")
	-100)
       ;; Toh Ne Win
       (list
	"\"Nonsense!  I have not created a religion.  I am the religion!\"
				 - Lord Leto II, God Emperor of Arakkis"
	-150)
       ;; Rob O'Callahan
       (list
	(concat "-- ?\n\\[?Robert O'Callahan,? "
		"\\(http://www\\.cs\\.cmu\\.edu/~roc,? [0-9]th year CMU CS PhD student\\|<robert@ocallahan\\.org>\\)\n")
	-400
	;; end of "Joshua 5:13-14"
	" [0-9]+:[0-9]+-[0-9]+[\]\)]\n")
       ;; Alex Groce
       (list
	"--\n.*\n--\nAlex David Groce (agroce\\+@cs\\.cmu\\.edu)
Ph.D. Student, Carnegie Mellon University - Computer Science Department
8112 Wean Hall (412)-268-3066
http://www.cs.cmu.edu/~agroce"
       -300)
       ;; Juno
       (list
	"________________________________________________________________
GET INTERNET ACCESS FROM JUNO!
Juno offers FREE or PREMIUM Internet access for less!
Join Juno today!  For your FREE software, visit:
http://dl.www.juno.com/get/tagj."
	-300)
       (list
	"____________________________________________________________________*
You don't need to buy Internet access to use free Internet e-mail\\.
Get completely free e-mail from Juno at http://www\\.juno\\.com\\(/getjuno.html\\)?
\[Oo\]r call Juno at (800) 654-JUNO \\[654-5866\\]"
	-300)
       (list
	(regexp-quote
	 "________________________________________________________________
Get secure free e-mail that you don't need Web access to use
from Juno, the world's second largest online service.
Download your free software at http://www.juno.com/getit.b.html.")
	-300)
       (list
	(regexp-quote
	 "___________________________________________________________________
Get the Internet just the way you want it.
Free software, free e-mail, and free Internet access for a month!
Try Juno Web: http://dl.www.juno.com/dynoget/tagj.")
	-300)
       (list
	(regexp-quote
	 "________________________________________________________________
YOU'RE PAYING TOO MUCH FOR THE INTERNET!
Juno now offers FREE Internet Access!
Try it today - there's no risk!  For your FREE software, visit:
http://dl.www.juno.com/get/tagj.")
	-300)
       ;; Hotmail/MSN
       (list
	"\n_+\n.*\n?http://www.hotmail.com"
	-200)
       (list
	"\n_+\n.*\\(\n.*\\)?http://[a-z]+.msn.\\(click-url\\.\\)?com[^ \n]*\n"
	-300)
;;        ;; Like the above, but no optional newline; not sure why the above
;;        ;; doesn't work.
;;        (list
;; 	"\n_+\n.*http://[a-z]+.msn.com[^ \n]*\n"
;; 	-200)
       (list
	"_________________________________________________________________________
Get Your Private, Free E-mail from MSN Hotmail at http://www\\.hotmail\\.com\\.

Share information about yourself, create your own public profile at[ ]
http://profiles\\.msn\\.com\\."
	-250)
       (list
	"_________________________________________________________________
.*
http://msnmessenger-download\\.com"
	-250)
;;        ;; Hotmail
;;        (list
;; 	"_________________________________________________________________
;; Join the world’s largest e-mail service with MSN Hotmail.
;; http://www.hotmail.com"
;; 	-200)
;;        (list
;; 	"_________________________________________________________________
;; Chat with friends online, try MSN Messenger: http://messenger.msn.com"
;; 	-150)
;;        (list
;; 	"_________________________________________________________________
;; Get your FREE download of MSN Explorer at http://explorer.msn.com/intl.asp."
;; 	-150)
;;        (list
;; 	(regexp-quote "______________________________________________________
;; Get Your Private, Free Email at http://www.hotmail.com")
;; 	-150)
;;        (list
;; 	(regexp-quote "________________________________________________________________________
;; Get Your Private, Free E-mail from MSN Hotmail at http://www.hotmail.com")
;; 	-160)
;;        (list
;; 	"_________________________________________________________________________
;; Get Your Private, Free E-mail from MSN Hotmail at http://www\\.hotmail\\.com\\.
;;
;; Share information about yourself, create your own public profile at[ ]
;; http://profiles\\.msn\\.com\\."
;; 	-250)
;;        (list
;; 	(regexp-quote "_______________________________________________________________
;; Get Free Email and Do More On The Web. Visit http://www.msn.com")
;; 	-150)
;;        ;; (list
;;        ;;  "/intl.asp"
;;        ;;  -20)
       (list
	(regexp-quote "----------------------------------------------------
Sign Up for NetZero Platinum Today
Only $9.95 per month!
http://my.netzero.net/s/signup?r=platinum&refcd=PT97")
	-200)
       (list
	(regexp-quote "Find the best deals on the web at AltaVista Shopping!
http://www.shopping.altavista.com")
	-100)
       ;; MIT Webmail
       (list
	(regexp-quote "----------------------------------------------------------------
This mail sent using MIT WebMail. See http://web.mit.edu/webmail")
	-150)
       ;; Jon Salz
       (list
	(regexp-quote "===========================================================================
Jon Salz <jsalz@mit.edu>  \"Tiger got to hunt, Bird got to fly,
Phone: 617-764-1255        Man got to sit and wonder, \"Why? Why? Why?\"
AIM: JSalzAtMIT            Tiger got to sleep, Bird got to land,
                           Man got to tell himself he understand.\"

                                    - Seventh Book of Bokonon")
	-440)
       ;; Heather Ralph
       (list
	(regexp-quote "                \\=/,         _-===-_-====-_-===-_-==========-_-====-_
                |  @___oo   (                                        )_")
	-900
	(regexp-quote " | \\____(      )___) )___ 	    (_       _       _         _     )
  \\______(_______;;; __;;; 	      ======- -=====- -=======- -====")
	"^From: Heather Ralph")
       (list
	(concat
	 (regexp-quote "        \\|/ ____ \\|/") ".*\n"
	 (regexp-quote "        ~@-/ oO \\-@~") ".*\n"
	 (regexp-quote "        /_( \\__/ )_\\") ".*\n"
	 (regexp-quote "           \\__U_/") ".*\n")
	-300)
       (list
	(regexp-quote "                                      (__)
                                      (oo)
                              /-------(..)
                             /|      || ~
                            * ||++---||
                   w   w      ^^     ^^   w  w
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
	-400)
       (list
	(regexp-quote (concat "                               aa   aa  __   (??????)
                               ||@@_||_(,,)  o " "
                               |       (oo) o
                    w   w     ~\\______/(~~)       w  w
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
	-300)
       (list
	"                   *\\\\\\\\|// *
                   *(O-O) *
 _______________*oOO__(_)__OOo______________* *
|                                         *|"
	-800				; -700 wasn't enough
	"|________________________*Oooo\\._____________*|
                 *\\.oooO   (   ) *
                 *(   )    ) / *
                 * \\\\ (    (_/ *
                 *  \\\\_)")
       ;; Chris Chapman
       (list
	(regexp-quote "--	        :) Chris Chapman (:
_________________________________________________________________________
| President,    - Oh look! big title :) -  | University of Washington    |")
	-900
	(concat "\\("
		(regexp-quote "                              || ||
                             ooO Ooo")
		"\\|"
		(regexp-quote "+------------------------------------------+-----------------------------+")
		"\\)"))
       ;; Swing email lists
       (list
	(regexp-quote "##############################################################################
UW Swing Kids list; info at <http://homes.cs.washington.edu/~paul/sk/lists/>
To stop: mail \"unsubscribe\" to <swingkids-chat-request@cs.washington.edu>")
	-240)
       (list
	(regexp-quote "##########################################################################
# Welcome to the Seattle Lindy Hoppers list.  To unsubscribe, send the   #
# word \"unsubscribe\" to <swing-seattle-chat-request@cs.washington.edu>.  #
# For more info, see <http://homes.cs.washington.edu/~paul/sk/lists/> #
##########################################################################")
	-400)
       ;; Jim Suruda
       (list
	(regexp-quote "			     __    __    __    __
                            /  \\  /  \\  /  \\  /  \\
       ____________________/  __\\/  __\\/  __\\/  __\\__________________")
	-600
	(regexp-quote "                          |/   \\_/   \\_/   \\_/   \\    o \\
                                                  \\_____/--<"))
       ;; Jim Suruda
       (list
	"                        0
                     __/|\\\\
                        | \\\\             Jim .*Suruda
                       / \\\\              jsuruda@cs.washington.edu
                      /   L             scolex@u.washington.edu
_*"
	-600)
       ;; Greg Badros
       (list
	"[[<]eom[]>] means End-Of-Message."
	100
	"mernst using msg, v0.01"
	"^From:.*gjb")
       ;; Dave Grove -- blank lines after signature
       ;; [but this deletes the signature, too, doesn't it?]
       (list
        (regexp-quote "--dave\n\n")
	-15)
       ;; Jikes mailing lists
       (list
	(regexp-quote "To learn more about this mailing list, including disclaimers and how to
end your subscription, see http://www.ibm.com/research/jikes/subscribe .")
	-200)
       ;; Free email
       (list
	(regexp-quote "__________________________________________________________________
Get your free Australian email account at http://www.start.com.au")
	-200)
       ;; Yahoo! Mail
       (list
	"\n[-_]*\n\\(Do You Yahoo\\|DO YOU YAHOO\\)!\\?\n.*\n\\(.*\n\\)?\\(.*\n\\)?\\(.*\n\\)?\\(.*\n\\)?"
	-200)
       (list
	(regexp-quote "__________________________________________________________________
Sent by Yahoo! Mail. Get your free e-mail at http://mail.yahoo.com")
	-140)
       (list
	(regexp-quote
	 "------------------------ Yahoo! Groups Sponsor ---------------------~-->\n")
	100
	"---------------------------------------------------------------------~->\n")
;;        (list
;;         (regexp-quote "__________________________________________________
;; Do You Yahoo!?
;; Get email alerts & NEW webcam video instant messaging with Yahoo! Messenger. http://im.yahoo.com")
;;         -180)
;;        (list
;; 	(regexp-quote "__________________________________________________
;; Do You Yahoo!?
;; Make international calls for as low as $.04/minute with Yahoo! Messenger
;; http://phonecard.yahoo.com/")
;; 	-180)
;;        (list
;; 	(regexp-quote "_________________________________________________________
;; DO YOU YAHOO!?
;; Get your free @yahoo.com address at http://mail.yahoo.com")
;; 	-140)
;;        (list
;; 	(regexp-quote "_________________________________________________________
;; Do You Yahoo!?
;; Free instant messaging and more at http://messenger.yahoo.com")
;; 	-140)
;;        (list
;; 	(regexp-quote "__________________________________________________
;; Do You Yahoo!?
;; Bid and sell for free at http://auctions.yahoo.com")
;; 	-140)
;;        (list
;; 	(regexp-quote "__________________________________________________
;; Do You Yahoo!?
;; Talk to your friends online with Yahoo! Messenger.
;; http://messenger.yahoo.com")
;; 	-150)
;;        (list
;; 	(regexp-quote "__________________________________________________
;; Do You Yahoo!?
;; Talk to your friends online with Yahoo! Messenger.
;; http://im.yahoo.com")
;; 	-150)
;;        (list
;; 	(regexp-quote "__________________________________________________
;; Do You Yahoo!?
;; Send instant messages & get email alerts with Yahoo! Messenger.
;; http://im.yahoo.com/")
;; 	-200)
;;        (list
;; 	(regexp-quote "__________________________________________________
;; Do You Yahoo!?
;; Yahoo! Photos -- now, 100 FREE prints!
;; http://photos.yahoo.com")
;; 	-150)
;;        (list
;; 	"__________________________________________________
;; Do You Yahoo!\\?
;; Get email at your own domain with Yahoo! Mail. *
;; http://personal.mail.yahoo.com/"
;; 	-160)
       ;; Netscape WebMail
       (list
	(regexp-quote "____________________________________________________________________
Get your own FREE, personal Netscape WebMail account today at http://home.netscape.com/webmail")
	-200)
       ;; Nichole <merle@u.washington.edu> of swing-seattle-announce
       (list
	(regexp-quote "\"One day, they will call you Banjo, king of the sea
monkeys....BANJO!\"--Thee Spivies")
	-100)
       (list
	(regexp-quote "\"And one day they will call you Banjo, king of the sea monkeys!....BANJO!!\"
~thee spivies")
	-100)
       ;; Other
       (list
	(regexp-quote "--
This message is best viewed with ISO 8859/1 (latin-1) character encoding.")
	-100)

    ;;; The following are obsolete, but are kept around anyway...

       ;; Red Rock Eater
       (list
	"\n *[-=]+
 *This message was forwarded through the Red Rock Eater News Service (RRE)."
	100
	"\\(an empty message to  rre-help@weber.ucsd.edu\\|or send a message to  rre-request@weber.ucsd.edu  with  Subject: help\\)
 *[-=]+"
	"^To: rre@weber.ucsd.edu")
       ;; A.Word.A.Day
       ;; Perhaps should use (regexp-quote "...........................................................................") as start and the current start as end.
       (list
	(regexp-quote "To subscribe or unsubscribe, please send a message to wsmith@wordsmith.org
with \"Subject:\" line as \"subscribe <Your Real Name>\" or \"unsubscribe\".
Email anu@wordsmith.org if you have any questions, comments or suggestions.")
	nil
	"^Subject: A\\.Word\\.A\\.Day")
       ;; Zoran Budimlic
       (list
	(regexp-quote "             ./'\\_____,/~\\_____   _________         )~\\      _____     _____")
	nil
	(regexp-quote "               `~~~~~~~~~~~              ~~~~~~~~`                     ~~~~~~~")
	"^From: Zoran")
       ;; On This Day
       (list
	(regexp-quote "The following is output from \"On This Day.\"  \"On This Day\" and its")
	600
	(regexp-quote "(C) Copyright 1990 The Software Construction Co., All Rights Reserved."))
       ;; swing-seattle-announce
       (list
	(regexp-quote "##########################################################################
# Welcome to Swing-Seattle-Announce.  To unsubscribe, email the word     #")
	-800
	(regexp-quote "# age limit, start & end time, and all the other relevant details.       #
##########################################################################")
	"^To: .*\\bswing-seattle-announce@cs.washington.edu")
       ))


;;; This seems to err if the message body is empty!
;; This should check whether point is in the summary buffer, and, if the
;; message is deleted, do the right thing to move point to the current
;; message (which is usually the next undeleted message, or is this message
;; if it was the last one).
(defun rmail-trim-uninteresting-mail ()
  "Remove uninteresting parts of messages.
Variable `rmail-uninteresting-texts' (which see) determines uninterestingness."
  (save-excursion
    (let ((header-end (and (progn (goto-char (point-min))
				  (search-forward "\n\n" nil t))
			   (1- (point))))
	  (ignored-items rmail-uninteresting-texts))
      ;; Do nothing (and more ill is to come!) if no body
      (if header-end
      (while ignored-items
	(let* ((this-ignored (car ignored-items))
	       (start-re (car this-ignored))
	       (start-offset (car (cdr this-ignored)))
	       (end-re (car (cdr (cdr this-ignored))))
	       (header-re (car (cdr (cdr (cdr this-ignored))))))
	  (setq ignored-items (cdr ignored-items))
	  (goto-char (point-min))
	  (if (and (or (not header-re)
		       (re-search-forward header-re header-end t))
		   (if (or (not start-offset) (> start-offset 0))
		       (progn
			 (goto-char header-end)
			 (re-search-forward
			  start-re
			  (if start-offset
			      (min (point-max) (+ (point) start-offset))
			    nil)
			  t))
		     (progn
		       (goto-char (point-max))
		       (re-search-backward
			start-re
			(max header-end (+ (point) start-offset))
			t))))
	      (progn
		(let ((start-point (match-beginning 0)))
		  ;; end-point is end of start-re if end-re doesn't exist
		  (if end-re (re-search-forward end-re))
		  (let ((end-point (match-end 0)))
		    ;; Don't look for more ignored texts if one was found.
		    ;; (Is this the right thing?)
		    (setq ignored-items nil)
		    ;; Instead of (rmail-edit-current-message) ...
		    ;;   (rmail-cease-edit) (message "")
		    (let ((buffer-read-only nil))
		      (delete-region start-point end-point)
		      (delete-blank-lines)
		      ;; Without this, RMAIL gets confused, because there
		      ;; is no separator between messages (or some such).
		      (while (< (point) (1+ header-end))
			(insert "\n")))))))))))))


(defun rmail-unquote-from-lines ()
  "Remove \">\" from lines that start with \"From\".
This heuristic isn't always right, but it seems to work well in practice."
  ;; Don't set case-fold-search
  (if (search-forward "\n>From " nil t)
      ;; Avoid calling rmail-cease-edit if no change was made, because
      ;; that displays the message, which calls this again!
      (let ((editing nil))
	(unwind-protect
	    (progn
	      (goto-char (match-beginning 0))
	      (while (search-forward "\n>From " nil t)
		(let ((start (1+ (match-beginning 0))))
		  (if (rmail-falsely-quoted-from-line start)
		      (if editing
			  (progn
			    (goto-char start)
			    (delete-char 1))
			(progn
			  (setq editing t)
			  ;; This ought to keep point at the same place...
			  (rmail-edit-current-message)
			  ;; ... but just in case, start over from the top.
			  (goto-char (point-min))))
		    ;; if no edit, skip past this
		    (goto-char (+ start 6))))))
	  (goto-char (point-min))
	  (if editing
	      (rmail-cease-edit))
	  (message "")))))


;; This test could be more sophisticated.  It may be good enough.
;; It moves point.
(defun rmail-falsely-quoted-from-line (line-start)
  "Return non-nil if LINE-START is at the beginning of a spurious \">From\"."
  (let ((prev-line-empty-p (= ?\n (char-after (- line-start 2)))))
    (and
     (or
      prev-line-empty-p
      ;; prev line doesn't start with >
      (not (progn (forward-line -1) (= ?> (char-after (point))))))
     (or
      ;; next line empty (or non-existent)
      (progn
	;; go to next line
	(goto-char line-start) (forward-line 1)
	(or (= (point) (point-max))
	    (= ?\n (char-after (point)))))
      (and
       ;; next line doesn't start with >
       (not (= ?> (char-after (point))))
       (or (not prev-line-empty-p)
	   (and
	    ;; first word doesn't end with :
	    (re-search-forward "\\s " nil t)
	    (not (= ?: (char-after (- (point) 2)))))))))))


;; The better solution is to set the group sticky bit ("chmod g+s") on the
;; directories (but not the files) of the repository.
(defun rmail-warn-of-repository-addition ()
  (if (and (save-excursion
	     (re-search-forward
	      "^Update of" nil t))
	   (not (save-excursion
		  (re-search-forward
		   "YOU MUST RUN *chgrp" nil t)))
	   (save-excursion
	     (re-search-forward
	      "Directory \\(/g4/projects/invariants/.CVS/.*\\) added to the repository" nil t)))
      (let ((dir (match-string 1)))
	(save-excursion
	  (rmail-edit-current-message)
	  (re-search-forward "^Update of" nil t)
	  (replace-match (concat "*****\nYOU MUST RUN   chgrp invariants " dir "\n*****\n\nUpdate of"))
	  (rmail-cease-edit)))))



;; Perhaps have this run automatically.  (That may be dangerous, as other
;; parts of the message may have changed, too, and this is only heuristic.)

(defun rmail-splice-lines ()
  "Put lines ending with \"=\" back together.
This is a hack; real MIME decoding should be used instead."
  (interactive)
  (save-excursion
    (if (let ((case-fold-search nil))
	  ;; two spliced lines in a row; avoid being fooled.
	  (goto-char (point-min))
	  (re-search-forward "[^-+=]=\n[a-z].*\n.*[^-+=]=\n[a-z]" nil t))
	(let ((header-end (progn (goto-char (point-min))
				 ;; Skip over headers; errs if none found
				 (re-search-forward "\n\n"))))
	  (rmail-edit-current-message)
	  ;; Do this only for the body; others work in body or headers.
	  ;; Do less aggressive splicing?  Maybe letter before and after.
	  (replace-regexp-noninteractive "\\([^-+=]\\)=\n\\([a-z]\\)" "\\1\\2")
	  (goto-char (point-min))
	  (replace-string-noninteractive "=20\n" "\n")
	  (goto-char (point-min))
	  (replace-string-noninteractive "=92" "'")
	  ;; Comment out for debugging
	  (rmail-cease-edit)
	  (message "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clean unix mail file
;;;

(defvar clean-mail-buffer-replacements
  '(("This is a multi-part message in MIME format.\n\n" "")
    ("‘" "`")
    ("’" "'")
    ("“" "\"")
    ("”" "\"")
    ;; Fix some MIME markup
    ("\\([^-+=]\\)=\\(\n\\|20\\)" "\\1")
    ;; The below are probably too aggressive
    ("=3D" "=")
    ("=2E" ".")
    ("=91" "`")
    ("=92" "'")
    ("=93" "\"")
    ("=94" "\"")
    ;;; A more discriminating version of the "=92" processing; is it worth it?
    ;; ("=92\\(d \\|s \\|t \\|ve\\)" "'\\1"))
    ;; ("s=92 " "s' ")
    ))


;; Kind of a hack.
(defun clean-mail-buffer ()
  "Delete unwanted headers from a Unix mail file."
  (interactive)
  (save-excursion
    ;; Remove MIME/alternate parts
    (goto-char (point-min))
    (while (re-search-forward "Content-Type: multipart/alternative; \n?[ \t]*boundary=\"\\(.*\\)\"\n" nil t)
      (let ((boundary (match-string 1))
	    part2-beg part2-end)
	(replace-match "")
	(search-forward boundary)
	(replace-match "")
	(search-forward boundary)
	(beginning-of-line 1)
	(setq part2-beg (point))
	(forward-line 1)
	(search-forward boundary)
	(end-of-line 1)
	(setq part2-end (point))
	(delete-region part2-beg part2-end)))

    ;; Delete uninteresting headers
    ;; It would be better to only keep interesting ones rather than trying
    ;; to delete all uninteresting ones.
    (goto-char (point-min))
    (while (re-search-forward rmail-ignored-headers nil t)
      (delete-region (match-beginning 0)
		     (progn (re-search-forward "^[^ \t]")
			    (backward-char 1)
			    (point))))

    ;; Insert form feeds
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward "\n\\(\n+From .*\n\\)[-A-Za-z]+:" nil t)
	(delete-region (match-beginning 1)
		       (match-end 1))
	(beginning-of-line)
	;; (insert "\f\n")
	(insert "\f")
	))

    (dolist (from-and-to clean-mail-buffer-replacements)
      (let ((from (cl-first from-and-to))
	    (to (cl-second from-and-to)))
	(goto-char (point-min))
	(replace-regexp-noninteractive from to)))

    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

(provide 'mail-simplify)

;;; mail-simplify.el ends here
