;;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'mew)          ;; for macro (!) mew-summary-prepare-draft
  (require 'mew-summary3) ;; for macro (!) mew-summary-prepare-draft
  (require 'browse-url-once)
  )

(autoload 'browse-url-once "browse-url-once")
(autoload 'browse-url-if-matched "browse-url-once")
(autoload 'browse-url-once-if-matched "browse-url-once")
(autoload 'browse-url-once-via-text-properties "browse-url-once")

;; Mew data is in ~/Mail

;; An alternative to Mew is Wanderlust: https://github.com/wanderlust/wanderlust
;; It is more actively maintained than Mew is.
;; So, I should switch to that at some point.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To do
;;;

;; Automatically "s update" after going to a folder.
;; ("s all" is so slow as to be annoying, for big folders.)
;; Approach: advise mew-summary-goto-folder ?  (That's the g key.)

;; "s update" should move point only if there are new messages. If there
;; are no new messages, it should not move point. This is irritating, for
;; example, when I do "M-x mew" (as opposed to mew-summary-goto-folder),
;; and the setting of mew-auto-get causes "s update" to happen.

;; Set default to "Summary & Message mode" rather than "Summary mode only".

;; make "d" output to Gmail "all".
;; make D really delete; currently 'mew-summary-clean-trash


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes
;;;

;; To go to another folder: g
;; To get new mail: s RET
;; To sync a local folder with the server: s all RET
;; To update info about folders of current world: C-u Z

;; To process deletions, refiling, etc., must type "x".

;; Threaded view: t t

;; To retrieve all truncated mail marked with a "T":
;;  m a m I
;; which means: mark all, and retrieve marked.

;; Ordinarily, the text version is shown for a multipart/alternative message.
;; To see the HTML version instead:  :
;; To restore:  .

;; To wrap/fill long lines:  _

;; To follow a link in an external browser, middle-click it or type "g"
;; while the cursor on it.

;; To save message body to a file: y n

;; To customize filing, edit
;;   ~/Mail/.mew-refile-msgid-alist
;; (I should link that among my various machines.)


;;; Mail editing and sending

;; Bindings:
;;  a is answer (instead of using r)
;;  w is write (instead of using m)

;; To edit a message (say, one in +draft): E

;; Attachments:
;; After C-c C-a to start attaching files:
;;   "c" copies (attaches) a file.
;;   C-f and C-b navigate among the part/directories.
;;   Only when you are on the top directory, C-p will move the cursor up
;;     into the main part (Cover.txt), or d will delete all attachments.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General options
;;;

;; debugging
;; (setq mew-debug t)


(setq mew-demo nil)			; no splash screen
(setq mew-demo-picture nil)		; no cat picture

(setq mew-use-full-window t)


;; Password caching
(setq mew-use-cached-passwd t)
(setq mew-passwd-timer-unit 999)	; default 10 (minutes)
(setq mew-passwd-lifetime 999)		; default 2
;; enable later, maybe
;; (setq mew-use-master-passwd t)


(setq mew-prog-grep-opts '("-i" "-l" "-e"))  ;; default '("-l" "-e")
(setq mew-prog-vgrep-opts '("-i" "-l" "-e")) ;; default '("-l" "-e")

(setq mew-use-8bit t)			; avoid Base64 encoding where possible

(setq Info-additional-directory-list
      (list (substitute-in-file-name "$HOME/emacs/mew/info")))


(add-hook 'kill-emacs-query-functions 'mew-smtp-flush-queue-default-case)
(defun mew-smtp-flush-queue-default-case ()
  (mew-smtp-flush-queue mew-case-default)
  ;; This function only seems to work if this sit-for form is here and non-zero.
  ;; I don't understand that at all!
  (sit-for 0.1)
  ;; Always permit Emacs to exit, when this is used on kill-emacs-query-functions.
  t)


;; after mew-varsx, not mew-vars2
(with-eval-after-load "mew-varsx"
  (setq mew-mime-content-type
        (append '(("application/octet-stream" "\\.hg" mew-b64 mew-prog-octet-stream mew-icon-application/octet-stream)
		  ("application/octet-stream" "\\.bundle" mew-b64 mew-prog-octet-stream mew-icon-application/octet-stream)
		  )
	        mew-mime-content-type)))

;; This is wrong, but I haven't had time yet to debug what is wrong with it.
;; (defvar mew-prog-xdvi "xdvi")
;; (setq mew-mime-content-type
;;       (cons
;;        '("Application/X-Dvi" "\\.dvi" mew-b64 mew-prog-xdvi mew-icon-text)
;;        mew-mime-content-type))



;; xpdf was not installed on my system, and Mew silently failed.
(with-eval-after-load "mew-unix"
  ;; (setcar mew-prog-pdf-ext "papers") ;; was: ("xpdf" ("-geometry" "+0+0") t)
  ;; (setq mew-prog-pdf '("acroread" ("-geometry" "+0+0") t))
  ;; Evince cannot handle -geometry argument, and Mew fails silently if it is
  ;; given (even though evince does not fail silently).
  (setq mew-prog-pdf-ext '("papers" nil t))
  (setq mew-prog-pdf "papers") ;; was "xpdf"
  )
;; Shouldn't this be the default already?
;; (with-eval-after-load "mew-darwin"
;;      (setq mew-prog-pdf-ext '("open" nil t))
;;      (setq mew-prog-pdf "open") ;; was "xpdf"
;;   )




;; Contains passwords
(if (file-exists-p "~/private/mewmde.el")
    (load-file "~/private/mewmde.el"))


(setq mew-file-max-size 1000000)	; default 100000


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configurations (personalities, cases) to handle multiple accounts
;;;

;;; use C to change account when in summary mode


;; "stunnel" works on laptop.
;; Could also try "mewstunnel", but probably not necessary.
;; The default value ("mewstunnel") works on laptop.
;; (if (file-exists-p "/usr/bin/stunnel4")
;;   (setq mew-prog-ssl "/usr/bin/stunnel4"))
;; (setq mew-prog-ssl "/homes/gws/mernst/bin/Linux-i686/stunnel4")


(setq mew-case-default "cse")
;; One of the cases should have the same name as `mew-case-default' and is
;; used at startup.
;; When adding to this list, also put the password at ~/private/mewmde.el
(setq mew-config-alist
      '(
        (personal
         (name "Michael Ernst")
         (user "mernst")
         (mail-domain "alum.mit.edu")
         (proto "%")
         (imap-user "michael.ernst")
         (imap-server "imap.gmail.com")
         (imap-ssl t)
         (imap-ssl-port "993")
         ;; (imap-size 0)
         (imap-size 1000000)
	 ;; (imap-trash-folder "%[Gmail]/Trash")
         (smtp-server "smtp.gmail.com") ;; if not localhost
         (smtp-ssl t)
         (smtp-ssl-port "587")
         (smtp-port "587")
         (smtp-user "michael.ernst@gmail.com")
         (dcc "michael.ernst@gmail.com")
         (ssl-verify-level 0)
         (tls-smtp "smtp")
         (smtp-auth-list ("PLAIN" "LOGIN" "CRAM-MD5"))
         )
        (cse
          (name "Michael Ernst")
          (user "mernst")
          (mail-domain "cs.washington.edu")
          (proto "%")
          (imap-user "mernst@cs.washington.edu")
          (imap-server "imap.gmail.com")
          (imap-ssl t)
          (imap-ssl-port "993")
          (imap-size 0)
          (imap-size 1000000)
	  ;; (imap-trash-folder "%[Gmail]/Trash")
          (smtp-server "smtp.gmail.com") ;; if not localhost
	  ;; Alternately, could try this but then I need a different password
	  ;; and I haven't been able to work it out:
	  ;; (smtp-server "smtp.cs.washington.edu") ;; if not localhost
          (smtp-ssl t)
          (smtp-ssl-port "587")
          (smtp-port "587")
          (smtp-user "mernst@cs.washington.edu")
          (dcc "mernst@cs.washington.edu")
          (ssl-verify-level 0)
          (tls-smtp "smtp")
          (smtp-auth-list ("PLAIN" "LOGIN" "CRAM-MD5"))
          )
        (imdea
         (name "Michael Ernst")
         (user "michael.ernst")
         (mail-domain "imdea.org")
         (proto "%")
         (imap-user "michael.ernst")
         (imap-server "mail.imdea.org")
         (imap-ssl t)
         (imap-ssl-port "993")
         ;; (imap-size 0)
         (imap-size 1000000)
	 ;; (imap-trash-folder "%[Gmail]/Trash")
         (smtp-server "mail.imdea.org") ;; if not localhost
         (smtp-ssl t)
         (smtp-ssl-port "587")
         (smtp-port "587")
         (smtp-user "michael.ernst@imdea.org")
         (dcc "michael.ernst@imdea.org")
         (ssl-verify-level 0)
         (tls-smtp "smtp")
         (smtp-auth-list ("PLAIN" "LOGIN" "CRAM-MD5"))
         )
        ))



(setq mew-visit-inbox-after-setting-case t)


(setq mew-case-guess-when-prepared t)	; defualt t
(setq mew-case-guess-when-composed t)	; defualt nil
(setq mew-case-guess-when-replied t)	; defualt t


;; mew-case-guess-alist and mew-refile-guess-alist should perhaps be
;; generated from a common data structure.

;; Choosing the identity when sending
;; Later values in the list override earlier ones.
(let ((to-body
       '(
	 ;; Personal
	 ("scath@alumni.duke.edu" "personal")
	 ("cath@literacy-source.org" "personal")
	 ("smchowell@charter.net" "personal")
	 ("noellwilson1@gmail.com" "personal")
	 ("the-ernsts@earthlink.net" "personal")
	 ("candpernst@gmail.com" "personal")
	 ("David.Ernst@trs-rentelco.com" "personal")
	 (".*@seattleschools.org" "personal")
	 ("kribs@uta.edu" "personal")
	 ("cmkz@mathed.uta.edu" "personal")
	 (".*@wrightangle.com" "personal")
	 ("rap@cs.ubc.ca" "personal")
	 ;; ("wolf@cs.ubc.ca" "personal") ;; commented out because of Data Programming
	 ("rcalhoun@alum.mit.edu" "personal")
	 (".*@aclu-wa.org" "personal")
	 ;; Consulting
	 ("drcox2@super.org" "personal")
	 (".*@goldfarb-huck.com" "personal")
	 ("pat.butler@ironmountain.com" "personal")

	 ;; Override CSE defaults
	 ("hollys@u.washington.edu" "personal")
	 ("samburns@uw.edu" "personal")
	 ("acowan@uw.edu" "personal")

	 ;; CSE
	 (".*@\\(\\(cs\\|u\\).\\)?\\(washington\\|uw\\).edu" "cse")
	 ("mahmood@notnoop.com" "cse")
	 ("jsr308-discuss@googlegroups.com" "cse")
	 ("tschiller@finlingua.com" "cse")
	 ("pbsfcin@gmail.com" "cse")
	 ("winglam2@illinois.edu" "cse")
	 ("wdietl@uwaterloo.ca" "cse")
	 ("me@pavpanchekha.com" "cse")
	 ("crenickshinde1996@gmail.com" "cse")
	 ("alberto.goffi@usi.ch" "cse")
	 ("a.blasi3@campus.unimib.it" "cse")

	 ;; IMDEA
	 ;; (".*@\\(software\\.\\)?imdea.org" "imdea")
	 ))
      (to-and-from-body
       '(
	 ;; Personal
	 ("scath@alumni.duke.edu" "personal")
         ("michael.ernst@gmail.com" "personal")
	 ;; CSE
	 ;; ... nothing here ...
	 )))
  (setq mew-case-guess-alist
	(list (cons "To:" (append to-and-from-body to-body))
	      (cons "From:" to-and-from-body))))


;; Refiling mail
;; To debug use of these variables, debug functions
;; mew-refile-guess-by-alist and mew-refile-guess-by-alist1.
(let ((to-and-from-body
       '(

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;; Personal
	 ;;;


	 ("kribs@uta.edu" "%chris-kribs")
	 ("kribs@mathed.uta.edu" "%chris-kribs")
	 ("cmkz@mathed.uta.edu" "%chris-kribs")

	 ("candpernst@gmail.com" "%ernsts")
	 ("cpernst@verizon.net" "%ernsts")
	 ("lizzie_mcq@hotmail.com" "%ernsts")
	 ("brenner0492@msn.com" "%ernsts")
	 ("12069737310.19403918337.8rCcRv73W3@txt.voice.google.com" "%ernsts")

	 ("gwilson@llpf.com" "%cat")
	 ("smchowell@charter.net" "%cat")
	 ("smchowell@gmail.com" "%cat")
	 ("cath@literacy-source.org" "%cat")
	 ("nrwilson@olemiss.edu" "%cat")
	 ("noellwilson1@gmail.com" "personal")

	 ("maeveahowell@gmail.com" "%maeve")

	 (".*@seattleschools.org" "%sps")
	 (".*@inbound.schoology.com" "%sps")
	 ("mcd-bailey-einmo@googlegroups.com" "%sps")
	 ("mcd-jarakessi-grutz@googlegroups.com" "%sps")
	 ("mcd-kayda@googlegroups.com" "%sps")
	 ("mcd-einmo-vandermeulen@googlegroups.com" "%sps")
	 ("mcd-vandermeulen-einmo@googlegroups.com" "%sps")
	 ("mcd-grutz-powers@googlegroups.com" "%sps")
	 ("mcd-tonda-brooke@googlegroups.com" "%sps")
	 ("mcd-einmo-shernoff@googlegroups.com" "%sps")
	 ("mcd-pta-3-spanish-pm@googlegroups.com" "%sps")
	 ("info@mcdonaldpta.org" "%sps")
	 ("hims.ptsa.membership@gmail.com" "%sps")
	 ("HamiltonPTSA@live.com" "%sps")
	 ("HamiltonWeeklyNews@gmail.com" "%sps")
	 ("notify@membershiptoolkit.com>" "%sps")
	 ("andrew@thecertains.com" "%sps")

	 ;; Kids' activities get filed as "sps"
	 ("parentsupport@creativecoding4kids.com" "%sps")
	 ("kingcounty@engineeringforkids.net" "%sps")
	 ("@chess4life.com" "%sps")
	 ("nealfreeland@outlook.com" "%sps")
	 ("coopedup4h@googlegroups.com" "%sps")

	 ("rap@cs.ubc.ca" "%rachel-and-steve")
	 ("wolf@cs.ubc.ca" "%rachel-and-steve")
	 ("steven.wolfman@gmail.com" "%rachel-and-steve")
	 ("rachel.pottinger@gmail.com" "%rachel-and-steve")

	 ("sechang2@gmail.com" "%jamey-and-sharon")
	 ("jamey.hicks@gmail.com" "%jamey-and-sharon")
	 ("sharon@alum.mit.edu" "%jamey-and-sharon")

	 ("josephmjoy@hotmail.com" "%joseph-and-chandana")
	 ("josephj@rinworks.com" "%joseph-and-chandana")
	 ("chandans@exmsft.com" "%joseph-and-chandana")
	 ("diyajoy@hotmail.com" "%joseph-and-chandana")

	 ("rcalhoun@alum.mit.edu" "%tep")
	 ("jderoo@eitronix.com" "%tep")
	 ("drool@mit.edu" "%tep")
	 ("friedmann2@gmail.com" "%tep")

	 ("markle@alum.mit.edu" "%mark-levine")

	 ("pcrowley@wustl.edu" "%pat-crowley")

	 ("aerokeune@hotmail.com" "%wallingford")
	 ("jrilling@hotmail.com" "%wallingford")

	 ("hollys@u.washington.edu" "%dance")
	 ("holly32532@gmail.com" "%dance")

	 ("drcox2@super.org" "%consulting")
	 ("gary.s.thornton@gmail.com" "%consulting")
	 ("dburrows@grammatech.com" "%consulting")
	 ("roth@kdg-law.com" "%consulting")
	 ("roth@goldfarb-huck.com" "%consulting")
	 ("pat.butler@ironmountain.com" "%consulting")

	 ("director@childlearning.org" "%clcc")

	 ("auto-confirm@amazon.com" "%commerce")
	 ("order-update@amazon.com" "%commerce")
	 ("orders@amazon.com" "%commerce")
	 ("payments-messages@amazon.com" "%commerce")
	 ("return@amazon.com" "%commerce")
	 ("ship-confirm@amazon.com" "%commerce")
	 (".*@marketplace.amazon.com" "%commerce")
	 ("orders@jet.com" "%commerce")
	 ("Fidelity.Investments.email@fidelity.com" "%commerce")
	 ("Fidelity.Investments.email@workplacedocs.fidelity.com" "%commerce")
	 ("Fidelity.Investments.email@shareholderdocs.fidelity.com" "%commerce")
	 ("Fidelity.Investments@mail.fidelity.com" "%commerce")
	 (".*fiacardservices.com" "%commerce")
	 ("CityOfSeattleSUEBP-DoNotReply@Seattle.Gov" "%commerce")
	 ("donotreplyseattleutilities@seattle.gov" "%commerce")
	 ("online.communications@alerts.comcast.net" "%commerce")
	 ("kris@damhorsttoys.com" "%commerce")
	 ("noreply_transactional@abebooks.com" "%commerce")
	 ("myaccount@pse.com" "%commerce")

	 (".*@aclu-wa.org" "%aclu")

	 (".*@kidscompany.org" "%kidsco")

	 ("info@asiflex.com" "%benefits")
	 ("notification@naviabenefits.com" "%benefits")


	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;; Travel
	 ;;;

	 ("Alaska.IT@alaskaair.com" "%travel")


	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;; Teaching
	 ;;;

	 ("cse140-staff@cs.washington.edu" "%dp")
	 ("cse190b_au15@uw.edu" "%da-seminar")
	 ("cse190p-staff@cs.washington.edu" "%dp")
	 ("cse331.*@\\(\\(u\\|cs\\).\\)?\\(washington\\|uw\\).edu" "%331")
	 ("cse403-\\(ta\\|staff\\)@cs.\\(washington\\|uw\\).edu" "%403")
	 ("cse403a_sp[0-9][0-9]@\\(\\(u\\|cs\\).\\)?\\(washington\\|uw\\).edu" "%403")
	 ("cse403-18sp-discussion@cs.washington.edu" "%403")
	 ("cse490e1_wi17@uw.edu" "%490")
	 ("cse503a_sp17@u.washington.edu" "%503")
	 ("cse599e1_au16@uw.edu" "%599e1")


	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;; Refereeing
	 ;;;

	 ("icse2016-papers-pc@borbala.com" "%icse-2016")
	 ("icse2016-papers-webadmin@borbala.com" "%icse-2016")
	 ("noreply@pldi16.hotcrp.com" "%referee")
	 ("pldi-2016-dpc@googlegroups.com" "%referee")
	 ("secdev16oc@ieee.org" "%secdev-2017")
	 ("secdev17@cse.psu.edu" "%secdev-2017")


	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;; Administration
	 ;;;

	 ("ariba_apps@u.washington.edu" "%admin")

	 ("cardkey@cs.washington.edu" "%space")

	 ("web-redesign@cs.washington.edu" "%bug-webpages")
	 ("support@cs.washington.edu" "%sysadmin")

	 ("admit-committee@cs.washington.edu" "%prospective-grads-2017")

	 ("cse-maintenance@cs.washington.edu" "%maintenance")

	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;; Research
	 ;;;

	 ("Jingyue.Li@dnv.com" "%jingyue-li")

	 ("alberto.goffi@usi.ch" "%nlp")
	 ("toradocu@cs.washington.edu" "%nlp")
	 ("xilin@cs.washington.edu" "%nlp")
	 ("tellina@cs.washington.edu" "%nlp")

	 ("slavadon@gmail.com" "%synoptic")

	 ("bagpipe@cs.washington.edu" "%konstantin-weitz")

	 ("daikon-announce@googlegroups.com" "%daikon")
	 ("daikon-developers@googlegroups.com" "%daikon")
	 ("daikon-discuss@googlegroups.com" "%daikon")

	 ("annotation-tools@googlecode.com" "%checkers")
	 ("annotation-tools-discuss@googlegroups.com" "%checkers")
	 ("checker-framework@googlecode.com" "%checkers")
	 ("checker-framework-discuss@googlegroups.com" "%checkers")
	 ("checker-framework-dev@googlegroups.com" "%checkers")
	 ("checker-framework-gsoc@googlegroups.com" "%grants-google")
	 ("google-summer-of-code-announce+owners@googlegroups.com" "%grants-google")

	 ("randoop-discuss@googlegroups.com" "%randoop")
	 ("randoop-developers@googlegroups.com" "%randoop")

	 ("crystalvc@googlegroups.com" "%speculate-validate")

	 ("corgi@cs.washington.edu" "%verdi")
	 ("verdi@cs.washington.edu" "%verdi")
	 ("verdi-dev@cs.washington.edu" "%verdi")
	 ("neutrons@cs.washington.edu" "%neutrons")
	 ("neutrons-dev@cs.washington.edu" "%neutrons")

	 ("verigames@cs.washington.edu" "%games")
	 ("dilia.rodriguez@us.af.mil" "%games")
	 ("Dilia.Rodriguez@rl.af.mil" "%games")
	 ("Kelly.McLaughlin.ctr@darpa.mil" "%games")
	 ("notifications@basecamp.com" "%games")
	 ("mburns@cs.washington.edu" "%games")
	 ("timothy.pavlik@gmail.com" "%games")
	 ("team-dean@darpa.mil" "%games")
	 ("Drew.Dean@darpa.mil" "%games")
	 ("juliasoft.noreply@gmail.com" "%games")
	 ("sanna.kallio@juliasoft.com" "%games")
	 ("info@verigames.com" "%games")

	 ("sparta@cs.washington.edu" "%apac")
	 ("Julie.Konnor.ctr@darpa.mil" "%apac")
	 (".*@viaforensics.com" "%apac")

	 ("saidi@csl.sri.com" "%muse")
	 ("susan.furman@sri.com" "%muse")
	 ("Tina.Neff.ctr@darpa.mil" "%muse")
	 ("pascali-team@csl.sri.com" "%muse")
	 ("muse-users@logicblox.com" "%muse")
	 ("muse-aas@csl.sri.com" "%muse")

	 ("brass@cs.washington.edu" "%brass")

	 ("jsr-308-eg@jcp.org" "%jsr308")
	 ("type-annotations-dev@openjdk.java.net" "%jsr308")

	 )))
  (setq mew-refile-guess-alist
	(append
	 '(("Subject:"

	    ;; Put debugging entries at the beginning.

	    ("ICSE 2016 Reviewing" "%icse-2016")

	    ("\\bCLCC\\b" "%clcc")

	    ("A54978" "%grants-nsf")
	    ("FA60788" "%grants-nsf")
	    ("\\bARRA\\b" "%grants-nsf")
	    ("\\bCRIme\\b" "%grants-nsf")

	    ("A73715" "%games")
	    ("A73715" "%games")
	    ("BAA 12-17" "%games")
	    ("FA79933" "%games")
	    ("FA8750-11-2-0221" "%games")
	    ("FA8750-12-C-0174" "%games")
	    ("62-4757" "%games")
	    ("CSFV" "%games")

	    ("\\bAPAC\\b" "%apac")
	    ("\\bSPARTA\\b" "%apac")
	    ("FA8750-12-[2C]-0107" "%apac")
	    ("FA95907" "%apac")
	    ("A69949" "%apac")
	    ("623975 Interim" "%apac")

	    ("27-001450" "%muse")
	    ("A95882" "%muse")

	    ("Cse501" "%501")

	    ("Quick Fix Scout" "%speculate-validate")

	    ("Combined BS/MS" "%advising-csem")

	    ("MDE progress report" "%progress-report")
	    )
	   )
	 (list (cons "To:" to-and-from-body)
	       (cons "Cc:" to-and-from-body)
	       (cons "From:" to-and-from-body)
	       (cons "Sender:" to-and-from-body)
	       (cons "Reply-To:" to-and-from-body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mail reading (Message Mode)
;;;

(with-eval-after-load "mew-key"
  (define-key mew-summary-mode-map "\eo"  nil) ;; was 'mew-summary-auto-refile
  )

(setq mew-use-unread-mark t)
;; if nil, messages get filed with the unread mark on them
(setq mew-delete-unread-mark-by-mark t)

(setq mew-summary-mark-direction 'down)
(setq mew-summary-show-direction 'down)

;; Strip mailing list name when sorting by subject.
;; To customize what is stripped, adjust `mew-subject-simplify-replace-alist'.
(setq mew-sort-key-alist
      (cons '("subject" "ml")
	    mew-sort-key-alist))

(defun strip-leading-double-quote (s)
  "Return s, without any leading double quote"
  (if (string-prefix-p "\"" s)
      (substring s 1)
    s))

;; Ignore leading quote marks
(with-eval-after-load "mew-sort"
  (defun mew-sort-string (x y)
    "Like original, but strips leading double quote."
    (let* ((xstring (strip-leading-double-quote (mew-sort-key x)))
	   (ystring (strip-leading-double-quote (mew-sort-key y))))
      (or (string= xstring ystring)
	  (string< xstring ystring)))))


;; Surprisingly, setting mew-reply-regex does not seem to affet sorting nor
;; replies.

(defun mew-subject-simplify-ml-no-github (str)
  "Like `mew-subject-simplify-ml', but don't strip off GitHub notificatons."
  ;; The only change is to add a "/" to the regex.
  (if (string-match "^[[(][^])/]+[])][ \t]*" str)
      (substring str (match-end 0))
    str))
(defadvice mew-subject-simplify-ml (around no-github activate)
  (setq ad-return-value (mew-subject-simplify-ml-no-github (ad-get-arg 0))))
;; Testing:
;; (mew-subject-simplify-ml (mew-subject-simplify-ml "[Brass] [uwplse/brass] 84f91e: revised task 11"))

;; Used in function `mew-subject-simplify', which is used to strip leading
;; text and otherwise change the subject line for sorting and replying.
;; SEE BELOW FOR DEBUGGING.
(setq mew-subject-simplify-replace-alist
      (append (list
	       (list "^ +" nil) ;; Kivanc Muslu's subject lines sometimes start with a spurious space.
	       ;; This won't have any effect because mew-subject-simplify
	       ;; is generally called before mew-subject-simplify-ml which
	       ;; strips off the leading [TC BUGS].
	       ;; (list "^\\[TC BUGS\\] \\(Assigned\\|Closed\\|Commented\\|Created\\|Issue Comment Edited\\|Resolved\\|Updated\\): " nil)
	       (list "^\\(\\[TC BUGS\\] \\)?\\(Assigned\\|Closed\\|Commented\\|Created\\|Deleted\\|Issue Comment Edited\\|Moved\\|Reopened\\|Resolved\\|Updated\\): \\((CSFVDEV\\)" "\\3")

	       ;; This strips off GitHub notification prefixes, but I would rather put all the notifications together so it isn't really what I want.
	       ;; (list "^\\[[^/]*/[^/]*\\] [0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]: " nil)
	       ;; Keep GitHub notifications together
	       ;; The initial optional group is in case they were forwarded via an email address.
	       ;; (This doesn't seem to be working properly, but wait to see whether it works on new messages.)
               ;;	       (list "^\\(\\[[^]]*\\] \\)?\\[\\([^/]*/[^/]*\\)\\] [0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]: " "\\2")

	       ;; Keep Travis notifications together
	       (list "^\\(Broken\\|Errored\\|Failed\\|Fixed\\|Still Failing\\): " nil)

	       ;; There is no need for this; it is done by mew-subject-simplify-ml
	       ;; (list "^\\[[^]/]*\\] " nil) ; no slashes to avoid GitHub notifications
	       (list "^REMINDER: " nil)
	       )
	      mew-subject-simplify-replace-alist))
;; For debugging. Note that mew-subject-simplify2 calls mew-subject-simplify twice!
(if nil
    (progn
      (defadvice mew-subject-simplify (around trace activate)
	"Print arguments and result."
	(let ((result ad-do-it))
	  (message "mew-subject-simplify %s => %s" (ad-get-args 0) ad-return-value)
	  result))
      (defadvice mew-subject-simplify-ml (around trace activate)
	"Print arguments and result."
	(let ((result ad-do-it))
	  (message "mew-subject-simplify-ml %s => %s" (ad-get-args 0) ad-return-value)
	  result))
      (defadvice mew-subject-simplify2 (around trace activate)
	"Print arguments and result."
	(let ((result ad-do-it))
	  (message "mew-subject-simplify2 %s => %s" (ad-get-args 0) ad-return-value)
	  result))
      )
  )

(mew-sinfo-set-disp-msg t)
(setq mew-file-max-size 1000000)        ; max size for MIME analysis; default 10000
(setq mew-use-header-veil nil)          ; show all to & cc, even if there are many of them

;; nil means "not visible"
(setq mew-field-spec
      (append '(
	        ("^Accept-Language:" nil)
	        ("^ARC-\\(Seal\\|Message-Signature\\|Authentication-Results\\):" nil)
	        ("^Importance:" nil)
	        ("^IronPort-PHdr:" nil)
	        ("^Mailing-List: list seajug@yahoogroups.com; contact seajug-owner@yahoogroups.com$" nil)
	        ("^Received-SPF:" nil)
	        ("^Sender: seajug@yahoogroups.com$" nil)
	        ("^Thread-Index:" nil)
	        ("^User-Agent:" nil)
	        ("^X-Mailer:" nil)
	        ;; Is this ("tab/spc characters") added after mew-field-spec is processed?
	        ("^X-Mew: tab/spc characters on Subject: are simplified." nil)
	        ("^acceptlanguage:" nil)
	        )
	      mew-field-spec))

(defun mew-multipart-setup ()
  ;; set either mew-use-text/html or mew-use-text/html-list
  (setq mew-use-text/html t)		; decode HTML for all folders

  ;; This makes Mew prefer the "Text/Html" part of a multipart-alternative
  ;; message. (There are many broken mailers out there...)
  (setq mew-mime-multipart-alternative-list '("Text/Html" "Text/Plain" ".*"))

  ;; Bad "Apple Mail" attachments in multipart/alternative; work
  ;; around that:
  (setq mew-disable-alternative-regex-list '("Apple Mail"))
  )


(setq eww-download-directory "~/tmp")


;; Handle HTML messages
(cond ((and (fboundp 'shr-render-region)
	    ;; \\[shr-render-region] requires Emacs to be compiled with libxml2.
	    (fboundp 'libxml-parse-html-region))
       ;; EWW is installed.  It is distributed in Emacs 24.4, released 10/2014.
       (mew-multipart-setup)
       (setq mew-prog-text/html 'shr-render-region)

       ;; Consider this for the future
       ;; (add-hook 'mew-message-mode-hook
       ;;           (lambda()
       ;;             (setq-local browse-url-browser-function 'eww-browse-url)))
       )

      ((locate-library "mew-w3m")
       ;; (require 'w3m)
       ;; (require 'mew-w3m)
       (mew-multipart-setup)
       ;; Consider this for the future
       ;; (setq mew-use-w3m-minor-mode t)
       ;; (add-hook 'mew-message-hook 'mew-w3m-minor-mode-setter)
       ))


(setq mew-unix-browser browse-url-generic-program)	   ; default "firefox"
(setq mew-prog-msword-ext office-program)	; default "ooffice"
(setq mew-prog-msexcel-ext office-program)	; default "ooffice"
(setq mew-prog-mspowerpoint-ext office-program)	; default "ooffice"


;; ;; to use w3m
;; (setq mew-mime-multipart-alternative-list '("Text/Html" "Text/Plain" "*."))

(setq mew-header-max-depth 100) 	; default 50


;; default value:
;; `(type (5 date) " " (14 from) " " t (30 subj)
;;	  ,mew-summary-form-body-starter (0 body))
;; This puts more space between the components, making them easier to read.
;; Pre-defined symbols: 'type, 'time, 'date, 'year, 'size, 'from, 'subj, 'body.
;; See the functions called 'mew-summary-form-<symbol>'.
(setq mew-summary-form `(type " " (-6 day-month) "  " (24 from) "  " t (0 subj)))

(setq mew-summary-form-from-me-prefix "To: ") ; default "To:"

;; Maximum width of summaries, regardless of window width. Set this to a
;; large number to fill the window -- and more importantly to enable
;; searching for message titles in the summary buffer
;; (Since Mew reserves 3 characters (2 for the thread indication, 1 for some
;; other purpose), this must be at least (+ 3 (window-width)) to fill the
;; window.)
;; This doesn't seem to affect the layout of messages in digests. How do I
;; do that?
(setq mew-window-magic 200)		; default 80
;; Without this change, the mode line in the summary buffer contains "L??"
;; when no other window is displaying a message body (e.g., after  M-x mew
;; when no new messages are retrieved).
(setq line-number-display-limit-width 500) ; lines can easily be 400 chars wide

(defadvice mew-syntax-format (around reset-mew-window-magic activate)
  "Ensure that MIME attachments don't wrap during message composition,
even if `mew-window-magic' has been set to a large value."
  (let ((mew-window-magic (window-width)))
    ad-do-it))


(defun mew-time-int-to-mon-str (i)
  "Return a 3-letter string for the I-th month."
  (car (nth (1- i) mew-time-mon-alist)))
;; (mew-time-int-to-mon-str 3)

(defun mew-summary-form-day-month ()
  "A function to return a date, in the form DD-Mth."
  (let* ((date (mew-summary-form-date))
	 (day (int-to-string (string-to-number (substring date 3))))
	 (month (mew-time-int-to-mon-str (string-to-number (substring date 0 2)))))
    (format "%s-%s" day month)))

(setq mew-summary-form-extract-rule '(nickname name comment)) ; default '(nickname)


;; For mail refiling, either set mew-refile-guess-by-alist or do stuff at Google.



(defun open-urls ()
  "Open relevant URLs in message."

  ;; These URLs are browsed to every time they are encountered.
  (browse-url-if-matched "To read it, sign in to the Kaiser Permanente member website at\n\\(https://www.kp.org/wa\\)\\." 1)

  ;; All the below URLs are browsed to only once per Emacs session

  ;; It's easier to just turn on "auto checkout" for all the books
  (save-excursion
    (if (or (search-forward "Please visit the Seattle Public Library website http://spl.lib.overdrive.com ," nil t)
	    (search-forward "Log in at http://spl.lib.overdrive.com and click on “Account” then “Holds”" nil t))
	(browse-url-once "https://seattle.libraryreserve.com/10/50/en/SignIn.htm?URL=MyAccount%2ehtm%3fPerPage%3d40")))

  (save-excursion
    (if (and (search-backward "\nFrom: \"Cat Howell's blog: schowell\" <noreply@blogger.com>" nil t)
	     (re-search-forward "\n\nURL: \\(.*http://cathowell.blogspot.com/.*\\)" nil t))
	(browse-url-once (match-string 1))))

  (browse-url-once-via-text-properties "You can approve \\(or\\) reject this message or you can approve this message by *\nreplying to this email." 1)
  (browse-url-once-via-text-properties "Click the link below to access the Recommendation form\\.\nClick \\(here\\) to create your password and start your recommendation" 1)
  (browse-url-once-via-text-properties "PLEASE KEEP THIS EMAIL - you will need it to access your recommendation\\.\n\nClick \\(here\\) to access your recommendation" 1)
  (browse-url-once-via-text-properties "Recommendations will not be accepted via this\ne-mail account\\.\nClick \\(here\\) to create your password and start your recommendation" 1)
  (browse-url-once-via-text-properties "We first need to verify the email address that is included in your record at IDA\nis correct. Please visit this \\(URL\\) to verify this is a working email." 1)
  (browse-url-once-via-text-properties "Sign into the Digital \\(Library\\) website to access this title on your Bookshelf." 1)
  ;; Should only be if the email also contains "ACTION REQUIRED" in the subject.
  ;; Can I achieve that by going to point-min within save-excursion?
  (browse-url-once-via-text-properties "Actions: \\(Open\\)\nCash Advance" 1)
  (browse-url-once-via-text-properties "Actions: *\\(Open\\) *&nbsp\nCopyright" 1)
  (browse-url-once-via-text-properties "invites you to participate in the Doodle poll\\(.*\n\\)+ *\\(Participate now\\)" 2)
  (browse-url-once-via-text-properties "has left a new comment on your post \"\\(.\\)" 1)
  (browse-url-once-via-text-properties "requires your approval because[\n ]+\"\\(Payee\\|Traveler\\)[\n ]+must[\n ]+approve\"\n.*\n\n  Actions:   \\(Open\\)" 2)
  (browse-url-once-via-text-properties "You can approve \\(or\\) reject this message or you can approve this message" 1)
  (browse-url-once-via-text-properties "\\*The following item was added to your Stack Exchange \".*\n?.*\" feed.\n *Stack Ov.......\\(.\\)" 1)
  (browse-url-once-via-text-properties "Assigned #\\([0-9]+\\) to @mernst\\." 1)
  (browse-url-once-via-text-properties "Assigned #\\([0-9]+\\) to @mernst\\." 1)
  (browse-url-once-via-text-properties "You can accept \\(or\\) decline this invitation\\. You can also head over to" 1)
  ;; regex does not match "was canceled."
  (browse-url-once-via-text-properties "\\(Build #[0-9]+ \\(failed\\.\\|has errored\\.\\|is still failing\\.\\|was broken\\.\\)\\)" 1)
  (browse-url-once-via-text-properties "Click Here to view the notification details\\.")

  ;; Nothing to do, because inaction indicates approval
  ;; (browse-url-once-if-matched "To review details of 11/2013 please go to:\n\n     \\(https://prp.admin.washington.edu/.*\\)" 1))))

  (browse-url-once-if-matched "auto-browse[ \t\n]+to[: \t\n]+\\(http.[^ \t\n]*\\)" 1)
  (browse-url-once-if-matched "You can approve or reject this \\(?:message\\|request\\) online at:\n\\(http://groups.google.com/group/.*/pend\\(?:msg\\|ing\\)\\?hl=en\\)" 1)
  (browse-url-once-if-matched "At your convenience, visit:\n\n *\\([^ ].*\\)\n *\nto approve or deny the request." 1)
  (browse-url-once-if-matched "To approve or deny this request, please visit \\(https://norfolk.cs.washington.edu/htbin-php/faculty/ugrad/fac_ugrad_research.php\\)\\." 1)
  (browse-url-once-if-matched "request(s)\\s-waiting\\s-for\\s-your\\s-consideration\\s-at:\n\n\t\\(http.*\\)" 1)
  (browse-url-once-if-matched "At your convenience, visit:\n\n    \\(http.*\\)" 1)
  (browse-url-once-if-matched "as soon as possible \nby browsing to\n\n  \\(http.*\\)" 1)
  (browse-url-once-if-matched "ACTION REQUIRED: Your approval of a UW eGC1 for a grant or contract.\n\nPlease click the link below .*\n\n\\(http.*\\)\n" 1)
  (browse-url-once-if-matched "If you are still actively using .* and do not wish to have\nyour free host removed from our DNS servers and database please click the link\nbelow :\n\n\\(http.*\\)" 1)
  (browse-url-once-if-matched "To approve or deny this request, please visit \\(http.*\\) and complete\nthe web form." 1)
  (browse-url-once-if-matched "Log into eFECS at: \\(https://ucs.admin.washington.edu/effortreport/\\)" 1)
  (browse-url-once-if-matched "log in to MyGroupHealth for Members at \\(http://www.ghc.org\\)" 1)
  (browse-url-once-if-matched "which is available from \\(http://courses.cs.washington.edu/courses/cse590[a-z]/\\([0-9][0-9]\\(wi\\|sp\\|su\\|au\\)\\)?\\)\\." 1)
  (browse-url-once-if-matched "\\(http://www.applyweb.com/lor\\?token=[a-z0-9]+\\)" 1)
  (browse-url-once-if-matched "\\(https://applygrad.cs.cmu.edu/apply/rec.php\\)" 1)
  (browse-url-once-if-matched "\\(https://apps.grad.uw.edu/rec/default.aspx\\?rec=[A-Z0-9%]*\\)")
  (browse-url-once-if-matched "\\(https://apply.grad.ucsd.edu/recommendations/[0-9]+\\?token=[A-Za-z0-9_]*\\)")
  (browse-url-once-if-matched "If you are having difficulties with the link above, please copy and paste the\ntext below into your browser. Make sure to include the entire text below.\n\n------ Begin copying here -----------\n\\(https://rec.applyyourself.com/AYRecommendationLogin/Recommendation_Provider_Login_Action.asp\\?token=[A-Za-z0-9%]*\\)\n------ End copying here ------------" 1)
  (browse-url-once-if-matched "\\(https://mice.cs.columbia.edu/recruit/e.php\\?r_id=[A-Za-z]*\\)" 1)
  (browse-url-once-if-matched "\\(https://gradapply.mit.edu/eecs/l/[a-f0-9]*/\\)" 1)
  (browse-url-once-if-matched "please go to\n\\(https://apply.embark.com/orecs/\\) and login with" 1)
  (browse-url-once-if-matched "\\(https://services.adcom.uci.edu/approgress/lor/letterInfo.htm\\?token=[A-F0-9]*\\)" 1)
  (browse-url-once-if-matched "\\(https://gradapp.stl.vt.edu/pages/EditReference.php\\?r=[0-9a-f]*\\)" 1)
  (browse-url-once-if-matched "\\(https://services.cs.utexas.edu/recruit/grad/references/upload.html\\)" 1)
  (browse-url-once-if-matched "\\(http://ndseg.asee.org/reference/[A-Za-z0-9]*\\)" 1)
  (browse-url-once-if-matched "\\(https://gradadmit.berkeley.edu/jazztwo/app/rec/add/[a-f0-9]*\\)" 1)
  (browse-url-once-if-matched "\\(https://gradapply.ucsd.edu/recommender/index.php\\?code=[a-zA-Z0-9%]*\\)" 1)
  (browse-url-once-if-matched "\\(https://apply.grad.rochester.edu/refer/\\?[0-9]*\\)" 1)
  (browse-url-once-if-matched "You may submit your reference online via the Graduate Reference Center, which is\nlocated at \\(http://www.umass.edu/gradschool/application/recommend.html\\)" 1)
  (browse-url-once-if-matched "\\(https://www.ece.cmu.edu/prospective/graduate/application/recommendation/response/[0-9a-f]+/\\)" 1)
  (browse-url-once-if-matched "\\(http://cs.uwaterloo.ca/faculty-recruiting/html/reference/upload_letter.php?url=[0-9a-f_]*\\)" 1)
  (browse-url-once-if-matched "\\(https://academicjobsonline.org/ajo/cid/[0-9a-f]*\\)" 1)
  (browse-url-once-if-matched "'s site: \\(http://www.caringbridge.org/visit/[a-z]*\\(/journal\\)?\\)\n" 1)
  (browse-url-once-if-matched "\\(http://eecs.oregonstate.edu/faculty/referenceform.php?appID=[0-9]*&refid=1\\)" 1)
  (browse-url-once-if-matched "\\(http://webapps.cs.cmu.edu/FacultyReference/[-0-9a-f]+\\)" 1)
  (browse-url-once-if-matched "\\(https://jobs.cs.princeton.edu/reference.php/[0-9a-f]+\\)" 1)
  (browse-url-once-if-matched "\\(https://recruit.apo.ucla.edu/r/a/[0-9a-f/]+\\)" 1)
  (browse-url-once-if-matched "\\(http://academicpositions.harvard.edu/ref/new/[0-9a-f/]+\\)" 1)
  (browse-url-once-if-matched "\\(https://hiring.science.purdue.edu/hiring/upload_ref_letter/index/[0-9a-f]+\\)" 1)
  (browse-url-once-if-matched "\\(https://cs.nyu.edu/webapps/ref/[0-9a-f]+/submit\\)" 1)
  (browse-url-once-if-matched "\\(http://cs.uwaterloo.ca/faculty-recruiting/html/reference/upload_letter.php\\?url=[0-9a-f_]+\\)" 1)
  (browse-url-once-if-matched "\\(https://academicjobs.columbia.edu/userfiles/Central\\?referenceId=[0-9_]+\\)" 1)
  (browse-url-once-if-matched "\\(http://apply.mpi-sws.org/reference/[0-9a-f]+/\\?reference=true\\)" 1)
  (browse-url-once-if-matched "\\(http://eecs.oregonstate.edu/faculty/referenceform.php\\?appID=[0-9]+&refid=[0-9]+\\)" 1)
  (browse-url-once-if-matched "\\(https://norfolk.cs.washington.edu/recommend\\?id=[0-9a-f]+\\)" 1)
  (browse-url-once-if-matched "\\(https://apps.cs.ubc.ca/fac-recruit/se/apply/submitreference.jsp\\?id=[0-9]+&email=mernst@cs.washington.edu\\)" 1)
  (browse-url-once-if-matched "\\(https://careers.research.microsoft.com/ReferenceLetter/upload\\?token=[-0-9a-f]+\\)" 1)
  (browse-url-once-if-matched "\\(https://www.rochester.edu/fort/csc/reference_ver.php\\?id=[0-9a-zA-Z]+\\)" 1)
  (browse-url-once-if-matched "\\(https://academiccareers.uchicago.edu/userfiles/Central\\?referenceId=[0-9_]+\\)" 1)
  (browse-url-once-if-matched "\\(https://www.eecs.umich.edu/eecs/etc/jobs/reference_login.html\\)" 1)
  (browse-url-once-if-matched "\\(https://jobs.illinois.edu/reference-form\\?referenceID=[0-9]+&referenceKey=[0-9a-f]+&formID=[0-9]+&jobId=[0-9]+\\)" 1)
  (browse-url-once-if-matched "\\(https://recruit.ap.uci.edu/r/a/[0-9]+/[0-9a-f]+\\)" 1)
  (browse-url-once-if-matched "Log in, enter the grades, and submit: \\(https://gradepage.uw.edu/\\)" 1)
  (browse-url-once-if-matched "\\(http://indiana.peopleadmin.com/ref/new/[0-9]+/[0-9a-f]+\\)")
  (browse-url-once-if-matched "\\(http://www.tamengineeringjobs.com/ref/new/[0-9]+/[0-9a-f]+\\)")
  (browse-url-once-if-matched "To see the details of the change, please click on the link below:\n\n\\(http://www.changedetection.com/log/.*\\)" 1)
  (browse-url-once-if-matched "https://jobs.uic.edu/reference-page\\?referenceID=[0-9]+&referenceKey=[0-9a-f]+&formID=[0-9]+&jobId=[0-9]+")
  (browse-url-once-if-matched "You can view, comment on, or merge this pull request online at:\n\n *\\(https://github.com/.*/pull/[0-9]+\\)" 1)
  (browse-url-once-if-matched "remote: Create pull request for .*: *\nremote:   \\(https://bitbucket.org/.*/pull-requests/new?source=.*&t=1\\)" 1)
  (browse-url-once-if-matched "remote: To create a merge request for .*, visit: *\nremote:   \\(https://gitlab.eclipse.org/.*/merge_requests/new.*? //)" 1)
  (browse-url-once-if-matched "\\(https://norfolk.cs.washington.edu/recruit/faculty\\?sreview=1&index=[0-9]+\\)" 1)
  (browse-url-once-if-matched "Read this week's newsletter:[ \n]+\\(https?://hamiltonms.seattleschools.org/about/calendar_and_news/hamilton_weekly_news/[^ \t\n]*\\)" 1)
  (browse-url-once-if-matched "auto-browse to \\(https://uw-cse.slack.com/\\)" 1)
  (browse-url-once-if-matched "You can view, comment on, or merge this pull request online at:\n\n  \\(https://github.com/.*\\)\n\nCommit Summary\n\n\\* Bump " 1)
  )
(add-hook 'mew-message-hook 'open-urls)


;; (defun fixup-w3m-output ()
;;   "Clean up ugly w3m output."
;;   ;; "" seems to be a separator that precedes a &nbsp; so just remove it
;;   (save-excursion
;;     (while (search-forward "" nil t)
;;       (replace-match "" nil t)))
;;   ;; change non-breaking spaces to regular spaces
;;   (save-excursion
;;     (while (search-forward " " nil t)
;;       (replace-match " " nil t)))
;;   )
;; (add-hook 'mew-message-hook 'fixup-w3m-output)


;; From Christophe.Troestler@umons.ac.be, with "if" test added by me.
(defadvice mew-fill-match-adaptive-prefix (after mew-remove-itemize-chars activate)
  "Remove the symbols commonly used as indicators for itemize lists from
the prefix used for filling messages."
  ;; (message "%s" ad-return-value) ; debugging
  (if ad-return-value
      (setq ad-return-value
	    (replace-regexp-in-string "[-*•]" " " ad-return-value))))

(defun mew-set-fill-column ()
  (setq fill-column (min 82 (frame-width))))
(add-hook 'mew-message-mode-hook 'mew-set-fill-column)


(defvar mew-sent-queued-mail nil)
(defadvice mew (after send-queued-mail activate)
  "Try to send queued mail upon Mew startup."
  (if (not mew-sent-queued-mail)
      (progn
	(setq mew-sent-queued-mail t)
	(mew-summary-send-message))))


;;; The send-queued-mail advice is a more direct way to do this.
;;; It works for the +queue buffer, but not the +draft buffer.
;; (defun mew-goto-queue-if-nonempty ()
;;   "Visit the +queue folder or the +draft folder if it is empty; otherwise visit %inbox."
;;   (interactive)
;;   (cond ((progn
;; 	  (mew-summary-visit-folder "+queue")
;; 	  ;; (message "queue (point-min) (point-max) = %s %s" (point-min) (point-max))
;; 	  (not (= (point-min) (point-max))))
;; 	 (message "+queue is non-empty")
;; 	 nil ;; do nothing, stay in +queue
;; 	 )
;; 	((progn
;; 	  (mew-summary-visit-folder "+draft")
;; 	  ;; (message "draft (point-min) (point-max) = %s %s" (point-min) (point-max))
;; 	  (not (= (point-min) (point-max))))
;; 	 (message "+draft is non-empty")
;; 	 nil ;; do nothing, stay in +draft
;; 	 )
;; 	(t
;; 	 ;; return to %inbox
;; 	 (mew-summary-visit-folder "%inbox"))))
;;
;; ;; This is actually kicking in the second, not first, time that I run M-x mew.
;; ;; That's acceptable, though.
;; (defvar mew-visited-queue nil)
;; ;; (setq mew-visited-queue t) ; temporarily disable for debugging
;; (defadvice mew (after mew-goto-queue-if-nonempty-once activate)
;;   (if (not mew-visited-queue)
;;       (progn
;; 	(mew-goto-queue-if-nonempty)
;; 	(setq mew-visited-queue t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mail sending
;;;


(setq mew-comment-start-skip "^>+ *")	; default "^> *"
;; This doesn't seem to work: (setq mew-comment-start-skip "^\\(> *\\)+")	; default "^> *"


(defun do-not-auto-fill-message-headers ()
  "Disable auto-fill in message headers"
  (save-excursion
    (goto-char (point-min))
    (search-forward mail-header-separator)
    (beginning-of-line)
    (set-justification-none (point-min) (point))))
(add-hook 'mew-draft-mode-newdraft-hook 'do-not-auto-fill-message-headers)

(defun fixup-cse-support-autoreply ()
  "Remove \"Autoreply\" from subject line, lest mail to CSE support be silently discarded."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\\(\nSubject: Re: [wreq #[0-9]*]\\) Support Online Autoreply\n" nil t)
	(replace-match "\\1\n"))))
(add-hook 'mew-draft-mode-newdraft-hook 'fixup-cse-support-autoreply)



;; Mew automatically uses format=flowed if the message body has lines of
;; length mew-flowed-fold-threshold or longer. Increase it to reduce the
;; use of that.
(setq mew-flowed-fold-threshold 80)	; default 78
;; Also do less folding, so things look less bad if the text passes through
;; a mailer that strips format=flowed. (I'm not so convinced about the
;; desirability of this...)
(setq mew-flowed-fold-length 75)	; default 70

(setq mew-use-format-flowed t)
(defun my-mew-use-format-flowed-hook ()
  (if mew-use-format-flowed
      (progn
	(visual-line-mode 1)
	;; (set-fill-column 996)	; 996 per http://www.ietf.org/rfc/rfc2646.txt
	(set-fill-column 10000)
	)
    (progn
      (visual-line-mode 0)
      (auto-fill-mode 1)
      (set-fill-column 70)		; is 66 a better value, for email?
      )))
;; Note that mew-draft-use-format-flowed is never invoked programmatically
;; by Mew. It is only invoked by the user typing \C-c\C-p\C-f .
(add-hook 'mew-draft-use-format-flowed-hooks 'my-mew-use-format-flowed-hook)

;; Note that quoting must not add spaces between ">".  This is OK:
;;   >>> Some quoted text
;; but this is not:
;;   > > > Some quoted text
;; I'm not sure whether my setup already handles that correctly, or not.

;; Problem: mailman 2.1.9 and eariler drops the
;;   format="flowed"
;; from the "Content-Type:" line.
;; So this can make messages sent through such

;; This seems like a bad idea; I'm getting some garbled text, especially in
;; replies that go through CSE mailing lists. Examples:
;;   Re: [Fuse2013] Request for FuSE 2013 student nominations
;;   Re: [Researchers] [Postdocs] An example of how one shouldn't be intimidated by research authority
;; But maybe it's because I had filled the paragraphs wrongly? Let's give
;; it another try.
;; An issue is that the mailing list drops the


;; This may not seem necessary, because format=flowed will be used
;; automatically if any lines are long enough to need it. But, my hooks
;; for mew-draft-use-format-flowed also set the fill column and such, so
;; invoking it explicitly is still necessary.

;; In Mew, one must do \C-c\C-p\C-f (M-x mew-draft-use-format-flowed) to
;; enable flowed (how does that differ from \C-c\C-f (M-x
;; mew-draft-encode-flowed) ?).  Here is an attempt to make it the
;; universal default. I may want to disable it sometimes.
(defun mew-draft-use-format-flowed-t ()
  "Turn on flowed format."
  (mew-draft-use-format-flowed t))
(add-hook 'mew-draft-mode-hook 'mew-draft-use-format-flowed-t)

;; Ther relevant routines are:
;;   mew-ecsdb-guess-region
;;   mew-charset-guess-region
(defun mew-charset-set-delsp (charset)
  (let ((ecsdb (mew-charset-to-ecsdb charset)))
    (if ecsdb
	(mew-ecsdb-set-delsp ecsdb nil))))
(with-eval-after-load "mew-mule3"
  ;; Why should Greek set delsp? It gets set if there is a smart single
  ;; right quote anyway, so it's a common charset and not specific to Greek.
  (mew-charset-set-delsp "iso-8859-7")
  ;; Similarly for Latin.
  (mew-charset-set-delsp "iso-8859-8")
  ;; en dash (–) isn't in standard character sets (see
  ;; http://www.madore.org/~david/computers/unicode/cstab.html), so
  ;; "iso-2022-jp-3" gets chosen for any document that contains an en dash.
  (mew-charset-set-delsp "iso-2022-jp-3")
  (mew-charset-set-delsp "iso-2022-jp")
  ;; For Kivanc Muslu's name. I fundamentally never want to set delsp to t.
  (mew-charset-set-delsp "utf-8")
  )


;; If I try to send again from the same message buffer, this doesn't seem
;; to get invoked. Workaround: kill the buffer, get the message from the
;; drafts folder, and try again.
(defadvice mew-encode-flowed (before warn-if-delsp activate)
  "Warn if the charset sets delsp.
If delsp is set, then format=flowed breaks words arbirtary locations,
not just on word boundaries."
  (let ((delsp (mew-charset-to-delsp charset)))
    (if (and delsp
             (not (y-or-n-p (concat "Charset " charset " sets delsp. OK? "))))
	(error (concat "Charset " charset " sets delsp")))))

;; Headers
(setq mew-x-mailer nil)
(setq mew-fcc nil)
(setq mew-ask-fcc t)
(setq mew-ask-send nil)
(setq mew-ask-subject t)                ; warn about empty subject line

(defun enable-ispell-message ()
  "Enable spell-checking of email messages."
  (interactive)
  (add-hook 'mew-make-message-hook 'ispell-message))

(defun disable-ispell-message ()
  "Disable spell-checking of email messages."
  (interactive)
  (remove-hook 'mew-make-message-hook 'ispell-message)
  (add-hook 'mew-send-hook 'enable-ispell-message-once))

(defun enable-ispell-message-once ()
  "Enable spell-checking of email messages."
  (interactive)
  (add-hook 'mew-make-message-hook 'ispell-message)
  (remove-hook 'mew-send-hook 'enable-ispell-message-once))

(enable-ispell-message)
;; (disable-ispell-message)

;; todo: add lots more here
(defvar signature-to-delete
  "^[> ]*--
[> ]*You received this message because you are subscribed to the Google\\( \\|[> \n]*\\)Groups \".*\" group.
[> ]*To unsubscribe from this group and stop receiving emails from it,\\( \\|[> \n]*\\)send\\( \\|[> \n]*\\)an email to .*\+unsubscribe@googlegroups.com.
[> ]*To post to this group, send email to\\( \\|[> \n]*\\).*@googlegroups.com.
[> ]*To view this discussion on the web visit\\( \\|[> \n]*\\)https://groups.google.com/d/msgid/.*.
[> ]*For more options, visit https://groups.google.com/d/optout.[> \n]*"
  "Possibly-quoted signatures not to include at end of messages.")

(defun delete-blank-lines-at-end ()
  ;; I want to go to the end of the main part, ignoring attachments
  (save-excursion
    (goto-char (point-min))
    (if (search-forward mew-draft-attach-boundary-beg nil t)
	(progn
	  (goto-char (1- (match-beginning 0)))
	  ;; In Emacs 24.3 and earlier, delete-blank-lines fails if the
	  ;; region includes several blank lines plus any read-only text at
	  ;; the end.
	  (save-restriction
	    (narrow-to-region (point-min) (point))
	    (delete-blank-lines)
	    )
	  (progn
	    (goto-char (point-max))
	    (delete-blank-lines))

	  ;; Remove undesired signatures and quoted blank lines, too.
	  (while (looking-back signature-to-delete nil)
	    (delete-region (match-beginning 0) (match-end 0)))
	  (if (looking-back "^[> \n]+" (save-excursion (beginning-of-line) (point)))
	      (delete-region (match-beginning 0) (match-end 0)))
	  ))))

(add-hook 'mew-make-message-hook 'delete-blank-lines-at-end)


;; I would like to bind a keystroke to this. But which one?
;; (define-key mew-draft-header-map "\C-\t" 'mde-mail-interactive-insert-alias)
;; (define-key mew-header-mode-map "\C-\t" 'mde-mail-interactive-insert-alias)


(defun query-if-no-attachment ()
  (let ((snippet (attachment-text)))
    (if (and snippet
	     (not (mew-attach-p))
	     (not (y-or-n-p
		   (concat "Message has no attachment but has text:  ..."
			   snippet "...\nSend anyway? "))))
        (error "Message has no attachment. Not sent."))))

(defun on-quoted-line ()
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at ">"))))

(defun attachment-text ()
  "Return text around 'attach{ed,ment}', or nil if none."
  (save-excursion
    (goto-char (point-min))
    (let (result)
      (while (and (not result)
		  (re-search-forward "^\\(?:[^>].*\\)?\\(attach\\(?:ed\\|ing\\|ment\\)\\)" nil t))
	(if (not (on-quoted-line))
	    (setq result (buffer-substring (- (match-beginning 1) 10) (+ (match-end 1) 10)))))
      result)))

(add-hook 'mew-make-message-hook 'query-if-no-attachment)

;; Is added to mail-send-hook elsewhere, but Mew only uses mew-send-hook.
;; Should I be using mew-make-message-hook instead?  ispell-message is on both.
(add-hook 'mew-make-message-hook 'mde-mail-send-hook)
(autoload 'mde-mail-send-hook "sendmail-mde")

;;;
;;; Killing draft buffer
;;;

(defvar disable-maybe-kill-mew-draft nil)
(defun maybe-kill-mew-draft ()
  "Ask whether to kill the mew draft along with the buffer."
  (if (and (not disable-maybe-kill-mew-draft)
	   (equal major-mode 'mew-draft-mode))
      (mew-draft-kill)))
;; Test whether the body is empty. There's no point to this unless I call
;; a version of mew-draft-kill that does no user querying.
;; 	   (or (save-excursion
;; 		 (goto-char (point-max))
;; 		 (while (= ?\n (char-before))
;; 		   (backward-char 1))
;; 		 (equal mew-header-separator
;; 			(buffer-substring (- (point) (length mew-header-separator))
;; 					  (point)))))

(defadvice mew-draft-kill (around disable-maybe-kill-mew-draft activate)
  (let ((disable-maybe-kill-mew-draft t))
    ad-do-it))
(defadvice mew-remove-buffer (around disable-maybe-kill-mew-draft activate)
  (let ((disable-maybe-kill-mew-draft t))
    ad-do-it))

(add-hook 'kill-buffer-hook 'maybe-kill-mew-draft)



;;;
;;; Address completion
;;;

(with-eval-after-load "mew-complete"
  (defvar mew-complete-address-default-func 'tab-to-tab-stop
    "Function called if `mew-complete-address' cannot complete a name.
For example, this might use some address insertion mechanism.")
  ;; Redefine to not hardcode the call to tab-to-tab-stop
  (defun mew-complete-address ()
    "Complete and expand an address short name.
First alias key is completed. When completed solely or the @ character
is inserted before the cursor, the short name is expanded to its address."
    (interactive)
    (mew-draft-set-completion-ignore-case mew-complete-address-ignore-case)
    (let ((word (mew-delete-backward-char))
	  (completion-ignore-case mew-complete-address-ignore-case))
      (if (null word)
          ;; WAS: (tab-to-tab-stop)
	  (funcall mew-complete-address-default-func)
	(if mew-use-full-alias
	    (mew-complete
	     word mew-addrbook-alist "alias" nil nil nil
	     'mew-addrbook-alias-get
	     'mew-addrbook-alias-hit)
	  (if (string-match "@." word)
	      (insert (or (mew-addrbook-alias-next word mew-addrbook-alist) word))
	    (mew-complete
	     word mew-addrbook-alist "alias" ?@ nil nil
	     'mew-addrbook-alias-get
	     'mew-addrbook-alias-hit)))))))
(setq mew-complete-address-default-func
      'mde-mail-interactive-insert-alias-interactively)


;; Just like mew-draft-header-comp, but doesn't insert a tab character if
;; no completion is possible. mew-draft-header-comp really shouldn't
;; assume that it is bound to TAB.
(defun mew-draft-header-comp-no-tab ()
  "Complete and expand address short names.
First, a short name is completed. When completed solely or the @ character
is inserted before the cursor, the short name is expanded to its address."
  (interactive)
  (if (mew-draft-on-field-p)
      (mew-complete-field)
    (let ((func (mew-draft-on-value-p mew-field-completion-switch)))
      (if func
	  (funcall func)))))

(defun mew-draft-header-comp-no-tab-maybe ()
  "Call mew-draft-header-comp-no-tab, if in an appropriate location."
  (let ((bol-point) (save-excursion (beginning-of-line) (point)))
    (if (and (looking-at "[ \n]")
	     (not (looking-back "^" bol-point))	; at beginning of line, don't complete
	     (not (looking-back ": *" bol-point)) ; if no text in field, don't complete
	     (not (looking-back "> *" bol-point)) ; if already completed, don't complete, which would give an alternate, less desirable email address for the same person
	     )
	(mew-draft-header-comp-no-tab))))

(defun mew-draft-header-comp-and-next-line ()
  "Complete address via `mew-draft-header-comp', then invoke `next-line'."
  (interactive)
  (mew-draft-header-comp-no-tab-maybe)
  (call-interactively 'next-line))

(defun mew-draft-header-comp-and-newline ()
  "Complete address via `mew-draft-header-comp', then invoke `next-line'."
  (interactive)
  (mew-draft-header-comp-no-tab-maybe)
  (call-interactively 'newline))

(defun mew-draft-header-comp-and-comma ()
  "Complete address, then insert comma."
  (interactive)
  (mew-draft-header-comp-no-tab-maybe)
  (insert ?,))

(defun mew-draft-header-comp-and-end-of-buffer ()
  "Complete address, then move to end of buffer."
  (interactive)
  (mew-draft-header-comp-no-tab-maybe)
  (call-interactively 'end-of-buffer))



;;; Is any of this necessary any more?
(define-key mew-draft-header-map "\C-n"    'mew-draft-header-comp-and-next-line)
(define-key mew-draft-header-map "\C-m"    'mew-draft-header-comp-and-newline)
(define-key mew-draft-header-map [return]  'mew-draft-header-comp-and-newline)
(define-key mew-draft-header-map ","       'mew-draft-header-comp-and-comma)
(define-key mew-draft-header-map "\e>"     'mew-draft-header-comp-and-end-of-buffer)
(define-key mew-draft-header-map [end]     'mew-draft-header-comp-and-end-of-buffer)
(define-key mew-header-mode-map "\C-n"    'mew-draft-header-comp-and-next-line)
(define-key mew-header-mode-map "\C-m"    'mew-draft-header-comp-and-newline)
(define-key mew-header-mode-map [return]  'mew-draft-header-comp-and-newline)
(define-key mew-header-mode-map ","       'mew-draft-header-comp-and-comma)
(define-key mew-header-mode-map [end]     'mew-draft-header-comp-and-end-of-buffer)



;; The versions with my name are not automatically added to the list, so seed it.
(setq mew-mail-address-list
      '("mernst@alum.mit.edu"
	"Michael Ernst <mernst@alum.mit.edu>"
	"mernst@cs.washington.edu"
	"Michael Ernst <mernst@cs.washington.edu>"
	"michael.ernst@imdea.org"
	"Michael Ernst <michael.ernst@imdea.org>"
	"mernst@uw.edu"
	"Michael Ernst <mernst@uw.edu>"
	"mernst@cs.uw.edu"
	"Michael Ernst <mernst@cs.uw.edu>"
	))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Replying
;;;


;; default: (list mew-from: mew-subj: mew-date:)
(setq mew-cite-fields (list mew-subj: mew-from: mew-to: mew-date:))
;; default: "From: %s\nSubject: %s\nDate: %s\n\n"
(setq mew-cite-format "Subject: %s\nFrom: %s\nTo: %s\nDate: %s\n\n")
;; This quoting within mew-cite-format does not work because it doesn't
;; quote continuation lines in, for example, the To: field.
;; (setq mew-cite-format "> Subject: %s\n> From: %s\n> To: %s\n> Date: %s> \n> \n")
(defadvice mew-cite-strings (after quote-it activate)
  (let ((mail-yank-prefix (mew-compute-mail-yank-prefix)))
    (setq ad-return-value
	  (concat mail-yank-prefix
		  (replace-regexp-in-string "\n" (concat "\n" mail-yank-prefix)
					    ad-return-value)))))

(setq mew-summary-reply-with-citation-position 'body)

(defun mew-compute-mail-yank-prefix ()
  "Compute a value for `mail-yank-prefix'."
  (cond
   (mew-cite-prefix-function
    (funcall mew-cite-prefix-function))
   (mew-cite-prefix
    mew-cite-prefix)
   (t
    mew-cite-default-prefix)))

(defadvice mew-cite-original (after set-mail-yank-prefix activate)
  (make-local-variable 'mail-yank-prefix)
  (setq mail-yank-prefix
	(mew-compute-mail-yank-prefix)))

(defadvice mew-cite-original (around move-point-to-beginning activate)
  "Remove blank lines at end, and move point to beginning."
  (let ((start (min (marker-position (mark-marker)) (point))))
    ad-do-it
    (while (looking-back ">+[ \t]*\n" (save-excursion (beginning-of-line) (point)))
      (delete-region (match-beginning 0) (point)))
    (goto-char start)
    ;; This might replace in more than the quoted text. I think that's OK.
    (replace-string-noninteractive " " " ")
    (goto-char start)
    ;; This might replace in more than the quoted text. I think that's OK.
    (replace-string-noninteractive "\n> -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n" "\n> ----------------------------------------------------------------------\n")
    (goto-char start)))

;;; Don't do this, because it screws up format="flowed" processing.
;; (defadvice mew-cite-format-flowed (after always-insert-space activate)
;;   "When citing, insert \"> \" always.
;; Mew by default inserts only \">\" if the following character is \">\".
;; I'm not sure why Mew has this behavior. It looks bad, and also it fools
;; ispell into spell-checking those lines, which is undesirable."
;;   (if (char-equal (char-after) mew-flowed-quoted)
;;       (insert mew-flowed-stuffed)))


(define-key mew-summary-mode-map "a" 'mew-summary-reply-to-sender)   ; default 'mew-summary-reply
(define-key mew-summary-mode-map "A" 'mew-summary-reply)   ; default 'mew-summary-reply-with-citation
(define-key mew-message-mode-map "a" 'mew-message-reply-to-sender)   ; default 'mew-message-reply
(define-key mew-message-mode-map "A" 'mew-message-reply)   ; default 'mew-message-reply-with-citation
(define-key mew-summary-mode-map "\C-t" 'mew-summary-make-thread)   ; had no Mew-specific binding


(defun mew-summary-reply-to-sender ()
  "Like 'mew-summary-reply', but only reply to sender."
  (interactive)
  (mew-summary-reply t))

(defun mew-message-reply-to-sender ()
  "Like 'mew-message-reply', but only reply to sender."
  (interactive)
  (mew-message-goto-summary)
  (call-interactively 'mew-summary-reply-to-sender))


;; This needs to happen before the "use-full-name" advice
(defadvice mew-to-cc-newsgroups (after remove-dcc-value first activate)
  "If there is a dcc value, remove it from the cc list."
  (setf (nth 1 ad-return-value)
	(remove (mew-cfent-value mew-case 'dcc 'defaultvalue)
		(nth 1 ad-return-value))))

;; When an HTML message is displayed using w3m or eww in Mew, its lines are
;; broken.  Then, when it is yanked into a reply buffer, the broken lines
;; remain. This interacts badly with format=flowed.
;; So, cause the message to be re-formatted by w3m or eww (with a very wide
;; buffer width) immediately before being yanked.
;; This should only do any work if the buffer was formatted using w3m or eww.
;; Does the extra work cause any problems?
(defun mew-cite-refill-message-wide ()
  "From a Mew reply buffer, reformat the corresponding Mew message with a wide fill-column."
  (save-window-excursion
    (save-restriction
      (with-current-buffer (mew-buffer-message)
	(mew-message-goto-summary)
	;; A bit gross, but seems to mostly work.
	(let ((w3m-fill-column 996)
	      (shr-width 996)
	      )
	  (mew-summary-display 'redisplay)
	  (message "mew-message-line-status before: %s" (mew-message-line-status-safe))

	  ;; ;;; This does not work.
	  ;; (with-current-buffer (mew-buffer-message)
	  ;;    (mew-normal-line))

	  (while (not (= 0 (mew-message-line-status-safe)))
	    (mew-summary-line))

	  ;; ;;; This works, but it seems brittle.
	  ;; (mew-summary-line)
	  ;; (message "mew-message-line-status mid: %s" (mew-message-line-status-safe))
	  ;; (mew-summary-line)

	  (message "mew-message-line-status after: %s" (mew-message-line-status-safe))

	  )))))



;; (defun mew-cite-refill-message-wide ()
;;   "From a Mew reply buffer, reformat the corresponding Mew message with wide lines."
;;   (save-window-excursion
;;     (with-current-buffer (mew-buffer-message)
;;       (mew-message-goto-summary)
;;       (if *****is-html*****
;; 	  (let ((w3m-fill-column 996))
;; 	    (mew-summary-display 'redisplay))
;; 	(if (with-current-buffer (mew-buffer-message)
;; 	      (= 1 (mew-message-line-status)))
;; 	    ;; wrapped lines; change to normal lines
;; 	    (progn
;; 	      (mew-summary-line)
;; 	      (mew-summary-line)))))))


(defun mew-message-line-status-safe ()
  "Invokes `mew-message-line-status', in the right buffer."
  (with-current-buffer (mew-buffer-message)
    (mew-message-line-status)))

(defadvice mew-draft-cite (before refill-wide activate)
  (mew-cite-refill-message-wide))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forwarding
;;;

;; Maybe the default should depend on whether the current message is MIME
;; and/or has attachments?
(defadvice mew-summary-forward (around maybe-inline activate)
  "With prefix argument, insert the original message inline
rather than as an attachment."
  (if current-prefix-arg
      (jl-forward-inline)
    ad-do-it))

;; Originally from http://cermics.enpc.fr/~lelong/Emacs/dotmewdotel.html,
;; slightly modified.
(defun jl-forward-inline ()
  "Forwards a message inline. Inspired by mew-summary-reply."
  (interactive)
  (mew-summary-msg-or-part
   (mew-summary-not-in-draft
    (mew-current-set-window-config)
    (let* ((owin (selected-window))
           (fld (mew-summary-folder-name))
           (msg (mew-summary-message-number2))
           (draft (mew-folder-new-message mew-draft-folder))
           (to (and mew-ask-to (mew-input-address (concat mew-to: " "))))
           (cc (and mew-ask-cc (mew-input-address (concat mew-cc: " "))))
           (asked (or mew-ask-to mew-ask-cc))
           msg-subject msg-to msg-from msg-date fwsubject cwin)
      (mew-summary-prepare-draft
       (mew-draft-find-and-switch draft t)
       (mew-delete-directory-recursively (mew-attachdir draft))
       (setq cwin (selected-window)) ;; draft
       (select-window owin)
       (mew-summary-set-message-buffer fld msg)
       (setq msg-subject (mew-header-get-value mew-subj:))
       (setq msg-to (mew-header-get-value mew-to:))
       (setq msg-from (mew-header-get-value mew-from:))
       (setq msg-date (mew-header-get-value mew-date:))
       (if msg-subject
           (setq fwsubject (mew-subject-simplify (concat mew-forward-string msg-subject))))
       (select-window cwin) ;; draft
       ;;
       (mew-draft-header fwsubject 'nl to cc nil nil nil nil asked)
       (mew-draft-mode)
       (save-excursion
	 (goto-char (point-max))
	 (insert "------- Start of forwarded message -------\n")
         (insert "Subject: ") (insert msg-subject) (insert "\n")
         (insert "Date: ") (insert msg-date) (insert "\n")
         (insert "From: ") (insert msg-from) (insert "\n")
         (insert "To: ") (insert msg-to) (insert "\n")
         (insert "\n\n")
         (mew-draft-yank)
	 (insert "------- End of forwarded message -------\n"))
       ;; Do fixups, should really be integrated into `mew-draft-yank' or `mew-draft-cite'.
       (save-excursion
	 (replace-string-noninteractive " " " ")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fixes
;;;

;; As of 10/2009, mew-summary-mode-line crashes frequently, because
;; (mew-sinfo-get-mid-marker) returns nil. So catch that error.
(defadvice mew-summary-mode-line (around catch-error activate)
  (condition-case err
      ad-do-it
    (error
     (message "Caught error in mew-summary-mode-line: %s" err))))


;; This depends on using a version of Mew that contains this patch:
;; https://github.com/kazu-yamamoto/Mew/pull/38
(let ((redhat-version
       (and (file-exists-p "/etc/redhat-release")
	    (shell-command-to-string "cat /etc/redhat-release"))))
  (if (and redhat-version
	   (string-match "Fedora release 19" redhat-version)
	   )
      ;; This is not necessary with stunnel 5, because fips is off by default
      (setq mew-prog-ssl-arg "fips=no\n")
    ))


;;; I have submitted a patch that should make the below unnecessary.
;; ;; Outputs mew-ssl-options-extra.
;; (defvar mew-ssl-options-extra "")
;; (defun mew-ssl-options-with-extra (case server remoteport localport tls)
;;   (setq server (mew-ssl-server server))
;;   (if (= mew-ssl-ver 3)
;;       (let (args)
;; 	(setq args
;; 	      `("-c" "-f"
;; 		"-a" ,(expand-file-name (mew-ssl-cert-directory case))
;; 		"-d" ,(format "%s:%d" mew-ssl-localhost localport)
;; 		"-v" ,(number-to-string (mew-ssl-verify-level case))
;; 		"-D" "debug"
;; 		"-P" ""
;; 		"-r" ,(format "%s:%d" server remoteport)
;; 		,@mew-prog-ssl-arg))
;; 	(if tls (setq args (cons "-n" (cons tls args))))
;; 	args)
;;     (let ((file (mew-make-temp-name)))
;;       (with-temp-buffer
;; 	(insert "client=yes\n")
;; 	(insert "pid=\n")
;; 	(insert (format "verify=%d\n" (mew-ssl-verify-level case)))
;; 	(insert "foreground=yes\n")
;; 	(insert "debug=debug\n")
;; 	(if (and mew-ssl-libwrap (>= mew-ssl-minor-ver 45))
;; 	    (insert "libwrap=no\n"))
;; 	(if (>= mew-ssl-minor-ver 22)
;; 	    (insert "syslog=no\n"))
;; 	(insert "CApath=" (expand-file-name (mew-ssl-cert-directory case)) "\n")
;; 	(insert mew-ssl-options-extra)
;; 	(insert (format "[%d]\n" localport))
;; 	(insert (format "accept=%s:%d\n" mew-ssl-localhost localport))
;; 	(insert (format "connect=%s:%d\n" server remoteport))
;; 	(if tls (insert (format "protocol=%s\nsslVersion=TLSv1\n" tls)))
;; 	(mew-frwlet mew-cs-dummy mew-cs-text-for-write
;; 	  ;; NEVER use call-process-region for privacy reasons
;; 	  (write-region (point-min) (point-max) file nil 'no-msg))
;; 	(list file)))))
;;
;;
;; (with-eval-after-load "mew-ssl"
;;      (defun mew-ssl-options (case server remoteport localport tls)
;;        (mew-ssl-options-with-extra case server remoteport localport tls)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

(provide 'mew-mde)
