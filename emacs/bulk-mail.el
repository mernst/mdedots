;;; -*- lexical-binding: t -*-

;;; bulk-mail.el --- Support for sending bulk mail


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bulk mail (e.g., trolling for prospective grad students)
;;;

;; An example of a formatted file is: ~/prof/contacts/contacts-edu

;; Then, put the cursor on an email address and call
;;   M-x compose-mail-address-buffer-and-bury
;; repeatedly.  On each iteration, this gives a
;; prepared mail buffer; you still have to explicitly send the mail with
;; C-c C-c (and can do customization before you do).

;; Or, do (compose-all-mail-address-buffer) to make lots of messages.
;; You can send them all with
;;   (setq mew-make-message-hook nil)
;; and then, in the +draft folder,
;;   (while t (mew-summary-reedit) (mew-draft-send-message) (sit-for 5))

;;; Code:

(defvar compose-mail-function 'compose-mail-and-insert
  "Either symbol `compose-mail-and-insert' or a function with the same interface.")
(defvar my-subject nil
  "Subject line for bulk mail to be sent.
Can contain extra header lines, such as Cc:")
(defvar my-message nil
  "Message for bulk mail to be sent.")
(defvar email-address-group-comma-concatenates t
  "If nil, send individual mail.
That is, each address group will consist of just one address.")
(defvar my-honorific nil
  "Honorific for bulk mail to be sent.  Example: \"Councilmember\".")

(defun compose-mail-address-buffer ()
  "Call this repeatedly in a buffer full of email addresses.
Each time it sends mail to one person.
Customize it first to hard-code the message, etc."
  (interactive)
  (if (null my-subject)
      (error "My-subject must be non-nil"))
  (funcall compose-mail-function
	   (get-email-address-group)
	   my-subject my-message))

(defun compose-mail-address-buffer-and-bury ()
  "Call this repeatedly in a buffer full of email addresses.
Each time it sends mail to one person.
Customize it first to hard-code the message, etc."
  (interactive)
  (compose-mail-address-buffer)
  (bury-buffer))

(defun compose-all-mail-address-buffer ()
  "Call this in a buffer full of email addresses to send mail to everyone.
Customize it first to hard-code the message, etc."
  (interactive)
  (while t
    (compose-mail-address-buffer-and-bury)))

(defun get-email-address-group ()
  "This collects all the email addresses in the next group in the buffer.
Point is left after the group.
A group starts after a space and continues until a line does not
end with a comma.  Each line in the group should be indented;
this makes them continuation lines in the email."
  (let* ((group-start (progn
			(re-search-forward "^[ \t]+")
			(point)))
	 (group-end (progn
		      (re-search-forward (if email-address-group-comma-concatenates
					     "[^,]$"
					   "$"))
		      (skip-chars-backward ",\n")
		      (point))))
    (buffer-substring group-start group-end)))

(defun compose-mail-and-insert (addresses subject message &optional other-headers)
  "A wrapper around compose-mail that also inserts the message."
  (compose-mail addresses subject other-headers)
  (goto-char (point-max))
  (insert message))

(defun mail-firstname (address)
  "Return a string representing the first name of the addressee."
  (cond ((string-prefix-p "Juan Pablo Galeotti" address)
	 "Juan Pablo")
	((string-match "^\"?\\([^ ]+\\) " address)
	 (match-string 1 address))
	(t
	 nil)))

(defun mail-lastname (address)
  "Return a string representing the last name of the addressee."
  (and (string-match " \\([^ ]+\\)\"? <" address)
       (match-string 1 address)))

(defun mail-names (fn addresses)
  "Return a string representing the first or names of the addressees,
depending on the function argument."
  (let* ((names (split-string addresses ">,[ \t\n]+"))
	 (firstnames (mapcar fn names)))
    (cond ((= (length firstnames) 1)
	   (car firstnames))
	  ((= (length firstnames) 2)
	   (concat (car firstnames) " and " (cadr firstnames)))
	  (t
	   (setf (car (last firstnames)) (concat "and " (car (last firstnames))))
	   (mapconcat 'identity firstnames ", ")))))

(defun mail-firstnames (addresses)
  "Return a string representing the first names of the addressees."
  (mail-names #'mail-firstname addresses))
;; (mail-firstnames "Saman Amarasinghe <saman@csail.mit.edu>,\n Daniel Jackson <dnj@csail.mit.edu>,\n Rob Miller <rcm@csail.mit.edu>,\n Martin Rinard <rinard@csail.mit.edu>,\n Armando Solar-Lezama <asolar@csail.mit.edu>,\n Adam Chlipala <adamc@csail.mit.edu>")

(defun mail-lastnames (addresses)
  "Return a string representing the last names of the addressees."
  (mail-names #'mail-lastname addresses))
;; (mail-lastnames "Saman Amarasinghe <saman@csail.mit.edu>,\n Daniel Jackson <dnj@csail.mit.edu>,\n Rob Miller <rcm@csail.mit.edu>,\n Martin Rinard <rinard@csail.mit.edu>,\n Armando Solar-Lezama <asolar@csail.mit.edu>,\n Adam Chlipala <adamc@csail.mit.edu>")

;; this could be advice around compose-mail-and-insert
(defun compose-mail-and-insert-with-firstname (addresses subject message &optional other-headers)
  "A wrapper around compose-mail that also inserts the message and a first name."
  (let* ((firstname (mail-firstnames addresses)))
    (compose-mail addresses subject other-headers)
    (goto-char (point-max))
    (insert message)
    (search-backward mail-header-separator)
    (forward-line 1)
    (if firstname
	(save-excursion
	  ;; (insert firstname "-\n\n")
	  (insert "Dear " firstname ",\n\n")
	  ))))

(defun compose-mail-and-insert-with-lastname (addresses subject message &optional other-headers)
  "A wrapper around compose-mail that also inserts the message and a first name."
  (let* ((lastname (mail-lastnames addresses)))
    (compose-mail addresses subject other-headers)
    (goto-char (point-max))
    (insert message)
    (search-backward mail-header-separator)
    (forward-line 1)
    (if lastname
	(save-excursion
	  ;; (insert lastname "-\n\n")
	  (insert "Dear " my-honorific " " lastname ",\n\n")
	  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Refereeing requests
;;;

(defun ecoop-request-external-review (number)
  (interactive "sPaper number: ")
  (compose-mail nil (concat "ECOOP review request (paper " number ")"))
  (mail-position-on-field "Subject")
  (goto-char (point-max))
  (insert "
Could you help me out with an external review of this ECOOP submission?

  *****

I would like to have the reviews done by January 23.

                     Thanks,

                    -Mike

")
  (ecoop-attach-paper-and-form number))

(defun ecoop-attach-paper-and-form (number)
  (interactive "sPaper number: ")
  (let* ((ecoop-dir "/afs/csail.mit.edu/u/m/mernst/prof/referee/ecoop-2006/")
	 (pdf-file (concat ecoop-dir "papers/ecoop2006-" number ".pdf"))
	 (review-file (concat ecoop-dir "reviews/reviewform" number ".txt")))
    (if (not (file-exists-p pdf-file))
	(error "No file %s" pdf-file))
    (if (not (file-exists-p review-file))
	(error "No file %s" review-file))
    (vm-mime-attach-file
     pdf-file
     "application/pdf" nil (concat "ECOOP paper " number) nil)
    (vm-mime-attach-file
     review-file
     "application/octet-stream" nil (concat "ECOOP review form " number) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Political
;;;

;; as of 3/2014, from http://www.seattle.gov/council/councilcontact.htm
(defvar bulk-mail-councilmembers
 "
 Sally Bagshaw <sally.bagshaw@seattle.gov>
 Tim Burgess <tim.burgess@seattle.gov>
 Sally Clark <sally.clark@seattle.gov>
 Jean Godden <jean.godden@seattle.gov>
 Bruce Harrell <bruce.harrell@seattle.gov>
 Nick Licata <nick.licata@seattle.gov>
 Mike O'Brien <mike.obrien@seattle.gov>
 Tom Rasmussen <tom.rasmussen@seattle.gov>
 Kshama Sawant <kshama.sawant@seattle.gov>
")

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-lastname)
(setq my-honorific "Councilmember")
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Please do not limit ride-sharing in Seattle")
(setq my-message
      "\
I urge you to vote against limits to ridesharing companies next Monday.

If Seattle limits ridesharing, that would hurt consumers who want more choices and convenience, and it would hurt workers who drive to put food on their tables.  (Even some current taxi drivers are finding it desirable to work for rideshare companies!)

Seattle should not be in the business of legislating a monopoly for taxis.  Nor should Seattle punish the ridesharing companies for their innovation and their success in the marketplace.  Only competition from ridesharing companies has started to (slowly) improve Seattle's notoriously bad taxi service.  Let's keep that competition, which is good for the city.

                    -Michael Ernst
")
))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other customizations for the above
;;;

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "UW is recruiting grad students, postdocs, and faculty")
(setq my-message
      "\
The University of Washington is looking for great grad students.  We have recently increased the size of our entering class of grad students, and so we're interested in increasing our pool of applicants.  I will finish 5-6 students this year and want to refresh my pipeline; we have added a great new faculty member in Zach Tatlock; and we have hired outside programming languages and software engineering as well.

Please encourage your outstanding students to apply to UW's PhD program!  You can point them at http://www.cs.washington.edu/education/grad/prospective.html .

I also have funding for postdocs.  And UW continues to expand its faculty, thanks to targeted support from our legislature, so let me know of exceptional prospects in any field.

Naturally, we will reciprocate.  I continue to send undergrads to grad programs all over the world -- we discourage them from staying at UW.  The best two faculty prospects I am graduating this year are Colin Gordon (concurrency, separation logic) and Sai Zhang (testing, error localization, resolving end-user problems).  If you are hiring, please take a look at them.  I'm happy to provide more information.

Thanks!

                    -Mike

--
http://homes.cs.washington.edu/~mernst/")
))

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Acceptance to the Combined BS/MS degree program
Cc: ugrad-advisor@cs.washington.edu")
(setq my-message
      "\
Congratulations!  You have been admitted into the Combined BS/MS
degree program in Computer Science & Engineering.

Before August 31, please let us know whether you accept this offer
by responding to our catalyst survey:
  https://catalysttools.washington.edu/webq/survey/cseadv/55721

If you have questions or concerns, or wish to discuss your plans
before deciding, please feel free to contact the CSE advising
staff, me, or any other faculty member.  For more information,
see the BS/MS webpages:
  http://www.cs.washington.edu/education/bsms/

If you accept, and you will be a graduate student in the next
academic year, then you must apply to the UW Graduate
School (this is a necessary formality).
  https://www.grad.washington.edu/applForAdmiss/
Please note that there is a $75 application fee and you should
apply for the quarter you will become a graduate student.

We will contact you over the summer to discuss future course
plans.

We look forward to working with you!

Sincerely,

Michael Ernst
Combined BS/MS Chair
")
))

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Your application to the Combined BS/MS degree program
Cc: ugrad-advisor@cs.washington.edu")
(setq my-message
      "\
I regret to inform you that you were not admitted to the CSE
aCombined BS/MS program.  The program has a very limited number of
spaces, and we received applications from many more strong
students than we have the capacity to admit.  The process, which
was based on all aspects of the applications, was extremely
competitive and the admissions committee had to make some very
difficult decisions.

We understand that this is disappointing news and that you may
have concerns about your future plans.  Please feel free to
contact the CSE advising staff and faculty to discuss your career
and academic options.

We wish you the best with the remainder of your studies.

Sincerely,

Michael Ernst
Combined BS/MS Chair
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "CSE 331 TAing")
(setq my-message
      "\
We are looking to add an additional TA to CSE 331 next quarter, to reduce
the overall workload for all the TAs.  (We are making some other changes to
reduce workload as well.)  Would you be interested in considering TAing?  I
would be happy to talk with you about the position.

                     Thanks,

                    -Mike")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Follow-on to CSE 331: research and teaching")
(setq my-message
      "\
Congratulations for doing very well in CSE 331 last winter!

I am writing to ask you to think about both research and teaching, since I
suspect you would do well at both.  For teaching, CSE 331 is always looking
for well-qualified TAs to help the students.  (I'm next teaching CSE 331 in
spring, but it is currently offered every quarter.)

I suggest you also consider getting involved in research.  Research is an
excellent way to enrich your education -- it's fun, and you'll get
experiences that you can't obtain any other way.

If you already have a research position, that's great!  If not, or if you
are looking for a change, I would be happy to talk with you about
opportunities in my research group, and elsewhere in the department, for
this or later quarters.

                    -Mike")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Opportunity to TA for CSE 331")
(setq my-message
      "\
I'm looking for excellent TAs to help with CSE 331 next quarter.  I think
you would make a good match for the class.  Would you be interested in
talking with me about the opportunity?

TAing CSE 331 is attractive for a number or reasons.  You get to teach
sections and have considerable leeway in designing those sections.  The
class infrastructure is solid, so the staff can work on making improvements
to the material itself, or can just reuse the existing materials where they
are adequate.  I'm open to new ideas about teaching the class, so the TAs
aren't in a straitjacket.  And, the material is interesting, important,
and fun.  CSE 331 teaches programming concepts from both a theoretical and
a practical point of view.  More importantly, it goes beyond mere
programming to system design, reasoning, and related activities.

I would enjoy talking with you about CSE 331.  My calendar is at
http://homes.cs.washington.edu/~mernst/.

                    -Mike")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "UW CSE software engineer job")
(setq my-message
      "\
I'm writing to you about a software engineer (staff programmer) job at UW
CSE, which you previously applied for:
https://uwhires.admin.washington.edu/eng/candidates/default.cfm?szCategory=jobprofile&szOrderID=78994

I just discovered that your resume was accessible only to someone in HR,
who thought I also had access.  I'm sorry about this mixup.

Now that I can see your resume, I like what I see, and we may have a good
fit.  Since it has been a little while since your application, can you let
me know if you would still like to be considered for the position?

                     Thanks,

                    -Mike

--
Michael Ernst
Professor, U. of Washington Computer Science & Engineering
mernst@cs.washington.edu   http://homes.cs.washington.edu/~mernst/")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Choosing which grad schools to apply to")
(setq my-message
      "\
I'm writing to ask you for a quick piece of information that will be
helpful to me.

When you applied to graduate school, you did not apply to the University of
Washington.  Would you be willing to let me know your reasons?  Any honest
reasons are fine -- you won't offend me.

Thanks very much.  I wish you the very best in your graduate studies!

                    -Mike

--
Michael Ernst
Professor, U. of Washington Computer Science & Engineering
mernst@cs.washington.edu   http://homes.cs.washington.edu/~mernst/")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "CSE 140 registration:  please drop")
(setq my-message
      "\
You are currently registered for CSE 140.  However, CSE 140 is not open to
any student who has taken, or is currently taking, any 300-level CSE class.
You have taken CSE 373, or you are registered for it in Winter 2013.  Can
you please drop your registration for CSE 140, to make room for other students
in the class?

                     Thanks,

                    -Mike

--
Michael Ernst
Professor, U. of Washington Computer Science & Engineering
mernst@cs.washington.edu   http://homes.cs.washington.edu/~mernst/")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "CSE 140 registration:  please drop")
(setq my-message
      "\
You are currently registered for CSE 140.  However, CSE 140 is not open to
any student who has taken, or is currently taking, any 300-level CSE class.
You have taken CSE 373, or you are registered for it in Winter 2013.  Can
you please drop your registration for CSE 140, to make room for other students
in the class?

                     Thanks,

                    -Mike

--
Michael Ernst
Professor, U. of Washington Computer Science & Engineering
mernst@cs.washington.edu   http://homes.cs.washington.edu/~mernst/")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Feedback on CSE 140")
(setq my-message
      "\
You started CSE 140 in Winter 2013, then dropped the class.

I would love to have your feedback on CSE 140, so that we can improve it in the future.  I'm curious about:

 * why you dropped
 * what you liked and disliked
 * what you expected and how it was different from your expectations
 * how the class could have been better advertised to give you a clearer picture of it
 * what parts of it should change and what should remain the same.

I'm happy to receive as much feedback as you can provide, but even a brief answer would be helpful to me.

You can reply to me by email, or if you are uncomfortable with that there is a form for sending anonymous email at https://catalyst.uw.edu/umail/form/mernst/4152 .

Thanks very much!  I really appreciate your help in improving CSE 140.

                    -Mike")
))





(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Speak at Notkinfest?")
(setq my-message
      "\
Recall that there will be an opportunity to speak to the audience at the
Notkinfest, to briefly recount an anecdote that captures David's spirit.

Before I finalize the list of speakers at the Notkinfest, I wanted to give
you a chance to get on the list.  If you don't want to, or if you prefer to
contribute a written anecdote, that is fine too.

                     Thanks,

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Notkinfest invitation and call for anecdotes
Cc: Kay Beck-Benton <kbeck@cs.washington.edu>")
(setq my-message
      "\
You are invited to a Notkinfest on Friday, February 1.  See the invitation at
http://homes.cs.washington.edu/~mernst/notkinfest-dinner-invitation.pdf .
In brief, there will be a colloquium at 4:00, reception 4:30-6:00 (with
some remarks at 5:00), and dinner at 6:00 (to which you and a guest are
invited).  Dinner attendance is by invitation and requires an RSVP to
kbeck@cs.washington.edu by January 25.  If you cannot attend, please RSVP
that fact, as it will help with planning.

The dinner schedule includes plenty of time to share stories about David --
in fact, this is the main purpose of the Notkinfest!  Please let me
(mernst@cs.washington.edu) know if you would like to speak briefly to the
crowd to recount an anecdote that captures David's spirit.  If you cannot
attend or do not wish to speak publicly, then we would be happy to have
your stories in writing, and we will make a small book.  Please also send
me photographs or other memorabilia to share.

Feel free to ask Kay or me if you have questions.  I look forward to seeing
you on February 1.

                    -Mike
")
))


;; for reception only:
(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Notkinfest invitation")
(setq my-message
      "\
You are invited to a Notkinfest on Friday, February 1.  The
invitation is attached.  In brief, there will be a colloquium at
4:00 and a reception 4:30-6:00 (with some remarks at 5:00).

I look forward to seeing you on February 1.

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Slides about your research with David Notkin")
(setq my-message
      "\
I've been asked to give a brief technical presentation at the Notkinfest,
titled \"Software evolution then and now:  the research impact of David
Notkin\".  It's part of the public portion of the Notkinfest, and we expect
a lot of students to attend, so it's a good way to advertise our field and
serve the CSE student community.  But, it is a very short talk slot (30
minutes) because, you know, software engineering isn't very interesting or
deep.

Despite the short slot, I would like to include a discussion of your work
with David.  Do you happen to have, or could create, a few slides
that capture the essence of the problem, the solution, and why it is
interesting and effective?  If not, I'll work to create some visuals on
my own, but if you could prime me, that would be a huge help.

Thanks, once again, for your help!  I really appreciate it.

                    -Mike
")
))


(if nil (progn

;;; Commentary:
;;

(require 'bulk-mail)
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Sai Zhang faculty application")
(setq my-message
      "\
If your department is interested in hiring in software engineering this year, I suggest you take a look at my student Sai Zhang (http://homes.cs.washington.edu/~szhang/).  The consensus is that he is one of the top 2 students graduating in software engineering this year.  (The other is Lingming Zhang of the University of Texas, and I am arguing for my department to interview Lingming this year; we have never interviewed one of our own new graduates.)  I still think Sai is better, but I might be biased. :-)

I wanted to write to make sure that Sai Zhang's application does not fall through the cracks.  Please let me know if you need any more information that would help you, and best wishes for a good recruiting year!

                     Thanks,

                    -Mike")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Signing up to meet with Chris Parnin")
(setq my-message
      "\
Chris Parnin is interviewing for a faculty position in CSE this week.  His bio appears below.  Could you sign up to meet with him?  You can use this link to do so:
https://reserve.cs.washington.edu/visitor/week.php?year=2014&month=02&day=20&area=5&room=2245

Also, don't forget his talk at 3:30 on Thursday, February 20 in room EEB 105.

Thanks!

                    -Mike


Chris Parnin is hard to pigeonhole, since his work is characterized by unorthodoxy, but the simplest perspective is that he studies software engineering from empirical, HCI, and cognitive neuroscience perspectives. He also builds tools informed by his findings.  Topics he has addressed include workplace interruptions, cognitive frameworks of program comprehension, adoption of language features and tools, software visualization, alternative and live programming environments, usability of developer tools, and crowdsourcing systems.  Two of his recent research results involve using fMRI and EMG to actually study the brain activity of developers, and understanding how crowds of developers come together on sites such as Stack Overflow and Github to contribute software knowledge. Chris also has over a decade of professional programming experience in the defense industry.  Despite supporting himself and his family through grad school, Chris's research has been recognized by the SIGSOFT Distinguished Paper Award at ICSE 2009, Best Paper Nominee at CHI 2010, Best Paper Award at ICPC 2012, IBM HVC Most Influential Paper Award 2013, and featured in the press such as Game Developer's Magazine and Hacker Monthly.  Chris will receive his PhD from Georgia Tech in 2014.
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Possible visit in summer or fall 2014")
(setq my-message
      "\
I hope to be in Europe on sabbatical from early August to late December 2014.

I would enjoy visiting you at some point during that time, if possible.
Would such a visit make sense?
Are there times that would be better or worse for you?

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "Signing up to meet with Chris Parnin")
(setq my-message
      "\
I'm looking to fill two more slots for meetings with Chris Parnin, at 10:00 and 11:00 on Thursday.  There is also time available at 5:00 or later on Friday.  Are you able to help fill one of those slots?  If not, I could perhaps rearrange some other meetings to make a slot available when you have time.

Here is the signup link:
https://reserve.cs.washington.edu/visitor/week.php?year=2014&month=02&day=20&area=5&room=2245

Thanks!

                    -Mike
")
))

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "One more slot to fill with Emina Torlak")
(setq my-message
      "\
Someone dropped out of Emina Torlak's schedule on Friday at 3:30 and I need to fill it.  Can you help?  (There is also a slot at 9:00 on Thursday, which would be delightful to fill, but it isn't as critical.)

Here is the signup link:
https://reserve.cs.washington.edu/visitor/week.php?year=2014&month=05&day=01&room=2251

Thanks!

                    -Mike
")
))

;; Open call
(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "René Just's faculty application")
(setq my-message
      "\
I wanted to draw your attention to René Just's faculty application, just to make sure that it doesn't fall through the cracks.  René is currently finishing up a postdoc with me.  René is extremely strong; for example, he won two ACM Distinguished Paper Awards last year and I expect him to continue the trend.  René works primarily in software engineering, such as testing and program analysis.  He is also a genuinely nice guy who would make a good colleague.  I encourage you to look at his application and try to hire him before other places beat you to it.  Let me know if you have any questions or concerns.

                    -Mike
")
))

;; SE focus
(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "René Just's faculty application")
(setq my-message
      "\
I wanted to draw your attention to René Just's faculty application, just to make sure that it doesn't fall through the cracks.  René is currently finishing up a postdoc with me.  René is extremely strong; for example, he won two ACM Distinguished Paper Awards last year and I expect him to continue the trend.  René works primarily in software engineering, such as testing and program analysis.  He is also a genuinely nice guy who would make a good colleague.  Your job posting says that software engineering is a focus area, so I think it would be an error not to interview René.  I encourage you to look at his application and try to hire him before other places beat you to it.  Let me know if you have any questions or concerns.

                    -Mike
")
))

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Kivanc Muslu's faculty application")
(setq my-message
      "\
I wanted to draw your attention to Kivanc Muslu's faculty application, just to make sure that it doesn't fall through the cracks.  Kivanc is finishing up his PhD.  In a short time, Kivanc has amassed a strong publication record in a broad set of topics, generally in the field of software engineering but spanning speculative analysis, development tools, testing, concurrency, type systems, and studies of development histories.  I encourage you to take a serious look at his application, before other places beat you to hiring him.  Let me know if you have any questions or concerns.

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Two faculty applications (René Just and Kivanc Muslu")
(setq my-message
      "\
I wanted to draw your attention to two faculty applications, just to make sure that it doesn't fall through the cracks.

René Just is currently finishing up a postdoc with me.  René is extremely strong; for example, he won two ACM Distinguished Paper Awards last year and I expect him to continue the trend.  René works primarily in software engineering, such as testing and program analysis.  He is also a genuinely nice guy who would make a good colleague.

Kivanc is finishing up his PhD.  In a short time, Kivanc has amassed a strong publication record in a broad set of topics, generally in the field of software engineering but spanning speculative analysis, development tools, testing, concurrency, type systems, and studies of development histories.

I encourage you to look at their applications and try to hire them before other places beat you to it.  Let me know if you have any questions or concerns.

                    -Mike
")
))

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "UW grad school")
(setq my-message
      "\
Congratulations on being admitted to grad school at UW!  We are delighted about this:  we think you will be a good match for UW, will be productive, and most importantly will have lots of fun learning and doing research.  I trust you are happy about it too!

You doubtless have multiple good options, so you have a big decision ahead of you.  To help you with your decision, I would enjoy talking with you about research, UW, grad school in general, or anything else that is on your mind.

One small complication is that I am on sabbatical, currently in South America.  My communication with the outside world is sometimes patchy; for example, I was stranded by a flood this week.  Nonetheless, I would love to talk with you.  If you can give me some options for when I could call you, I will do my best to accommodate one of them.

Congratulations again, and I look forward to talking with you.

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Research Software Engineer job at UW CSE")
(setq my-message
      "\
You recently applied for a Research Software Engineer job at UW CSE (https://uwhires.admin.washington.edu/eng/candidates/default.cfm?szCategory=jobprofile&szOrderID=122574).  From your application, it seems you could be a good match for the position.

Could we talk by phone to explore whether this is right?  I can answer your questions about the job, and you can tell me more about your interests.

I am currently traveling with limited access to phone/Internet, but could we talk during the week of August 3-7?  For scheduling, could you please see my schedule at http://homes.cs.washington.edu/~mernst/calendar.html and suggest some mutually available times?  I can also make time in the evening (8pm-10pm) if that is better you.

I look forward to talking with you.

                    -Mike
--
Michael Ernst
Professor, U. of Washington Computer Science & Engineering
mernst@cs.washington.edu   http://homes.cs.washington.edu/~mernst/
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "UW software engineer job: interview invitation")
(setq my-message
      "\
I'm writing to you about the Software Engineer job that you applied for in my group at UW (http://homes.cs.washington.edu/~mernst/programmer-ad.html).

It seems the job could be a good match to your skills and interests.  My team would like to meet you in person to learn more and to let you learn more.  Do you have particular constraints or preferences regarding when you could visit UW for an interview?  Our interviews generally last about 4.5 hours.

Also, could you provide me with contact information for 3 people who are familiar with your past work experience?

I look forward to meeting you.

                    -Mike
--
Michael Ernst
Professor, U. of Washington Computer Science & Engineering
mernst@cs.washington.edu   http://homes.cs.washington.edu/~mernst/
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "PNW PL&SE meeting, March 15, 2016 at UW")
(setq my-message
      "\
We are happy to invite you to the Inaugural PNW Programming Languages and Software Engineering Meeting, which will take place on March 15, 2016, at the University of Washington, 12pm-7pm, in Husky Union Building (HUB 332).

We hope you decide to attend! We encourage you to give a talk or a demo.

If you are interested in attending, please RSVP at
  https://docs.google.com/a/cs.washington.edu/forms/d/1OqueIXXIHSVTcGpknJjZDqEUi3wd3q3RJB8dEVi0mkI/viewform?c=0&w=1
as soon as possible.  This form also allows you to register your talk or demo.  The committee will start making selections on March 1.

Also feel free to forward this invitation to your colleagues. Thank you!

Cheers,

Your PL/SE colleagues at UW and MSR
http://uwplse.org/#faculty
http://research.microsoft.com/en-us/groups/rise/
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "CSE 590N schedule")
(setq my-message
      "\
This is a gentle reminder to please sign up for a slot to lead the CSE 590N discussion.  For any unclaimed week, you can also change the paper (including taking ones from the paper suggestions), if you know of a paper that is better.  Thanks!

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "CSE 590N schedule")
(setq my-message
      "\
Could you please select a paper and a day, and sign up to lead the CSE 590N discussion at http://uwplse.org/meet/serg/ ?  Let me know if you need any help or advice.  Thanks!

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "FSE Student Research Competition, 2-page paper due July 1")
(setq my-message
      "\
I encourage you to submit to the FSE Student Research Competition (http://www.cs.ucdavis.edu/fse2016/calls/student-research-competition/).  The competition itself is in Seattle in November 2016, at the Foundations of Software Engineering conference.

The deadline to submit is July 1.  The only thing that is required by July 1 is a two-page paper describing your research.  You should be able to adapt other things you have written, though it will take some effort to do that adaptation.  I'm happy to help you with that.

Please let me know whether you are interested, so that we can plan your submission.

                    -Mike
")
))


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "PLSE lunch presentation")
(setq my-message
      "\
Could you sign up for a slot to speak in the PLSE lunch (http://uwplse.org/meet/lunch/) this quarter?

                    -Mike
")
))

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "UW PLSE retreat invitation, Sep 12-13 in Leavenworth, WA
cc: Amanda Robles <arobles@cs.washington.edu>")
(setq my-message
      "\
I would like to invite you to our inaugural PLSE Research Retreat! The retreat will take place on Sep. 12-13, 2016 at Icicle Village Resort in Leavenworth, WA. The retreat will emulate a workshop-style meeting.  Both guests, and members of the UW PLSE research group, will participate with short talks and intensive discussions. The goal is for all of us to leave the retreat with inspiration for new and expanded directions in our field.

Some of the potential projects that will be discussed can be found here: http://uwplse.org/#projects

Below my signature is a rough agenda for the two-day event.

I hope you are able to attend, and I am looking forward to your response!
Please let me know whether you can attend, no later than Friday July 8.

                    -Mike


Day One: Sept 12

11:00am Retreat Starts
12:00pm Lunch
1:00pm - 4:00pm Talks and Roundtable Discussions
4:00pm - 5:00pm Short talks from visitors
5:00pm - 8:00pm Outside BBQ  and fun activities
8:00pm  Cocktail Hour Discussions: Fun topics, Deep Insights and Far Projections

Day Two: Sept 13
8:00am Start
9:00am - 3:30pm Roundtable sessions and Talks
4:00pm Feedback from visitors
5:00pm End
")
))
"
Retreat invitations:
Second round invited:
 Tom Zimmermann <tzimmer@microsoft.com>
Second round possibilities:
 Alex Orso (Georgia Tech)
 Nenad Medvidovic <neno@usc.edu>,
 Kim Herzig <kim@cs.uni-sb.de>
 Michael Hind <hindm@US.IBM.COM>
Only one of Mary Lou and Barbara?
 Mary Lou Soffa (Virginia)
 Barbara Ryder (Virginia Tech)
 Frank Tip (Samsung)
YES:
 Jens Palsberg
 Michal Young
 Darko Marinov
 Wolfram Schulte <schulte@microsoft.com>
NO:
 Alex Aiken
 Matt Dwyer
"

;; This was ill-advised.  I should have just asked for a talk title.
(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "UW PLSE retreat talks")
(setq my-message
      "\
The PLSE retreat is coming up in about a month.  We need to decide on speakers.  There will be short slots of about 7+3 minutes and long slots of about 12+3 minutes.  Most students are expected to give a talk, on a project that is not already finished (where \"already finished\" means approximately that the camera-ready version of the paper has been accepted).  The goal is to talk about earlier-stage work so that you can get feedback and so that there are more interesting discussions.  Can you give a talk, and what length do you prefer?

                    -Mike
")
))
"
 Martin Kellogg <kelloggm@cs.washington.edu>
 Calvin Loncaric <loncaric@cs.washington.edu>
 Chandra Nandi <cnandi@cs.washington.edu>
 Spencer Pearson <suspense@cs.washington.edu>
 Konstantin Weitz <weitzkon@cs.washington.edu>
 Alberto Goffi <alberto.goffi@usi.ch>
 Ben Keller <bjkeller@cs.washington.edu>
"

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "FSE SRC paper")
(setq my-message
      "\
Congratulations again on your acceptance to the FSE 2016 Student Research Competition.  Before the competition, you will need to do three things.

First, you will revise your paper to address comments from the reviewer and possibly to incorporate improved results.  This is due in less than a month (on September 15), so you will want to get started soon.

Second, you will make a poster.  You will stand in front of this during the poster session, explaining your work to conference attendees.  There is some brief advice on creating a poster at https://homes.cs.washington.edu/~mernst/advice/write-poster.html .  You will also need to prepare and practice explaining the poster, so that you can do a good job during the conference.

Third, you will prepare and practice a talk.  The talks are quite short (perhaps about 3 minutes, but the organizers have not yet decided), but that may make them even harder to present.  You will need to focus on key ideas, so that you can motivate them, explain them, and demonstrate novelty in a few minutes.  I have reserved 3:30-4:30 on Wednesday, November 9 for practice talks to the PLSE group.  Before that, you will want to do one or more smaller practice talk to me and/or friends and colleagues.  At least a week before your first practice talk, please provide me with your slides so I can provide comments.  You can find some advice about giving a talk at https://homes.cs.washington.edu/~mernst/advice/giving-talk.html .

                    -Mike
")
))
;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury
"
 Spencer Pearson <suspense@cs.washington.edu>
 Calvin Loncaric <loncaric@cs.washington.edu>
 Martin Kellogg <kelloggm@cs.washington.edu>
 Joe Santino <jsantino1504@live.com>
 Waylon Huang <waylonh@cs.washington.edu>
 Chandra Nandi <cnandi@cs.washington.edu>
 Chris Mackie <mackic@cs.washington.edu>
"
;; also:  Wing Lam <winglam2@illinois.edu>



(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Support letter for a grant proposal to maintain Randoop")
(setq my-message
      "\
Would you be willing to write a letter of support for a grant proposal I am writing?  The grant proposal is to do maintenance of Randoop.  The money is not allowed to fund research, only to support the research of other people who use Randoop, and you are an example of such a person.

Examples of work done under the grant might be:
 * improve handling of Java generics and Java 8 features such as lambdas, method references, and changes to type inference
 * filter out flaky tests, such as ones caused by nondeterminism or state in the program under test
 * incorporate techniques proposed by others into the Randoop implementation (for example, techniques from the GRT tool)
 * integrate with external tools to help focus test generation
 * minimize tests, because Randoop often creates lots of long tests
 * feature requests from users
 * bug fixes, tutorials, new releases

A brief letter is adequate.  You would send me the letter electronically, and I would include it in my grant proposal.  I need the letter no later than January 10 (3 weeks from now).  Your letter could state the strengths and utility of Randoop, that it is a widely-used tool that is important to the research community, and that there is a need to support and improve it.  The letter should be addressed not to me but to the funders (NSF).

If you can't write a letter, I understand.  Just let me know, and thanks anyway.

                    -Mike
"))
)
;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury
"
 Lei Ma <malei@hit.edu.cn>
 Cyrille Artho <artho@kth.se>
 Alessandra Gorla <alessandra.gorla@imdea.org>
 Mauro Pezzè <mauro.pezze@usi.ch>
 Juan Pablo Galeotti <galeotti@st.cs.uni-saarland.de>
 Gordon Fraser <gordon.fraser@sheffield.ac.uk>
 Rui Abreu <rui.maranhao@gmail.com>
 Mark Harman <mark.harman@ucl.ac.uk>
 Paolo Tonella <tonella@fbk.eu>
 David Lo <davidlo@smu.edu.sg>
 Durga Prasad Mohapatra <durga@nitrkl.ac.in>
 GSVP Raju <ards2003@gmail.com>
 Wladmir C. Brandão <wladmir@pucminas.br>
 Mark Alan J. Song <song@pucminas.br>
 Horst Lichter <lichter@swc.rwth-aachen.de>
 Milos Gligoric <gligoric@utexas.edu>
 Gregg Rothermel <grother@cse.unl.edu>
 Zvonimir Rakamaric <zvonimir@cs.utah.edu>
 Daniel G. de Oliveira <dangogyn@gmail.com>
"



(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "PLSE research showcase followup")
(setq my-message
      "\
I enjoyed talking with you last Wednesday evening about applying natural language processing to problems in software engineering -- just one of many problems in PL and SE that I am interested in.

A number of people asked whether my work is written up.  An initial version of the work on translating Javadoc comments to assertions appeared in July (http://homes.cs.washington.edu/~mernst/pubs/exception-oracles-issta2016-abstract.html).  Next year, we plan to extend that work and to write a paper on translation from sentences to bash commands.  In the meanwhile, I gave an invited talk on my NLP work earlier this month, and you can find the slides from that talk in PowerPoint and PDF at http://homes.cs.washington.edu/~mernst/pubs/nlp-meets-se-201612.pptx and http://homes.cs.washington.edu/~mernst/pubs/nlp-meets-se-201612.pdf.

Please let me know if you have questions -- I'm always happy to talk about this research.

Thanks for the fun conversations!  I look forward to the next time we meet.

                    -Mike
"))
)
;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury
"
 Martin Bickeboeller <bickeboeller@yahoo.com>
 Vikram Chalana	<vikram.chalana@winshuttle.com>
 Magnus Christerson <magnus@intentional.com>,
 Shane Clifford	<shane@intentional.com>
 Brad Fitzpatrick <bradfitz@gmail.com>,
 Kate Fitzpatrick <kate@fitzpat.com>
 Kevin Frei <freik@fb.com>
 Don Hacherl <don@hacherl.org>
 Wolfram Schulte <wolfram.schulte@outlook.com>
 Ben Zorn <Ben.Zorn@microsoft.com>
"
;; Rob Short - Robs124@hotmail.com - Ed has a very close relationship with Rob and he's a member of the CSE Campaign Committee - so please double-check with Ed before you reach out to him. 



(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "TAing for CSE 331 in autumn")
(setq my-message-orig
      "\
I'm looking for TAs for CSE 331 in the autumn.
It's a fun, important class that gives TAs considerable leeway, and the chance to teach section.

Could you let me know whether you are potentially interested?

If so, I would like to meet with you to talk about the class, to help you decide whether to be involved.  I am also interested to hear your ideas for improving the class, and I can talk about mine (based on interviews with students and TAs).
My calendar is in Google Apps as mernst@cs.washington.edu or at http://homes.cs.washington.edu/~mernst/calendar.html .

                     Thanks,

                    -Mike
")
(setq my-message-second-pass
      "\
I'm looking for TAs for CSE 331 in the autumn.
It's a fun, important class that gives TAs considerable leeway, and the chance to teach section.

Could you let me know whether you are potentially interested?

If so, I would like to talk with you about the class.  I can answer questions to help you decide whether to be involved.  I am also interested to hear your ideas for improving the class, and I can talk about mine (based on interviews with students and TAs).
It may be too late to arrange an in-person meeting, but in that case perhaps we could arrange a phone or video call.
My calendar is in Google Apps as mernst@cs.washington.edu or at http://homes.cs.washington.edu/~mernst/calendar.html .

                     Thanks,

                    -Mike
")
(setq my-message my-message-second-pass))
)
;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury
"
 Alexander Davis <ahd2112@uw.edu>
TO SCHEDULE Alexey Beall <beallal@cs.washington.edu>
YES: Andrew Tran <atran35@uw.edu>
NO, TOO BUSY: Armaan Sood <armaan1@cs.washington.edu>
 Belinda Li <lib49@cs.washington.edu>
MAYBE, DEPENDS ON BS/MS application.  I have heard good things about him.  He made the Android final assignment: Bryan Van Draanen <bryanvd@cs.washington.edu>
 Chen (Jason) Qiu <chenq9@cs.washington.edu>
NO, GRADUATING: Christine Ta <cta95@uw.edu>
 Cody Kesting <kesting@cs.washington.edu>
 Daniel Merken <dcm58@cs.washington.edu>
 David Dupre <djdupre@uw.edu>
NO, too busy: Haiqiao Chen <chenh43@cs.washington.edu>
 Hannah Werbel <hwerbel@uw.edu>
MEETING: Hongtao Huang <hongth@cs.washington.edu>
MEETING: Jacob Murphy <jkmurphy@cs.washington.edu>
NO: graduating Jacob Ward <jward23@uw.edu>
 Jake Sippy <jsippy@cs.washington.edu>
NO: Kevin does not recommend: Josh Katz
NO: not interested Julius Christenson <juliusc@uw.edu>
TO MEET: Kaushal Mangipudi <kaushalm@cs.washington.edu>
 Laura Vonessen <laurav4@cs.washington.edu>
NO: Zach disrecommends, and I got a bad impression from our meeting: Leah Perlmutter <lrperlmu@cs.washington.edu>
 Lian Yu <liany2@cs.washington.edu>
 Lucy Zhu <lucyzhu@cs.washington.edu>
 Martin Kellogg [DONT ASK YET]
NO, on internship: Natalie Fetsch <nfetsch@cs.washington.edu>
NO: graduating:  Nate Yazdani
NO, MIGHT BE ON INTERNSHIP IN FALL: Sarah Yu <sarahyu@cs.washington.edu>
 Su Ye <yes23@cs.washington.edu> [first name Ye??]
Zach has concerns about him: Tim Chirananthavat <theme@cs.washington.edu> 
NO: doesn't need a TAship: Vincent Liew <vliew@cs.washington.edu>
NO: graduating Waylon Huang
 Yifan (Vanadis) Xu <xuyf@cs.washington.edu>
 Yiyang Xu <xu517@cs.washington.edu>
NO: too busy Zhu (Ruby) Li <liz67@cs.washington.edu>
 Zongyuan Chen <zongyc@cs.washington.edu>
"
"
NO: graduating YiYang (Amy) Xu <xu517@uw.edu>
 Weifan Jiang <wfjiang@cs.washington.edu>
"
"
NO: graduating: Garrett Marconet <gmarco@cs.washington.edu>
NO: graduating: Adam Geller (atgeller)
MAYBE: will know by mid-June about internship: Revi Cheng <rlcheng@cs.washington.edu>
NO: graduating Chris Mackie
NO: TAing 312 Satvik Agarwal <satvik22@cs.washington.edu>
NO, GRADUATED: Zhitao (Reid) Zhang <zzt124@cs.washington.edu>
MEETING: Avidant Bhagat <avidant@cs.washington.edu>
"
;; TO DO:
"
 Anita Leung <leungak@cs.washington.edu>
 Haley Ruth <haleymr@cs.washington.edu>
 Josh Pollock <joshpoll@cs.washington.edu>
 Nicholas Fox <foxn2@cs.washington.edu>
 Michael James Hart <hartmj99@cs.washington.edu>
 Bailee Barrick <baileeb@cs.washington.edu>
 Sasha Krassovsky <avklezo@cs.washington.edu>
 Mohammad Kayali <kayali@cs.washington.edu>
NO: is TAing CSE 401: Aaron Johnston <aaronj1@cs.washington.edu>
NO: is TAing CSE 143: Tyler Mi <gartymi@cs.washington.edu>
 Kexuan Liu <kx31@cs.washington.edu>
"


;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury

"
 Tyler Waters <tylerrw@cs.washington.edu>
 Wei Wu <wuwei33@cs.washington.edu>
 Allie Pfleger <pflegera@cs.washington.edu>
 Camila Christensen <camilyo@cs.washington.edu>
 Mike Harris <micha06@cs.washington.edu>
 Apollo Zhu <zhuzhiyu@cs.washington.edu>
 Caroline Freer <cfreer@cs.washington.edu>
 Jacob Christy <jacobc03@cs.washington.edu>
 Esau Abraham <freddy12@cs.washington.edu>
 Simona Liao <zliao2@cs.washington.edu>
 Emily Chang <echang9@cs.washington.edu>
 Elliott Zackrone <ezackr@cs.washington.edu>
 Bowen Tian <bowent2@cs.washington.edu>
 Joshua Peterson <jpete4@cs.washington.edu>
 Anish Konanki <skonanki@cs.washington.edu>
 Wenxuan Liu <lwx2000@cs.washington.edu>
 Joan Kwo <qkwo@cs.washington.edu>
 Jun Hu <jxhu@cs.washington.edu>
 Wenxin Zhang <wzhang8@cs.washington.edu>
 Jacob Lee <jongwon1@cs.washington.edu>
"

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "TAing for CSE 403 in winter or spring")
(setq my-message

      "\
Congratulations for doing very well in CSE 403 in the spring!

I am writing to ask you to think about both teaching and research, since I suspect you would do well at both.

CSE 403 is looking for TAs.  Guiding students in their independent projects is more challenging and rewarding than most TA positions.  CSE 403 is being taught in both winter 2023 and spring 2023.

I suggest you also consider getting involved in research, either this quarter or in the future.  Research is an excellent way to enrich your education -- it's fun, and you'll get experiences that you can't obtain any other way.

If you already have a teaching or research position, that's great!  If not, or if you are looking for a change, I would be happy to talk with you.

Could you please let me know whether you are potentially interested?

                    -Mike


PS: Here are some potential research projects where I am looking for help:
 * Create a GitHub pull request UI that selectively views only a subset of changes that are related to one another.  This lets a developer examine the pull request in parts.
 * Detect errors in mixing signed and unsigned integers.  For example, division or modulus with an unsigned operand yields a meaningless result; likewise for an arithmetic comparison (<, <=, >, >1) when the two arguments have different signedness.
 * Improve test generation tools that automatically create unit tests for a program.  For example, the improvement can use side effect analysis, measurements of coverage and run time, and constants in the program under test.
 * Create tools to detect security vulnerabilities (SQL injection, cross-site scripting, etc.) by customizing a general taint analysis.
"
))
)


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Pavel Panchekha: potential faculty applicant")
(setq my-message
      "\
My student Pavel Panchekha (https://pavpanchekha.com/) is on the academic job market.  He plans to graduate in June 2019.  If you are open to hiring in programming languages, I encourage you to recruit him, because he is first-rate.  For example, he received a distinguished paper award at PLDI 2015 and has two papers at PLDI 2018.  He has expressed interest and may contact you, but you are likely to have lots of competition!

                    -Mike
"))
)
;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury
"
 David Stotts <stotts@cs.unc.edu>
 Gregg Rothermel <grother@cse.unl.edu>
 Alex Orso <orso@cc.gatech.edu>
 Trent Jaeger <tjaeger@cse.psu.edu>
 Tien N. Nguyen <tien@iastate.edu>
 Nancy Amato <amato@cse.tamu.edu>
 Mary Lou Soffa <soffa@virginia.edu>
"

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "CSE 331 HW1")
(setq my-message
      "\
I noticed that you didn't yet submit HW1 for CSE 331.

If you are having trouble, please don't get discouraged, but ask the staff for help.  We are convinced that every student in the class can succeed, and we are motivated to make that happen.

You have 4 late days and can use one of them on HW1, so you still have time to complete the assignment.  Even if you can't complete the assignment, please submit what you can, by the deadline.

                    -Mike
"))
)
;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury
"
 xiaoyixu@cs.washington.edu
 shenw3@cs.washington.edu
 kevinte@cs.washington.edu
 kelaita@cs.washington.edu
 zsw2017@cs.washington.edu
 djtides@cs.washington.edu
 katyasyc@cs.washington.edu
 jwang98@cs.washington.edu
 botonz@cs.washington.edu
 zhangxru@cs.washington.edu
 yfbai@cs.washington.edu
 yaoc2@cs.washington.edu
 wassaman@cs.washington.edu
 moocj@cs.washington.edu
 samstein@cs.washington.edu
 sjf91@cs.washington.edu
 jl9985@cs.washington.edu
 iancup@cs.washington.edu
 pieterb@cs.washington.edu
 natal@cs.washington.edu
 kspecht3@cs.washington.edu
 poulainm@cs.washington.edu
 chowc3@cs.washington.edu
 wajdib@cs.washington.edu
 mck35@cs.washington.edu
 yunyub@cs.washington.edu
 pludwig@cs.washington.edu
 jouvemax@cs.washington.edu
 nkwacker@cs.washington.edu
 sdelargy@cs.washington.edu
 hemil@cs.washington.edu
 linnan02@cs.washington.edu
"


(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert)
(setq email-address-group-comma-concatenates nil)
(setq my-subject "CSE 403 estimated grade")
(setq my-message
      "\
Here is an estimate of your current grade in CSE 403:  .
Your future work could affect this grade, leading to a higher or lower grade.

                    -Mike
"))
)
;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury
"
 johnbar@cs.washington.edu
 debruin@cs.washington.edu
 hangbui@cs.washington.edu
 bycao96@cs.washington.edu
 bencelsi@cs.washington.edu
 jedc101@cs.washington.edu
 pxc2@cs.washington.edu
 kamdenk@cs.washington.edu
 theme@cs.washington.edu
 cuiyuxi@cs.washington.edu
 gerar231@cs.washington.edu
 xiaoyg2@cs.washington.edu
 geselc@cs.washington.edu
 ericguo@cs.washington.edu
 ofeki@cs.washington.edu
 kbbj@cs.washington.edu
 robertk3@cs.washington.edu
 liur26@cs.washington.edu
 vloyko@cs.washington.edu
 kaushalm@cs.washington.edu
 yichum@cs.washington.edu
 pujaram@cs.washington.edu
 alyssr3@cs.washington.edu
 dthien@cs.washington.edu
 thuynt@cs.washington.edu
 wahleric@cs.washington.edu
 wang2667@cs.washington.edu
 wilkc@cs.washington.edu
 lzha@cs.washington.edu
 grz2@cs.washington.edu
 zigmanra@cs.washington.edu
"


;;;;;;;;;;;;;;;;

;; With point here, repeatedly call:  M-x compose-mail-address-buffer-and-bury

"
George Mason				
 David Rosenblum <dsr@gmu.edu>,
 Jeff Offutt <offutt@ise.gmu.edu>,
 Thomas LaToza <tlatoza@gmu.edu>,
 Paul Ammann <pammann@gmu.edu>,
 Brittany Jonhnson-Matthews <johnsonb@gmu.edu>

CMU ISR		
https://www.isri.cmu.edu/jobs/tenure-track.html
 Jonathan Aldrich <jonathan.aldrich@cs.cmu.edu>

VT
 Stephen Edwards <edwards@cs.vt.edu>,
 Eli Tilevich <tilevich@cs.vt.edu>

UVA
 Matt Dwyer <matthewbdwyer@virginia.edu>,
 Sebastian Elbaum <selbaum@virginia.edu>,
 Dave Evans <evans@cs.virginia.edu>,
 Kevin Sullivan <sullivan@cs.virginia.edu>,
 Mary Lou Soffa <soffa@virginia.edu>

NYU
 Mike Walfish <mwalfish@cs.nyu.edu>,
 Thomas Wies <wies@cs.nyu.edu>,

Rutgers
 Santosh Nagarakatte <santosh.nagarakatte@rutgers.edu>

Penn
 Mayur Naik <mhnaik@seas.upenn.edu>,

Stevens Institute of Tech
 David Naumann <naumann@cs.stevens.edu>,
 Dominic Duggan <dduggan@cs.stevens.edu>

UNC				
 David Stotts <stotts@cs.unc.edu>

NCSU
 Gregg Rothermel <gregg.e.rothermel@gmail.com>,
 Laurie Williams <williams@csc.ncsu.edu>,
 Chris Parnin <chris.parnin@gmail.com>,
 Tim Menzies <tim.menzies@gmail.com>,
 Katie Stolee <ktstolee@ncsu.edu>,

Pitt
 Bruce R. Childers <childers@cs.pitt.edu>

Cornell Tech
 Andrew Myers <andru@cs.cornell.edu>,
 Nate Foster <jnfoster@cs.cornell.edu>,
 Greg Morrisett <greg.morrisett@cornell.edu>,
 Adrian Sampson <asampson@cs.cornell.edu>

UMD (not hiring)
 Adam Porter <aporter@cs.umd.edu>,
 Mike Hicks <mwh@cs.umd.edu>,
 David Van Horn <dvanhorn@cs.umd.edu>,
 Jeff Hollingsworth <hollings@cs.umd.edu>,
 Neil Spring <nspring@cs.umd.edu>

Drexel
 Colin Gordon <csgordon@cs.drexel.edu>,
 Yuanfang Cai <yfcai@cs.drexel.edu>

Columbia
 Jeannette Wing <wing@columbia.edu>,
 Gail Kaiser <kaiser@cs.columbia.edu>,
 Baishakhi Ray <rayb@cs.columbia.edu>,

William and Mary
 Denys Poshyvanyk <denys@cs.wm.edu>

Tennessee
 Audris Mockus <audris@utk.edu>
"

(if nil (progn
(setq compose-mail-function 'compose-mail-and-insert-with-firstname)
(setq email-address-group-comma-concatenates t)
(setq my-subject "Martin Kellogg's faculty application")
(setq my-message
      "\
My student Martin Kellogg (https://homes.cs.washington.edu/~kelloggm/) has applied for a tenure-track faculty job.  His research is in software engineering, on the program analysis and programming languages end of the field.

I wanted to make sure that his application does not fall through the cracks.  I think he would be a great hire for you.

I'm happy to chat with you if that would be helpful.

Good luck with faculty hiring this year!

                    -Mike
"))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End
;;;


(provide 'bulk-mail)

;;; bulk-mail.el ends here
