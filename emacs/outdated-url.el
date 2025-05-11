;;; -*- lexical-binding: t -*-

;; This file contains functions that send mail to people asking them to
;; update URLs on webpages that they maintain.

;; Can also do a google query:  link:www.google.com

(require 'util-mde)			; for replace-regexp-noninteractive
(eval-when-compile
  (require 'sendmail)
  (require 'cl-lib)				; for cl-second
  )

;; To process Alta Vista results from a
;;   link:"http://foo.bar.baz/quux"
;; query:

(defun simplify-altavista-results ()
  (interactive)
  (goto-char (point-min))
  (delete-non-matching-lines "<dl><dt><b>")
  (goto-char (point-min))
  (replace-string-noninteractive "</font><font face=helvetica size=-1>" "")
  (goto-char (point-min))
  (replace-regexp-noninteractive "^.*<a href=\"\\(.*\\)\"><b>.*$" "\\1" nil)
  (sort-lines nil (point-min) (point-max)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Descriptors
;;;

;; A Descriptor:
;;   from-address
;;   long-description
;;   short-description
;;   old-url
;;   new-url

;;;
;;; My homepage
;;;

(defvar mde-csail-people-descriptor-1
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "people.csail.mit.edu/people/mernst/"
    "people.csail.mit.edu/mernst/"))

(defvar mde-csail-people-descriptor-2
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "people.csail.mit.edu/~mernst/"
    "people.csail.mit.edu/mernst/"))

(defvar mde-csail-tilde-descriptor
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "csail.mit.edu/~mernst/"
    "people.csail.mit.edu/mernst/"))

(defvar mde-www-pag-descriptor
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "www.pag.csail.mit.edu/~mernst/"
    "people.csail.mit.edu/mernst/"))

(defvar mde-pag-lcs-descriptor
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "pag.lcs.mit.edu/~mernst/"
    "people.csail.mit.edu/mernst/"))


(defvar mde-msr-descriptor
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "http://www.research.microsoft.com/research/analysts/mernst/"
    "http://sdg.lcs.mit.edu/~mernst/"))

(defvar mde-rice-descriptor
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "http://www.cs.rice.edu/~mernst/"
    "http://sdg.lcs.mit.edu/~mernst/"))

(defvar mde-theory-descriptor
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "http://theory.lcs.mit.edu/~mernst/"
    "http://sdg.lcs.mit.edu/~mernst/"))

(defvar mde-washington-descriptor
  '("Michael Ernst <mernst@csail.mit.edu>"
    "my home page"
    "my home page"
    "http://homes.cs.washington.edu/~mernst/"
    "http://sdg.lcs.mit.edu/~mernst/"))

;;;
;;; Other descriptors (not my homepage)
;;;

(defvar fair-theory-descriptor
  '("Michael Ernst <mernst@theory.lcs.mit.edu>"
    "FAIR -- Fairness and Accuracy in Reporting"
    "FAIR"
    "http://theory.lcs.mit.edu/~mernst/fair/"
    "http://www.fair.org/"))

(defvar fair-subdir-descriptor
  '("Michael Ernst <mernst@theory.lcs.mit.edu>"
    "FAIR -- Fairness and Accuracy in Reporting"
    "FAIR"
    "http://www.fair.org/fair/"
    "http://www.fair.org/"))


(defvar cf-web-ai-descriptor
  '("Michael Ernst <mernst@cf-web.org>"
    "CF-WEB -- Online Information About Cystic Fibrosis"
    "CF-WEB"
    "http://www.ai.mit.edu/people/mernst/cf/"
    "http://www.cf-web.org/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compose-url-mail (email urls descriptor)
  (let ((from-address (cl-first descriptor))
	(long-description (cl-second descriptor))
	(short-description (cl-third descriptor))
	(old-url (cl-fourth descriptor))
	(new-url (cl-fifth descriptor)))
    (compose-mail)
    (insert email)
    (insert "\nFrom: " from-address)
    (let ((multiple-urls (string-match "\n" urls)))
      (forward-line 1)
      (end-of-line)
      (if multiple-urls
	  (insert "Outdated URLs")
	(insert "Outdated URL at " urls))
      (forward-line 1)
      (if (looking-at "^BCC:")
	  (delete-region (point) (progn (forward-line 1) (point))))
      (goto-char (point-max))
      (insert "Dear webmaster,

" (if multiple-urls "Your WWW pages" "Your WWW page") "

  " urls "

" (if multiple-urls "contain links" "contains a link") " to " long-description ".
Thank you for this link.  However, you link to an old URL;
" short-description " has moved.  Please update your files:

  OLD: " old-url "
  NEW: " new-url "

				 Thanks,

				-" from-address))))

;; Testing
;; (compose-url-mail
;;  "mernst@theory.lcs.mit.edu"
;;     "http://www.montana.com/people/butte/fredw/
;;   http://www.montana.com/people/home1/butte/fredw/www/
;;   http://www.montana.com/people/home1/butte/fredw/www/index.htm")
;; (apply 'compose-url-mail (list "mernst@cf-web.org" "http://some.random.url/directory/file.html" mde-msr-descriptor))
;; (apply 'compose-url-mail (list "mernst@cf-web.org" "http://some.random.url/directory/file.html\nhttp://another.random.url/" cf-web-ai-descriptor))

(defun send-url-mail (email-and-url-list descriptor)
  (while email-and-url-list
    (let ((this-email-and-url (car email-and-url-list)))
      (setq email-and-url-list (cdr email-and-url-list))
      (compose-url-mail (cl-first this-email-and-url)
			(cl-second this-email-and-url)
			descriptor)
      (sit-for 2)
      (let ((mail-send-hook nil))
	(mail-send)))))

;; Generic form
;; (send-url-mail email-and-url descriptor)

;; Specific form
;; (send-url-mail email-and-url-cf-web-ai cf-web-ai-descriptor)
;; (send-url-mail email-and-url-fair-theory fair-theory-descriptor)
;; (send-url-mail email-and-url-mde-rice mde-rice-descriptor)
;; (send-url-mail email-and-url-msr mde-msr-descriptor)
;; (send-url-mail email-and-url-mde-theory mde-theory-descriptor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; link:"http://www.cs.rice.edu/~mernst/"

;; Sent January 23, 1998
(setq email-and-url-mde-rice
  '(("jis@juggling.org"
     "http://www.juggling.org/home/List.html")
    ("none (bashford@l12.informatik.uni-dortmund.de: host not found)"
     "http://ls12-www.informatik.uni-dortmund.de/~bashford/bookmarks.html")
    ("breadbox@muppetlabs.com"
     "http://www.muppetlabs.com/~breadbox/intercal-man/s01.html")))

;; Sent December 16, 1998
(setq email-and-url-mde-rice-19981216
 '(("ernesto@lcc.uma.es"
    "http://apolo.lcc.uma.es/~ernesto/conferences/index.html
  http://www.lcc.uma.es/ernesto/conferences/")
   ("kuchen@uni-muenster.de"
    "http://danae.uni-muenster.de/lehre/kuchen/FG214/index.html")
   ("nickie@softlab.ntua.gr"
    "http://softlab.ece.ntua.gr/local/CFP/")
   ("ley@uni-trier.de"
    "http://sunsite.informatik.rwth-aachen.de/dblp/db/indices/a-tree/e/Ernst:Michael.html")
   ("heekwak@saul.cis.upenn.edu"
    "http://www.cis.upenn.edu/~heekwak/research/cs-people.html")
   ("tjim@saul.cis.upenn.edu"
    "http://www.cis.upenn.edu/~tjim/cs-people.html")
   ("jis@juggling.org"
    "http://www.juggling.org/home/List.html")
   ("none (bashford@l12.informatik.uni-dortmund.de: host not found)"
    "http://ls12-www.informatik.uni-dortmund.de/~bashford/bookmarks.html")
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sent January 23, 1999
(setq email-and-url-fair-theory
  '(("none (user best@Chelsea-Mi.Net does not exist)"
     "http://152.160.206.1/urls/url5.htm")
    ("pec@superlink.net"
     "http://itre.ncsu.edu/radio/srcaf3.html")
    ("mworld@ids.net"
     "http://musiciansworld.com/Books.htm")
    ("none (user spk@ibm.net does not exist)"
     "http://orion.oac.uci.edu/~spk/media.html")
    ("wilt@rt66.com"
     "http://rt66.com/twl/Digital_Daily.html")
    ("webmaster@sddt.com"
     "http://sddt.com/files/library/journals.html")
    ("none (mikescot@ix.netcom.com: user unknown)"
     "http://sensemedia.net:8080/people/scott/027.htm")
    ("laux@ecn.purdue.edu"
     "http://widget.ecn.purdue.edu/~laux/NAVIGATOR/NetworkNavigator.html")
    ("tojek@cae.wisc.edu"
     "http://www.cae.wisc.edu/~tojek/linkinfo.html")
    ("chrish@cs.albany.edu"
     "http://www.cs.albany.edu/~chrish/lists/bookmarks.html")
    ("jimu@cs.su.oz.au"
     "http://www.cs.su.oz.au/~jimu/bookmarks.html")
    ("mparacha@nova.gmi.edu"
     "http://www.gmi.edu/~mparacha/magpage.html")
    ("lhuddles@unt.edu"
     "http://www.hist.unt.edu/09w-blk4.htm")
    ("lewis@inx.net"
     "http://www.inx.net/~lewis/subtest.htm")
    ("krishna@mit.edu"
     "http://www.mit.edu/~govinda/links.html")
    ("dmt@pairoducks.com"
     "http://www.pairoducks.com/local/local.html")
    ("murad@xnet.com"
     "http://www.xnet.com/~murad/mag.html")))

;; Sent December 9, 1998
(setq email-and-url-fair-theory-19981209
  '(
    ("RowanF@Conjure.com"
     "http://www.conjure.com/activism.html")
    ("RowanF@rsv.ricoh.com"
     "http://www.crc.ricoh.com/people/rowanf/library.html")
    ("Ryan.Harris@alltel.com"
     "http://cavern.uark.edu/depts/comminfo/www/news.html
  http://www.americancomm.org/~aca/studies/news.html")
    ("adavenpo+@snurgle.org"
     "http://www.snurgle.org/~adavenpo/www-old/pages/links.html")
    ("barzilai@digital-web.net"
     "http://chumbly.math.missouri.edu/htmlized/media/pbs.anti-labor.html")
    ("none (user best@Chelsea-Mi.Net does not exist)"
     "http://152.160.206.1/urls/url5.htm")
    ("chrish@cs.albany.edu"
     "http://www.cs.albany.edu/~chrish/lists/bookmarks.html")
    ("chuck0@flag.blackened.net"
     "http://flag.blackened.net/chuck0/home/morestuff.html")
    ("dmt@pairoducks.com"
     "http://www.pairoducks.com/local/local.html")
    ("dpwe@media.mit.edu"
     "http://dpwe.www.media.mit.edu/~dpwe/hotlinks.html")
    ("none (dshirley@cyber1.servtech.com: user unknown)"
     "http://www.servtech.com/~chickade/dshirley/")
    ("dshivers@hevanet.com"
     "http://www.hevanet.com/dshivers/5.end.html")
    ("eucyb@wxs.nl"
     "http://ourworld.compuserve.com/homepages/eucyb/ref.htm")
    ("greenwood_t@fortlewis.edu"
     "http://library.fortlewis.edu/~instruct/resource/massmed.htm")
    ("hrpatton@mail.vt.edu"
     "http://128.173.16.28:10021/artsci/english/patton/")
    ("imcomp@tm.net.my"
     "http://www.geocities.com/TheTropics/5802/magazin.html")
    ("justin@cyborgasmic.com"
     "http://www.links.net/www/news.html")
    ("none (unknown user kellow@cs.uregina.ca)"
     "http://leroy.cc.uregina.ca/~rasmussk/journals.html")
    ("none (kramsey@nwu.edu: user unknown)"
     "http://www.studorg.nwu.edu/seed/EnviroInfo.html")
    ("krishna@mit.edu"
     "http://www.mit.edu/~govinda/links.html")
    ("laux@ecn.purdue.edu"
     "http://widget.ecn.purdue.edu/~laux/NAVIGATOR/NetworkNavigator.html")
    ("lewis@inx.net"
     "http://www.inx.net/~lewis/subtest.htm")
    ("lfenster@tezcat.com"
     "http://www.tezcat.com/~lfenster/links/politic.html")
    ("lhuddles@unt.edu"
     "http://www.hist.unt.edu/09w-blk4.htm")
    ("liana@bga.com"
     "http://www.realtime.com/~liana/bookmark.html")
    ("mapplin@selu.edu"
     "http://www.selu.edu/Academics/Depts/CommThea/News.html")
    ("none (mikescot@ix.netcom.com: user unknown)"
     "http://pmwww.cs.vu.nl/home/fmdvries/scotlinks/027.html
  http://sensemedia.net:8080/people/scott/027.htm")
    ("mmac@students.si.fct.unl.pt"
     "http://students.si.fct.unl.pt/users/mmac/slistf.htm")
    ("mparacha@nova.gmi.edu"
     "http://www.gmi.edu/~mparacha/magpage.html")
    ("mschiang@ms1.hinet.net"
     "http://peter.ee.nctu.edu.tw/~cls/faculty/cms/library/lbr000/000idx.htm")
    ("murad@xnet.com"
     "http://www.xnet.com/~murad/mag.html")
    ("mworld@ids.net"
     "http://musiciansworld.com/Books.htm")
    ("newman@garnet.berkeley.edu"
     "http://garnet.berkeley.edu:3333/.mags/.mags.html")
    ("none (unknown user nmenad@bgnet.bgsu.edu)"
     "http://www-math.bgsu.edu/~menad/msi/external.html")
    ("pec@superlink.net"
     "http://itre.ncsu.edu/radio/srcaf3.html")
    ("spk@uci.edu"
     "http://orion.oac.uci.edu/~spk/media.html")
    ("support@fortune.org"
     "http://www.fortune.org/wierd.shtml")
    ("tojek@cae.wisc.edu"
     "http://www.cae.wisc.edu/~tojek/linkinfo.html")
    ("none (webmaster@dasewaside.com: host not found)"
     "http://www.vibe.com/vibe/dasewaside/politics/docs/sites.html")
    ("webmaster@deutsch.de"
     "http://www3.deutsch.de/scotts-server-list/027.html")
    ("webmaster@leftnet.net"
     "http://207.10.38.2/leftylinks.htm")
    ("webmaster@netmation.com"
     "http://www.netmation.com/www/usnytl.htm")
    ("wilt@rt66.com"
     "http://rt66.com/twl/Digital_Daily.html")
    ))

;; As of December 9, 1998, couldn't find these pages to update them.
(setq email-and-url-fair-theory-no-email-19981209
  '(
    ("none (appears stale, automatically generated anyway)"
     "http://www.jca.ax.apc.org/~yk/volunteer/InfoSeek-APC-6.html")
    ("none (can't find email address)"
     "http://www.geocities.com/SoHo/Cafe/4934/karibik.htm")
    ("none (can't find email)"
     "http://www.multicom.net/catalogs/massmed.htm")
    ("none (couldn't find email address)"
     "http://www.ncsa.uiuc.edu/people/jlayton/interest.htm")
    ("none (didn't wait for an answer)"
     "http://www3.dp.doe.gov/resources.media.html")
    ("none (netscape crashes)"
     "http://sddt.com/files/library/journals.html")
    ("none (page disappeared)"
     "http://www.mxl.cetys.mx/~neto/")
    ("none (page has moved)"
     "http://infoserver.etl.vt.edu/coe/COE_courses/edci_5774_spr95/David/monster.html")
    ("none: not found on server"
     "http://www.access.digex.net/~jcollins/comedy.html")
    ("none: page move notification dangles"
     "http://www.cs.su.oz.au/~jimu/bookmarks.html")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; link:"http://theory.lcs.mit.edu/~mernst" -link:"http://theory.lcs.mit.edu/~mernst/media" -link:"http://theory.lcs.mit.edu/~mernst/fair"

;; Sent January 23, 1999
(setq email-and-url-mde-theory
  '(("raul@gsyc.inf.uc3m.es"
     "http://www.gsyc.inf.uc3m.es/~raul/configuration.html")
    ("Peter.Braun@iaxp12.inf.uni-jena.de"
     "http://www.minet.uni-jena.de/~braunpet/infobody.htm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; link:"http://www.ai.mit.edu/people/mernst/cf"

;; Sent January 27, 1999
(setq email-and-url-cf-web-ai
 '(("AmaLibrary@ama-assn.org"
    "http://www.ama-assn.org/med_link/willness.htm")
   ("Karl-Josef.Ziegler@mzb.uni-magdeburg.de"
    "http://www.med.uni-magdeburg.de/mzb/intadr.htm")
   ("none (QV2373_116@Apollo.Commnet.edu: no such user)"
    "http://www.qvctc.commnet.edu/QVCTC/student/KimCurtin/cf.html")
   ("RadSurfer@yahoo.com"
    "http://members.tripod.com/~RadSurfer/useful.html")
   ("StewartPub@aol.com"
    "http://www.interactive-healthcare.com/internet/F1.htm")
   ("alfnat@ozemail.com.au"
    "http://www.lungnet.org.au/links.html")
   ("bzlab@imgate.wustl.edu"
    "http://www.surgery.wustl.edu/bjcmdl/medgen.htm")
   ("cfnj@cfusa.org"
    "http://www.cfusa.org/nj/links.html")
   ("cityspectrum@cityspectrum.com"
    "http://www.cityspectrum.com/healthl.htm")
   ("crsullivan@ucdavis.edu"
    "http://www.geocities.com/CollegePark/Library/6201/ytweety.html")
   ("dnonline@idir.net"
    "http://www.comedserv.com/dyslexia.htm")
   ("duncan@cerf.net"
    "http://www.neonatology.org/neo.links.parents.html
  http://www.neonatology.org/syllabus/syllabus.html")
   ("eaf@networx.on.ca"
    "http://www.freenet.hamilton.on.ca/Information/associations/hdef/general.html")
   ("genetica@famema.br"
    "http://www.famema.br/genetica/welcome.html")
   ("gs@sime.com"
    "http://www.expo.org/intermed/pulmo.htm")
   ("none (heatonj@fcbe.edu.on.ca: host unknown)"
    "http://www.icactive.com/jwz/VerC.htm")
   ("info@doctorsnet.com"
    "http://doctorsnet.com/MedLinks/Illness.htm")
   ("jackl@caos.kun.nl"
    "http://www-gdb.caos.kun.nl/gdb-bin/genera/genera/hgd/DBObject/GDB:120584")
   ("jaeckel@medizin-forum.de"
    "http://www.medizin-forum.de/schebo/engl1.htm")
   ("jcarr@mrbit.es"
    "http://www.mrbit.es/~jcarr/links/megaweb/6500b_m2.htm")
   ("jchfavre@scopus.ch"
    "http://ourworld.compuserve.com/homepages/JCh_FAVRE/medliens.htm")
   ("jdv@metamatrix.com"
    "http://www4.pgh.net/~jdv/cf.htm")
   ("jjones@utmb.edu"
    "http://www.utmb.edu/pcs/links.htm")
   ("none (jsabbott@pacbell.net: user unknown)"
    "http://www.asf.org/lunglinks.html
  http://www.asf.org/pedtxlinks.html")
   ("jsinger@kettering.edu"
    "http://www.gmi.edu/official/acad/mech-eng/homschol.htm")
   ("jwilkins@adp-ebanking.com"
    "http://www.geocities.com/Heartland/Plains/5729/links.html")
   ("malumm01@mcrcr.med.nyu.edu"
    "http://mcpth10.med.nyu.edu/Malumbres/bookmark.htm")
   ("med_res@lfmotol.cuni.cz"
    "http://www.lf2.cuni.cz/projekty/med_res/c4-0.htm
  http://www.lf2.cuni.cz/projekty/med_res/p4-0.htm")
   ("medlib@jaguar1.usouthal.edu"
    "http://southmed.usouthal.edu/library/www/cyst.htm")
   ("medlibem@hkuxa.hku.hk"
    "http://www.hku.hk/lib/medlib/docs/subject.html")
   ("mortgage@warwick.net"
    "http://uscj.org/metny/middletown/kosher.htm")
   ("office@phone-soft.com"
    "http://www.phone-soft.com/at/cyber-world/international/o0342i.htm")
   ("pessoa@dcc.ufmg.br"
    "http://www.dcc.ufmg.br/~pessoa/tese/q21-01.htm")
   ("polreply1@pol.net"
    "http://home.po.com/html/hpol/springboard/alpha_associations.shtml")
   ("r_baird@clinch.edu"
    "http://pluto.clinch.edu/home/jrb/public_html/melissa.html")
   ("rthill@ozemail.com.au"
    "http://www.ozemail.com.au/~rthill/")
   ("sew7490@rit.edu"
    "http://www.rit.edu/~sew7490/patriot.html")
   ("spcran@acs.ucalgary.ca"
    "http://rehab.educ.ucalgary.ca/courses/edis/551.58/Respiratory/cystic-overview.html")
   ("telltdo@tdo.infi.net"
    "http://vh1421.infi.net/local/links/medweb.htm")
   ("none (tjuly@umich.edu forwards to bad address)"
    "http://www.lakeville.k12.mi.us/cc/moreabou.htm")
   ("tjw@cybertap.com"
    "http://www.cybertap.com/sunrise/mil.htm")
   ("none (trc@fox.nstn.ca no longer controls that site)"
    "http://www.valleyweb.com/krrc/distext.html")
   ("tsuits@midamer.net"
    "http://www.midamer.net/space/health.html")
   ("twood@indian.vinu.edu"
    "http://twood.vinu.edu/Sites/sites.htm")
   ("vgrace@uws.org"
    "http://www.uws.org/agencies.html")
   ("vividhue@rpi.edu"
    "http://www.rpi.edu/~lalibr/ten.html")
   ("vneedham@phys.ksu.edu"
    "http://www.phys.ksu.edu/area/jrm/john.html")
   ("web@odrge.odr.georgetown.edu"
    "http://macpost.odr.georgetown.edu/GrantsandContracts/GrantsandContracts.html")
   ("webm@nhfirst.com"
    "http://web0.netis.com/nhfirst/kclimb/kccfpage.htm")
   ("none (webmaster@asf.org: expands to bad address)"
    "http://livertx.org/cysticlinks.html")
   ("none (webmaster@cfcare.com: local configuration error)"
    "http://www.cfcare.com/links.htm")
   ("webmaster@cxwms.ac.uk"
    "http://www.cxwms.ac.uk/School/Library/subjects.html")
   ("webmaster@info-s.com"
    "http://info-s.com/ill.html")
   ("webmaster@jobson.com"
    "http://www.uspharmacist.com/issues/1998/July/CYSFIB.htm")
   ("webmaster@msx.upmc.edu"
    "http://www.upmc.edu/clc/Links.htm")
   ("webmaster@nwci.net"
    "http://www.nwci.net/medicine.html")
   ("webmaster@pacific-mall.com"
    "http://www.pacific-mall.com/health_fitness/index.htm")
   ("webmaster@sicklehut.com"
    "http://homepages.ihug.co.nz/~jfung/respiratory.htm")
   ("webmaster@smalltownwebs.com"
    "http://www.smalltownwebs.com/cysticf.htm")
   ("wolfdrummer@mailcity.com"
    "http://www.geocities.com/SunsetStrip/Palladium/1277/cf.html")
   ("xcentric@ptbo.igs.net"
    "http://www.geocities.com/WestHollywood/7278/rhealth/health.html")
   ("none (ydchiou@students.wisc.edu: no such user)"
    "http://www.concentric.net/~ydchiou/cystic.htm")
   ("zube7270@fredonia.edu"
    "http://www.fredonia.edu/sa/apo/AboutAPO/APOFAQ.html")))

(setq email-and-url-cf-web-ai-no-email
  '(("none (DFONNER@ART01.FERRIS.EDU: host not found)"
    "http://about.ferris.edu/htmls/academics/course.offerings/physbo/biology/Courses/biol232/diseases/cystic.htm")
    ("none (bad hostname)"
     "http://connexus.apana.org.au/~bitz/bit-medl.html")
    ("none (can't find the link)"
     "http://webalias.com/community")
    ("none (fill out form at http://cnahealthpro.com/links.html)"
     "http://cnahealthpro.com/links.html")
    ("none (fill out form at http://cysticfibrosis.com/form.htm)"
     "http://www.cysticfibrosis.com/links.htm")
    ("none (fill out form at http://saturn.guestworld.tripod.lycos.com/wgb/wgbsign.dbm?owner=Helaina)"
     "http://sargon.mmu.ac.uk/resorce3.htm")
    ("none (form at http://www.ezconnect.com/disea.htm)"
     "http://www.ezconnect.com/disea.htm")
    ("none (form at http://www.io.com/websurfer/submit.html)"
     "http://www.io.com/websurfer/directory/health/health.html")
    ("none (help link is bad)"
     "http://a2z-spry.lycos.com/Health_and_Medicine/Illnesses_and_Disorders/Birth_Defects_and_Genetic_Disorders/index.html")
    ("none (no address for host www.citysurfer.com)"
     "http://www.citysurfer.com/hlthsite.htm")
    ("none (no such page)"
     "http://www.geocities.com/~sarahfiles/medical.html")
    ("none (page not found on server)"
     "http://www.qsl.net/w5pt/nursing.htm")
    ("none: connection timed out"
     "http://www.vrainn.com/Stuff/Search/cystic-fibrosis.html")
    ("none: connection timed out"
     "http://wwwhosp.umhc.umn.edu/cfcenter/resources.html")
    ("none: error 404 not found"
     "http://www.sequoias.cc.ca.us/personal/Sharon_Sharpe/Peds7.html")
    ("none: fill out form at http://lostparadise.com/pipeline.html"
     "http://www.ok-med.net/obstetrics_and_gynecology.html")
    ("none: no address for host www.luckman.com"
     "http://www.luckman.com/yp/0338/033857.html")
    ("none: no address for host www.mhs.schnet.edu.au"
     "http://www.mhs.schnet.edu.au/~library/biolcat.htm")))


;; Sent December 1, 1998
(setq email-and-url-cf-web-ai-19981201
 '(("rcalhoun@alum.mit.edu"
    "http://cf-web.mit.edu/rcalhoun/home.html")
   ("fredw@montana.com"
    "http://www.montana.com/people/butte/fredw/
  http://www.montana.com/people/home1/butte/fredw/www/
  http://www.montana.com/people/home1/butte/fredw/www/index.htm")
   ("duncan@cerf.net"
    "http://www.neonatology.org/neo.links.parents.html
  http://www.neonatology.org/syllabus/syllabus.html")
   ("eaf@networx.on.ca"
    "http://home.networx.on.ca/~eaf/general.htm
  http://www.freenet.hamilton.on.ca/Information/associations/hdef/general.html")
   ("dnonline@idir.net"
    "http://www.comedserv.com/cystic.htm
  http://www.comedserv.com/dyslexia.htm")
   ("Glen.Minty@Dal.CA"
    "http://www2.dal.ca/distsite/frank/cf-net.html
  http://www2.dal.ca/distsite/frank/cf.htm
  http://www2.dal.ca/distsite/frank/cf.html")
   ("brick@mit.edu"
    "http://www.mit.edu:8001/afs/athena.mit.edu/user/b/r/brick/.netscape-bookmarks.html
  http://www.mit.edu:8001/afs/athena/user/b/r/brick/www/Bookmarks/bookmarks_96_8_14.html
  http://www.mit.edu:8001/afs/athena/user/b/r/brick/www/Bookmarks/bookmarks_96_9_8.html")
   ("dcollins@kumc.edu"
    "http://www.kumc.edu/gec/prof/genecomp.html
  http://www.kumc.edu/instruction/medicine/genetics/support/cystic_f.html")
   ("gray@compcomm.com"
    "http://www.compcomm.com/health/health_associations.html
  http://www.compcomm.com/health/health_spec_diseases.html")
   ("info@advocacy-net.com"
    "http://advocacy-net.com/altmedicinemks.htm
  http://advocacy-net.com/celinedionmks.htm
  http://www.advocacy-net.com/celinedionmks.htm
  http://www.gen.com/ani/scammks.htm")
   ("info@healthatoz.com"
    "http://www.healthatoz.com/categories/DCGMCF.htm
  http://www.healthatoz.com/categories/DCPC.htm
  http://www.healthatoz.com/categories/MLDCGMCF.htm")
   ("info@megabios.com"
    "http://www.megabios.com/Links.html
  http://www.megabios.com/Partners.html")
   ("jchfavre@scopus.ch"
    "http://ourworld.compuserve.com/homepages/JCh_FAVRE/frqmuco.htm
  http://ourworld.compuserve.com/homepages/JCh_FAVRE/liens.htm
  http://ourworld.compuserve.com/homepages/JCh_FAVRE/medliens.htm
  http://ourworld.compuserve.com/homepages/JCh_FAVRE/qmuco.htm")
   ("none (jsabbott@pacbell.net: user unknown)"
    "http://www.asf.org/donationlinks.html
  http://www.asf.org/gentxlinks.html
  http://www.asf.org/liverlinks.html
  http://www.asf.org/liverlinks.html
  http://www.asf.org/lunglinks.html
  http://www.asf.org/pancreaslinks.html
  http://www.asf.org/pedtxlinks.html")
   ("kara@ath.forthnet.gr"
    "http://www.edae.gr/associations.htm
  http://www.edae.gr/eassociations.htm")
   ("normap@nbnet.nb.ca"
    "http://www3.nbnet.nb.ca/normap/CF101/associ.htm
  http://www3.nbnet.nb.ca/normap/CF101/cepacia.htm
  http://www3.nbnet.nb.ca/normap/CF101/stories.htm")
   ("polreply1@pol.net"
    "http://home.po.com/html/hpol/springboard/alpha_associations.shtml
  http://home.po.com/html/hpol/springboard/associations.shtml")
   ("r_baird@clinch.edu"
    "http://pluto.clinch.edu/home/jrb/public_html/melissa.html
  http://pluto.clinch.edu/~jrb/melissa.html")
   ("spcran@acs.ucalgary.ca"
    "http://rehab.educ.ucalgary.ca/courses/edis/551.58/Endocrine_and_Reproductive/cystic-overview.html
  http://rehab.educ.ucalgary.ca/courses/edis/551.58/Respiratory/cystic-overview.html")
   ("staff@infoability.org"
    "http://www.infoability.org/resource/physical.html
  http://www.infoability.org/resource/physical_fr.html")
   ("stanis@healthinfonet.co.il"
    "http://www.healthinfonet.co.il/links.diseases.htm
  http://www.healthinfonet.co.il/links.societies.htm")
   ("suzuki@dnavec.co.jp"
    "http://www.dnavec.co.jp/HOME/LINK/link3.html
  http://www.dnavec.co.jp/HOME/LINK/link6.html")
   ("none (trc@fox.nstn.ca no longer controls that site)"
    "http://eagle.valleyweb.com/krrc/distext.html
  http://eagle.valleyweb.com/krrc/resource.html")
   ("tsingebr@iastate.edu"
    "http://project.bio.iastate.edu/Courses/GEN308/Chapter_Links/ch7.htm
  http://project.bio.iastate.edu/Courses/GEN308/Chapter_Links/ch8.htm")
   ("u7j00ai@mail.lrz-muenchen.de"
    "http://www.gma.mwn.de/mlink.html
  http://www.gma.mwn.de/mlink1.html")
   ("webm@nhfirst.com"
    "http://web0.netis.com/nhfirst/kclimb/kc95use.htm
  http://www.nhfirst.com/kclimb/kclimb.htm")
   ("webmaster@dupontmerck.com"
    "http://www.dupontmerck.com/disease/
  http://www.dupontmerck.com/mappage.htm")
   ("webmaster@medicallink.se"
    "http://www.medicallink.se/medlink/m_links/med_links/obstetrics_an_gynecol.htm
  http://www.medicallink.se/medlink/m_links/med_links/pediatrics.htm
  http://www.medicallink.se/medlink/m_links/med_links/respiration_medicine.htm")
   ("webmaster@technet.net.mx"
    "http://192.100.204.58/tech-med/enf/
  http://www5.technet.net.mx/tech-med/enf/index.htm")
   ("70374.641@compuserve.com"
    "http://ourworld.compuserve.com/homepages/Ian_Townend/cflinks.htm")
   ("AROCHE@SVHERC.UCD.IE"
    "http://www.ucd.ie/~svh-erc/medthr/cystfibr.htm")
   ("Algis@kma.lt"
    "http://www.info.kma.lt/kma/Studies/gl_link.html")
   ("Chris.Cruger@Hitchcock.org"
    "http://www.weeks.hitchcock.org/Resource/DISEAS.HTM")
   ("none (DFONNER@ART01.FERRIS.EDU: host not found)"
    "http://about.ferris.edu/htmls/academics/course.offerings/physbo/biology/Courses/biol232/diseases/cystic.htm")
   ("JOHND@helix.org"
    "http://www.helixhealth.com/LINK/cf.htm")
   ("J_HOYER@DSIORG.DK"
    "http://www.handicap.dk/cf-foren/default.htm")
   ("Joel.Pranikoff@UC.Edu"
    "http://www.uc.edu/~pranikjd/BRSTFEED.HTML")
   ("Julia.S@t-online.de"
    "http://geocities.com/SunsetStrip/Arena/9019/hotlinx.htm")
   ("KJanowiak@aol.com"
    "http://members.aol.com/kjanowiak/gene_disease.html")
   ("Nutrinet@mail.ehnr.state.nc.us"
    "http://www.sips.state.nc.us/DHHS/WCH/nss/textonly/MCH/NSS/NSS/cystic.htm")
   ("OII-request@prism.prs.k12.nj.us"
    "http://oii.org/archives/Sprague.html")
   ("Peacock351@aol.com"
    "http://rdz.acor.org/lists/our-kids/Disweb/diswebbc.html")
   ("none (QV2373_116@Apollo.Commnet.edu: no such user)"
    "http://www.qvctc.commnet.edu/QVCTC/student/KimCurtin/cf.html")
   ("RadSurfer@yahoo.com"
    "http://members.tripod.com/~RadSurfer/useful.html")
   ("SJSpengler@lbl.gov"
    "http://www.lbl.gov/Education/ELSI/genetic-testing.html")
   ("StewartPub@aol.com"
    "http://www.interactive-healthcare.com/internet/F1.htm")
   ("Sylvia.DeBie@rug.ac.be"
    "http://allserv.rug.ac.be/~sdebie/index3b.html")
   ("admin@pedi-resp-pulm.com"
    "http://pedi-resp-pulm.com/resource.html")
   ("aess@cei.net"
    "http://www.arkeasterseals.org/syn_a_f.htm")
   ("afearon@plymouth.swis.net"
    "http://www.swis.net/health/mir/midgley/cf000.htm")
   ("alfnat@ozemail.com.au"
    "http://www.lungnet.org.au/links.html")
   ("allyn@outfitters.com"
    "http://outfitters.com/allyn/price.html")
   ("angelegm@ctv.es,jose.pulido@mad.servicom.es"
    "http://www.pypweb.com/fqart1.htm")
   ("aparks@agate.net"
    "http://www.ahead.org/links.htm")
   ("belda.thomas@mayo.edu"
    "http://msrcnet.com/resplinks.htm")
   ("bobm@foocus.com"
    "http://www.foocus.com/rclinks.htm")
   ("brokenlink@breastisbest.com"
    "http://www.breastisbest.com/links/articles.htm")
   ("btepp@quart.com"
    "http://www.quart.com/nsu/condx.html")
   ("bzlab@imgate.wustl.edu"
    "http://www.surgery.wustl.edu/bjcmdl/medgen.htm")
   ("cfawa@emedia.com.au,emedia@emedia.com.au"
    "http://www.emedia.com.au/cfawa/cf.html")
   ("cfnj@cfusa.org"
    "http://www.cfusa.org/nj/links.html")
   ("cfwh@wrldhlth.org"
    "http://www.wrldhlth.org/healthr.htm")
   ("chris@gcedunet.gac.peachnet.edu"
    "http://gcedunet.gac.peachnet.edu/~chris/bookmarks.html")
   ("cityspectrum@cityspectrum.com"
    "http://www.cityspectrum.com/healthl.htm")
   ("cmayer@lycos.com"
    "http://www.contrib.andrew.cmu.edu/~cmayer/purplrib.html")
   ("cmeinfo@kma.lt"
    "http://193.219.37.106/med_educ_guide.htm")
   ("cong@geocities.com"
    "http://www.geocities.com/CollegePark/2791/index.html")
   ("crosline@bellsouth.net"
    "http://www.provida.com/Document/LINKS.HTM")
   ("crsullivan@ucdavis.edu"
    "http://www.geocities.com/CollegePark/Library/6201/ytweety.html")
   ("crunnels@glasscity.net"
    "http://www.geocities.com/SouthBeach/Lagoon/6550/theresa.html")
   ("dana@sonic.net"
    "http://www.sonic.net/dana/library/index.html")
   ("detroit@cats.ucsc.edu"
    "http://www2.ucsc.edu/people/cierra/detroit/Judaism/Threat.html")
   ("dgleeson@nais.com"
    "http://www.smithnetny.com/fitness/links.htm")
   ("doby@hol.gr"
    "http://members.tripod.com/~limnos/")
   ("emory@asel.udel.edu"
    "http://www.asel.udel.edu/sem/resources/disability/index.html")
   ("esc@ulysses.att.com"
    "http://skunk.research.att.com/Surf/March95/surf032795.html")
   ("falkweb+@pitt.edu"
    "http://www.hsls.pitt.edu/intres/health/lung.html")
   ("fantognini@tinet.ch,100517.377@compuserve.com"
    "http://ourworld.compuserve.com/homepages/fantognini/iacfa/cflink.htm")
   ("fiona@cmdr.ubc.ca"
    "http://www.interchange.ubc.ca/bobh/")
   ("fjvega@gesi.es"
    "http://www.paidos.net/Areas_p/gastro/enlaces.htm")
   ("galaxy@einet.net"
    "http://www.einet.net/galaxy/Community/Health/Diseases-and-Disorders/Cystic-Fibrosis.html")
   ("garlipp@informatik.uni-rostock.de"
    "http://www.informatik.uni-rostock.de/HUM-MOLGEN/NewsGen/jan-apr-96.html")
   ("genetest@aol.com"
    "http://www.genetest.com/websites.htm")
   ("genetica@famema.br"
    "http://www.famema.br/genetica/welcome.html")
   ("gkchc@gkchc.org"
    "http://gkchc.org/support.htm")
   ("gpi@ncgr.org"
    "http://www.ncgr.org/gpi/grn/resapp/applications/cftr.html")
   ("gs@sime.com"
    "http://www.expo.org/intermed/pulmo.htm")
   ("guoli@ipoline.com"
    "http://home.ipoline.com/~guoli/pinfo/dict0.htm")
   ("gupta@bu.edu"
    "http://gopher1.bu.edu/COHIS/help/find/dislttr/c.htm")
   ("harvey@glinx.com"
    "http://www.glinx.com/vradio/magic/person/darrin.html")
   ("headpilot@webflier.com"
    "http://www.webflier.com/wfhtml/Health1.shtml")
   ("healthdat@aol.com"
    "http://www.mindspring.com/~hlthdata/other.html")
   ("none (heatonj@fcbe.edu.on.ca: host unknown)"
    "http://www.icactive.com/jwz/VerC.htm")
   ("help@omni.ac.uk"
    "http://yi.com/home/ArvaAdrian/omni/ejb.htm")
   ("hochstrd@cs.rose-hulman.edu"
    "http://www.eng.ncat.edu/~jona/facts.html")
   ("hsingh@u.arizona.edu"
    "http://student.biology.arizona.edu/honors96/group9/page8.html")
   ("iching1@aol.com"
    "http://members.aol.com/IChing1/yarrow.htm")
   ("info@del-crane.com"
    "http://www.del-crane.com/html/webres.htm")
   ("info@doctorsnet.com"
    "http://doctorsnet.com/MedLinks/Illness.htm")
   ("info@encoreortho.com"
    "http://www.encoremed.com/research/index.html")
   ("info@infoshop.com"
    "http://infoshop.com/info/medlink.html")
   ("info@ixion-biotech.com"
    "http://www.ixion-biotech.com/cystic.htm")
   ("info@nvog.nl"
    "http://www.nvog.nl/pub/links/indexoud.htm")
   ("info@thehealthconnection.com"
    "http://www.thehealthconnection.com/Disease%20Center/diseases/cysticfibrosis.asp")
   ("infobahn@outfitters.com"
    "http://www.outfitters.com/infobahn/seminars/awareness/medicine.html")
   ("innomagi@abacom.com"
    "http://www.abacom.com/innomagi/online/health_m/disncond.htm")
   ("jaeckel@medizin-forum.de"
    "http://www.medizin-forum.de/schebo/engl1.htm")
   ("janders@cem202c-1.che.ttu.edu"
    "http://cem202c-1.che.ttu.edu/~janders/joe.html")
   ("janen@starmail.com"
    "http://webzone1.co.uk/www/cathus/janelnk5.htm")
   ("jaward@express-news.net"
    "http://www.express-news.net/jaward/")
   ("jbuenk@arizona.midwestern.edu, buenker@hotmail.com"
    "http://alexia.lis.uiuc.edu/~buenker/miscmed.html")
   ("jcarr@mrbit.es"
    "http://www.mrbit.es/~jcarr/links/megaweb/6500b_m2.htm")
   ("jcooper@medreport.com"
    "http://medicalreporter.health.org/tmr0296/letters0296.html")
   ("jdud@wtp.net"
    "http://www.mcn.net/~jdud/cystic.html")
   ("jdv@metamatrix.com"
    "http://www4.pgh.net/~jdv/cf.htm")
   ("jennifer@fanlight.com"
    "http://www.fanlight.com/chronic.htm")
   ("jhenly@highexplorer.com"
    "http://www.geocities.com/Athens/Acropolis/2458/biology.html")
   ("jim@city-host.com"
    "http://www.christianfamily.net/health.htm")
   ("jjones@utmb.edu"
    "http://www.utmb.edu/pcs/ut01000.htm/about.htm")
   ("jklauber@disabilityresources.org"
    "http://www.geocities.com/CapitolHill/1703/CYSTIC.html")
   ("jlk2jc@polarbear.stark.k12.oh.us"
    "http://jhslib.stark.k12.oh.us/interlib/science.html")
   ("josepulido@jet.es"
    "http://www.geocities.com/HotSprings/2677/links.htm")
   ("jsinger@kettering.edu"
    "http://www.gmi.edu/official/acad/mech-eng/homschol.htm")
   ("keepercjr@aol.com"
    "http://members.aol.com/KeeperCJR/home.html")
   ("kenotz@execpc.com"
    "http://www.execpc.com/~kenotz/health.html")
   ("kristachan@aol.com"
    "http://members.aol.com/kristachan/bflink.htm")
   ("kwhite@lands.ab.ca"
    "http://www.aspenrha.ab.ca/thememay.htm")
   ("l.spero@utoronto.ca"
    "http://icarus.med.utoronto.ca/week17/17cycstic.htm")
   ("labguy@gramercy.ios.com"
    "http://idt.net/~labguy/lablink.html")
   ("lgiteck@scpie.com"
    "http://www.scpie.com/links/links6.shtml")
   ("magal@centroin.com.br"
    "http://www.geocities.com/HotSprings/1613/doencas.htm")
   ("malis@nca.pt"
    "http://www.helicobacter.org/ehpsg_links_treatment.htm")
   ("malumm01@mcrcr.med.nyu.edu"
    "http://mcpth10.med.nyu.edu/Malumbres/bookmark.htm")
   ("marathonman@argo.net"
    "http://www.donp.com/cf.html")
   ("mcmaster@cs.washington.edu"
    "http://www.vrainn.com/Stuff/Search/cystic-fibrosis.html")
   ("medlib@jaguar1.usouthal.edu"
    "http://southmed.usouthal.edu/library/www/cyst.htm")
   ("medlibem@hkuxa.hku.hk"
    "http://www.hku.hk/lib/medlib/docs/subject.html")
   ("medvalar@erie.ne"
    "http://www.nwpamed.com/Prolinks.htm")
   ("mfeltner@pepperdine.edu"
    "http://www.pepperdine.edu/seaver/natsci/Spme/Faculty_SPME/M_Feltner/Classes/SPME400/wwwsites.html")
   ("michael.mcpeck@sunysb.edu"
    "http://209.45.150.80/links.htm")
   ("midden@bgsu.bgnet.edu"
    "http://www.bgsu.edu/departments/chem/midden/chem308/Biochem_WWW_Pages96.html")
   ("monroejd@jmu.edu"
    "http://csm.jmu.edu/biology/courses/bio220/aotw1.html")
   ("morey@ixnetcom.com"
    "http://www.faculty.fairfield.edu/faculty/fleitas/essay.html")
   ("mortgage@warwick.net"
    "http://uscj.org/metny/middletown/kosher.htm")
   ("msrc@wtp.net"
    "http://wtp.net/~msrc/msrc5.htm")
   ("nina_drucker@prnewswire.com"
    "http://www.prnewswire.com/health/healthcare_assc.html")
   ("nmmirash@mtu.edu"
    "http://www.geocities.com/SiliconValley/1910/personal.htm")
   ("norman@zianet.com"
    "http://www.zianet.com/norman/medical.htm")
   ("nwilkin@olemiss.edu"
    "http://www.olemiss.edu/depts/pharm_admin/links.htm")
   ("obstet@uerj.br"
    "http://www.lampada.uerj.br/obst/links.html")
   ("omie@welchlink.welch.jhu.edu"
    "http://omie.med.jhmi.edu/LectureLinks/Lung.html")
   ("paul@plsys.co.uk"
    "http://www.plsys.co.uk/~paul/bio/Paul.Home.htmld/")
   ("pessoa@dcc.ufmg.br"
    "http://www.dcc.ufmg.br/~pessoa/tese/q21-01.htm")
   ("petersme@clem.mscd.edu"
    "http://clem.mscd.edu/~petersme/Welcome.html")
   ("pmurf@ix.netcom.com"
    "http://mcrcr2.med.nyu.edu/murphp01/disease.htm")
   ("purelife@rocketmail.com"
    "http://www.useekufind.com/health.htm")
   ("pursuit@uiuc.edu"
    "http://pursuit.rehab.uiuc.edu/pursuit/dis-resources/inet-dis/inet-dis.html")
   ("pvosta@janbe.jnj.com"
    "http://ourworld.compuserve.com/homepages/pvosta/pcrmed.htm")
   ("rbhunt@pol.net"
    "http://www.wardweb.com/obgyn.htm")
   ("ribfb@olemiss.edu"
    "http://www.olemiss.edu/depts/rips/pmmrp/pharm-link.htm")
   ("rthill@ozemail.com.au"
    "http://www.ozemail.com.au/~rthill/")
   ("rw@nhfirst.com"
    "http://web0.netis.com/nhfirst/kclimb/kccfpage.htm")
   ("s.walters@bham.ac.uk"
    "http://web.bham.ac.uk/walterss/cystic2.htm")
   ("scandipharm@worldnet.att.net"
    "http://www.scandipharm.com/_vti_bin/shtml.exe/other.htm/map1")
   ("sciwebmaster@sciweb.com"
    "http://www.sciweb.com/home/eddyjake/grants_pages.html")
   ("serchap@umslvma.umsl.edu,webmaster@www.umsl.edu"
    "http://www.umsl.edu/~writcert/connections.html")
   ("sew7490@rit.edu"
    "http://www.rit.edu/~sew7490/patriot.html")
   ("smitka@fnplzen.cz"
    "http://www.fnplzen.cz/sources/medicine/diseases.html")
   ("snjaka@hotmail.com"
    "http://www.geocities.com/EnchantedForest/7321/therents.html")
   ("sologuk@plains.nodak.edu"
    "http://www.cc.ndsu.nodak.edu/instruct/stammen/uswest/about_grant/html/sologuk3.htm")
   ("starwalker@snowcrest.net"
    "http://www.snowcrest.net/starwalker/genes/genetics.htm")
   ("strait@pinn.net"
    "http://www.geocities.com/SoHo/Lofts/7438/mylinks.html")
   ("sudhir@royal.net"
    "http://www.co.umist.ac.uk/~sudhir/Htdocs/Links/megalinks3.html")
   ("sujin@sis.pitt.edu"
    "http://www.sis.pitt.edu/~sujin/consumer.html")
   ("support@adrsa.org"
    "http://www.adrsa.org/links.htm")
   ("swdavis@bgsm.edu"
    "http://www.wp.com/netnurse/support.htm")
   ("telltdo@tdo.infi.net"
    "http://vh1421.infi.net/local/links/medweb.htm")
   ("thorn@denison.edu"
    "http://www.denison.edu/~thorn/A&P.html")
   ("none (tjuly@umich.edu forwards to bad address)"
    "http://www.lakeville.k12.mi.us/cc/moreabou.htm")
   ("tjw@cybertap.com"
    "http://www.cybertap.com/sunrise/mil.htm")
   ("trudydevries@unitedmp.com.au"
    "http://www.unitedmp.com.au/resource.html")
   ("tsuits@midamer.net"
    "http://www.midamer.net/space/health.html")
   ("twood@indian.vinu.edu"
    "http://twood.vinu.edu/Sites/sites.htm")
   ("unimed@sulbanet.com.br"
    "http://www.sulbanet.com.br/unimed/newpage3.htm")
   ("vtnhsrc@aol.com"
    "http://members.aol.com/vtnhsrc/links.htm")
   ("wbbebout@evansville.net"
    "http://drake.as.arizona.edu/~apoyner/med_links.html")
   ("webmaster@ability.org"
    "http://www.ability.org/cystic.html")
   ("none (webmaster@asf.org: expands to bad address)"
    "http://livertx.org/cysticlinks.html")
   ("webmaster@bcbsde.com"
    "http://www.bcbsde.com/Advlinks.htm")
   ("none (webmaster@cfcare.com: local configuration error)"
    "http://www.cfcare.com/links.htm")
   ("webmaster@cxwms.ac.uk"
    "http://www.cxwms.ac.uk/School/Library/subjects.html")
   ("webmaster@healthinfo.com"
    "http://www.healthinfo.com/library.htm")
   ("webmaster@info-s.com"
    "http://info-s.com/ill.html")
   ("webmaster@jobson.com"
    "http://www.uspharmacist.com/issues/1998/July/CYSFIB.htm")
   ("webmaster@lowellgeneral.org"
    "http://www.lowellgeneral.org/HTML/WebSiteFndr.html")
   ("webmaster@mcs-special-support.com"
    "http://www.mcs-special-support.com/health_and_medical/diseases/genetic/cystic_f.html")
   ("webmaster@microstep.fi"
    "http://194.89.148.4/uterveys.htm")
   ("webmaster@ndhosp.com"
    "http://site210136.primehost.com/child.htm")
   ("webmaster@nwci.net"
    "http://www.nwci.net/medicine.html")
   ("webmaster@osc.on.ca"
    "http://www.osc.on.ca/HumanBody/exhibitfloor/misc.html")
   ("webmaster@pacific-mall.com"
    "http://www.pacific-mall.com/health_fitness/index.htm")
   ("webmaster@sicklehut.com"
    "http://homepages.ihug.co.nz/~jfung/respiratory.htm")
   ("webmaster@tcstore.it"
    "http://www.pediatria.it/famiglie/content/r_mecon.htm")
   ("webmaster@vfed.org"
    "http://www.vfed.org:8080/public/illness.htm")
   ("webmaster@wythenews.com"
    "http://www.wythenews.com/970222/Content.htm")
   ("webmastr@uws.org"
    "http://www.uws.org/agencies.html")
   ("webteam@growing.com"
    "http://www.growing.com/doxys/neonatal.html")
   ("webweaver@netlink.co.uk"
    "http://www.community-care.org.uk/health/dc02.html")
   ("wolfdrummer@mailcity.com"
    "http://www.geocities.com/SunsetStrip/Palladium/1277/cf.html")
   ("wwwcomed@pop.uky.edu"
    "http://www.comed.uky.edu/medicine/ACME/LAB/hits1st.htm")
   ("xcentric@ptbo.igs.net"
    "http://www.geocities.com/WestHollywood/7278/rhealth/health.html")
   ("none (ydchiou@students.wisc.edu: no such user)"
    "http://www.concentric.net/~ydchiou/cystic.htm")
   ("yeast@reed.edu"
    "http://134.10.2.252/academic/departments/biology/courses/BIO361/gen.topareas.html")
   ("zube7270@fredonia.edu"
    "http://www.fredonia.edu/sa/apo/AboutAPO/APOFAQ.html")))

;; ===========================================================================
;;
;; ;; I used this on December 1, 1998 to send mail to Webmasters about
;; ;; outdated links to CF-WEB.
;;
;;
;; Your WWW page
;;
;; contains a link to CF-WEB -- Online Information About Cystic Fibrosis.
;; Thank you for this link.  However, you link to an old URL;
;; CF-WEB has moved to a new home.  Please update your files:
;;
;;   OLD: http://www.ai.mit.edu/people/mernst/cf/
;;   NEW: http://www.cf-web.org/
;;
;; 				-Michael Ernst
;; 				 Cystic Fibrosis Online Resources, Inc.
;; 				 mernst@cf-web.org
;;
;;
;; ---------------------------------------------------------------------------

;;; Notified by filling out a form on December 1, 1998
;;
;; email: http://bhsweb.norshore.wednet.edu/constr.crew/ccrew-pg.html
;;   http://bhsweb.norshore.wednet.edu/library/genetic-disorder-pg.html
;;
;; email: http://cnahealthpro.com/contact_cna_healthpro.html
;;   http://cnahealthpro.com/links.html
;;
;; email: http://cysticfibrosis.com/form.htm
;;   http://cysticfibrosis.com/links.htm
;;
;; email: http://lostparadise.com/pipeline.html
;;   http://www.ok-med.net/obstetrics_and_gynecology.html
;;
;; email: http://ourworld.compuserve.com/homepages/jpresch/tomfeed.htm
;;   http://ourworld.compuserve.com/homepages/jpresch/tomorg1.htm
;;
;; email: http://saturn.guestworld.tripod.lycos.com/wgb/wgbsign.dbm?owner=Helaina
;;   http://sargon.mmu.ac.uk/resorce3.htm
;;
;; email: http://www-med.stanford.edu/healthlink/feedback.html
;;   http://www-med.stanford.edu/healthlink/_news/_general/111396cfgene.html
;;
;; email: http://www.cysticfibrosis.com/form.htm
;;   http://www.cysticfibrosis.com/links.htm
;;
;; email: http://www.ezconnect.com/feedback.htm
;;   http://www.ezconnect.com/disea.htm
;;
;; email: http://www.gobutton.com/yellform.html
;;   http://gobutton.com/m/illnessesb.html
;;
;; email: http://www.io.com/websurfer/submit.html
;;   http://www.io.com/websurfer/directory/health/health.html
;;
;; email: http://www.antadir.asso.fr/courrier/indxcour.htm
;;   http://www.antadir.asso.fr/liens/liens.htm
;;
;; email: http://www2.nb.sympatico.ca/Sommaire/Sante/F_GENERAL/feedback_1.html
;;   http://www2.nb.sympatico.ca/Sommaire/Sante/F_LISTS/B2-C18-02_all1.html
;;   http://www2.nb.sympatico.ca/Sommaire/Sante/F_REV_HTML/FR4046.html
;;   http://www2.nb.sympatico.ca/Sommaire/Sante/F_REV_HTML/FR4152.html
;;   http://www2.sympatico.ca/Sommaire/Sante/F_REV_HTML/FR4150.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sent January 23, 1999
(setq email-and-url-msr
  '(("Jonathan.Hill@comlab.ox.ac.uk"
     "http://www.ece.purdue.edu/~eigenman/EE573/ci-tools.html")
    ("psm@sics.se"
     "http://www.sics.se/~psm/bookmarks.html")))


;;; Old MSR links:
;;;   link:"http://www.research.microsoft.com/research/analysts/mernst"
;;; I know I'm missing some, because
;;;   http://www.mit.edu:8001/activities/tep/html/number22.html
;;; points to
;;;   http://www.research.microsoft.com/research/analysts/mernst
;;; but on 6 Jan 1999, AltaVista gives no results from a search for
;;;   link:"http://www.research.microsoft.com/research/analysts/mernst"
;;; not even returning the below!
;;
;; IR '95:
;;
;; ley@uni-trier.de
;; http://www.informatik.uni-trier.de/~ley/db/conf/popl/popl95.html
;;
;; Conf resources:
;;
;; Bernard.Lang@inria.fr
;; http://pauillac.inria.fr/SIGPLAN/INDEX.html
;; http://pauillac.inria.fr/SIGPLAN/conferences/index.html
;;
;; mleone@cs.cmu.edu
;; http://foxnet.cs.cmu.edu/~mleone/language/new.2.95.html

