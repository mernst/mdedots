;;; -*- lexical-binding: t -*-

;; This file sets rmail-output-file-alist and sendmail-from-address-alist.
;; Those affect where RMAIL saves messages and what fields it sets in outgoing mail.

(eval-when-compile
  (if (locate-library "vm")
      (require 'vm)
    (message "VM not found while loading rmail-addresses-mde"))
  )

(setq vm-auto-folder-case-fold-search t)

;;; New solution: just save this file, and the right thing will happen.
;; To re-evaluate, run:
;;   (eval-buffer)
;; the key part of which is the call to (setup-addresses).


(defvar addr-info-list-1 nil)
(setq addr-info-list-1
   ;; This is a list of two-or-three-element lists.
   ;; Earlier elements take precedence.
   ;; The sublists contain: email-regexp, output-file, my-from-address.
   ;; If my-from-address is a symbol, it is evaluated (to a string).
   '(
     ;;; Advisees

     ;; HKN
     ;; This comes before research to avoid mis-classification (when
     ;; an advisee is also in HKN).
     (".*@hkn.org" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("aalamb@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("iahn@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("hkn-officers@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("hkn-eligibles@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("hkn@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("asa-exec@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("asa-president@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("asa-treasurer@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("asa-official@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("nitzan@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("k6zt@juno.com" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("laurie@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("KRicker@iec.org" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ;; ("sostler@mit.edu" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("academic-i-finboard-subgroup@mit.edu" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("finboard.*@mit.edu" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("myhre@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("kalata@cbis.ece.drexel.edu" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("c_royce@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("DPhillips@iec.org" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("brianwu@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("MSwartz@iec.org" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("spal@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("executive@hkn.org" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("petek@mit.edu" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("mmiller@iec.org" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("laurie@MIT.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("orr@WPI.EDU" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("nivedita@mit.edu" "~/class/admin/hkn/mail.mail" mernst-csail-address)
     ("hkn-all-tutors@mit.edu" "~/class/admin/hkn/mail.mail" mernst-csail-address)

     ("Jingyue.Li@idi.ntnu.no" "~/corr/prospective-staff.mail")

     ;;; Research

     ;; Papers
     ("glenn@corvustranslations.com" "~/research/invariants/mail/papers.mail")
     ("binoy.b@elsevier.com" "~/research/invariants/mail/papers.mail")
     ("authorsupport@elsevier.com" "~/research/invariants/mail/papers.mail")

     ;; Daikon
     ("daikon-\\(announce\\|discuss\\|developers\\)@\\(\\(lists\\|pag\\|geyer\\|pink-panther\\).\\)?\\(csail\\|lcs\\).mit.edu" "~/research/invariants/mail/daikon-lists.mail")
     ("bugzilla-daemon@csail.mit.edu" "~/research/invariants/mail/daikon-lists.mail")
     ("noreply@freshmeat.net" "~/research/invariants/mail/daikon-lists.mail")
     ("info@dvholten.de" "~/research/invariants/mail/daikon-lists.mail")

     ("joshkata@geocities.co.jp" "~/research/invariants/mail/josh.mail")
     ("kataoka@cs.washington.edu" "~/research/invariants/mail/josh.mail")
     ("yoshio.kataoka@toshiba.co.jp" "~/research/invariants/mail/josh.mail")
     ("joshkata@world.email.ne.jp" "~/research/invariants/mail/josh.mail")
     ("joshkata@gmail.com" "~/research/invariants/mail/josh.mail")
     ("takeo.imai@toshiba.co.jp" "~/research/invariants/mail/josh.mail")
     ("takeo@csail.mit.edu" "~/research/invariants/mail/josh.mail")

     ;; ("evans@cs.virginia.edu" "~/research/invariants/mail/annotation-user-study.mail")

     ;; These need to precede the Agitar rules
     ("saff@\\(csail.\\)?MIT.EDU" "~/class/advisees/saff.mail")
     ("david.saff@gmail.com" "~/class/advisees/saff.mail")
     ("david@saff.net" "~/class/advisees/saff.mail")

     ("@agitar.com" "~/research/invariants/mail/commercial.mail")
     ("@scrutiny.com" "~/research/invariants/mail/commercial.mail")

     ("isabelle@theory.lcs.MIT.EDU" "~/research/invariants/mail/isabelle.mail")
     ("lgdean@\\(theory.lcs.\\)?mit.edu" "~/research/invariants/mail/ioa.mail")
     ("ioa\\(-users\\)?@\\(theory\\|condor\\).\\(lcs\\|csail\\).mit.edu" "~/research/invariants/mail/ioa.mail")
     ("josh@\\(theory\\|condor\\).\\(lcs\\|csail\\).mit.edu" "~/research/invariants/mail/ioa.mail")
     ("mjt@\\(theory.lcs.\\)?MIT.EDU" "~/research/invariants/mail/ioa.mail")
     ("cluhrs@MIT.EDU" "~/research/invariants/mail/ioa.mail")
     ("dilsun@theory.lcs.mit.edu" "~/research/invariants/mail/ioa.mail")

     ("\\broc[^ \n]*@cs.cmu.edu" "~/research/invariants/mail/ajax.mail")
     ("robert@ocallahan.org" "~/research/invariants/mail/ajax.mail")
     ("rustan.leino@compaq.com" "~/research/invariants/mail/esc.mail")
     ("rustan@leino-online.com" "~/research/invariants/mail/esc.mail")
     ("leino@microsoft.com" "~/research/invariants/mail/esc.mail")
     ("Jim.Saxe@compaq.com" "~/research/invariants/mail/esc.mail")
     ("escjava@research.compaq.com" "~/research/invariants/mail/esc.mail")
     ("kiniry@acm.org" "~/research/invariants/mail/esc.mail")

     ;; Before JML rules, to override them
     ;; ("M.A.Wermelinger@open.ac.uk" "~/prof/referee/paste-2005/mail.mail")
     ;; ("Bertrand.Meyer@inf.ethz.ch" "~/prof/referee/tap-2007/mail.mail")
     ;; ("moriol@inf.ethz.ch" "~/prof/referee/tap-2007/mail.mail")
     ;; ("tap-org@se.inf.ethz.ch" "~/prof/referee/tap-2007/mail.mail")
     ;; ("tap-pc@se.inf.ethz.ch" "~/prof/referee/tap-2007/mail.mail")
     ;; ("tap07@easychair.org" "~/prof/referee/tap-2007/mail.mail")

     ("leavens@cs.iastate.edu" "~/research/invariants/mail/jml.mail")
     ("jml-interest-list@cs.iastate.edu" "~/research/invariants/mail/jml.mail")
     ("jmlspecs-interest-request@lists.sourceforge.net" "~/research/invariants/mail/jml.mail")
     ("jmlspecs-interest@lists.sourceforge.net" "~/research/invariants/mail/jml.mail")
     ("issues@dbc.dev.java.net" "~/research/invariants/mail/jml.mail")
     ("erikpoll@cs.kun.nl" "~/research/invariants/mail/jml.mail")
     ;; JSRs
     ;; ("max@jetbrains.com" "~/research/invariants/mail/jml.mail")

     ("gary_wingert@sonic.com" "~/research/macros/macros.mail")

     ("campbell@beloit.edu" "~/research/konane/campbell.mail")

     ;; Grants

     ;; ("Brian.Reid@cs.cmu.edu" "~/prof/grants/hdcp/mail.mail")
     ;; ("cleah@cs.cmu.edu" "~/prof/grants/hdcp/mail.mail")
     ;; ("scherlis@cs.cmu.edu" "~/prof/grants/hdcp/mail.mail")
     ;; ("wme@cs.cmu.edu" "~/prof/grants/hdcp/mail.mail")
     ;; ("gdennis@MIT.EDU" "~/prof/grants/hdcp/mail.mail")
     ;; ("mlowry@mail.arc.nasa.gov" "~/prof/grants/hdcp/mail.mail")
     ;; ("lowry@email.arc.nasa.gov" "~/prof/grants/hdcp/mail.mail")
     ;; ("koren@euler.ecs.umass.edu" "~/prof/grants/hdcp/mail.mail")
     ;; ("koren@ecs.umass.edu" "~/prof/grants/hdcp/mail.mail")

     ("rhangros@\\(west.\\)?raytheon.com" "~/prof/grants/raytheon/mail.mail")
     ("cbboettcher@\\(west.\\)?raytheon.com" "~/prof/grants/raytheon/mail.mail")
     ("dwbaggett@\\(west.\\)?raytheon.com" "~/prof/grants/raytheon/mail.mail")
     ("baggett@ice.rsc.raytheon.com" "~/prof/grants/raytheon/mail.mail")
     ("p-green@raytheon.com" "~/prof/grants/raytheon/mail.mail")
     ("araragi@cslab.kecl.ntt.co.jp" "~/prof/grants/2001-05-ntt/mail.mail")
     ("araragi@lcs.mit.edu" "~/prof/grants/2001-05-ntt/mail.mail")
     ("araragi@fancy.ocn.ne.jp" "~/prof/grants/2001-05-ntt/mail.mail")
     ("smcho@cslab.kecl.ntt.co.jp" "~/prof/grants/2001-05-ntt/mail.mail")
     ("smcho@amsterdam.lcs.mit.edu" "~/prof/grants/2001-05-ntt/mail.mail")
     ("NTT-PI@ai.mit.edu" "~/prof/grants/2001-05-ntt/mail.mail")

     ;; after Raytheon list, to avoid mis-filing mail
     ("gjay@\\(cs.\\)?ucsd.edu" "~/research/invariants/mail/gui.mail")

     ("lldiscuss@ai.mit.edu" "~/prof/grants/lincolnlab.mail")

     ("U.1.Naumann@herts.ac.uk" "~/prof/grants/2001-itr-adjoint/mail.mail")
     ("cnh@plume.mit.edu" "~/prof/grants/2001-11-itr-adjoint/mail.mail")
     ("naumann@mcs.anl.gov" "~/prof/grants/2001-11-itr-adjoint/mail.mail")

     ("zholly@mit.edu" "~/prof/grants/2002-07-deshpande/deshpande.mail")
     ("deshpandecenter@mit.edu" "~/prof/grants/2002-07-deshpande/deshpande.mail")
     ("isadora@MIT.EDU" "~/prof/grants/2002-07-deshpande/deshpande.mail")

     (".*@empirix.com" "~/prof/grants/2002-07-deshpande/empirix.mail")
     ("steve@vanu.com" "~/prof/grants/2002-07-deshpande/vanu.mail")
     ("richard@sigmapartners.com" "~/prof/grants/2002-07-deshpande/vcs.mail")
     ("mkaufman@theworld.com" "~/prof/grants/2002-07-deshpande/vcs.mail")

     ("sshurt@MIT.EDU" "~/prof/grants/2002-06-cmi/cmi.mail")
     ("jl290@eng.cam.ac.uk" "~/prof/grants/2002-06-cmi/cmi.mail")
     ("Ursula.Martin@dcs.qmul.ac.uk" "~/prof/grants/2002-06-cmi/cmi.mail")

     ("jlala@darpa.mil" "~/prof/grants/2002-08-darpa-cognitive/mail.mail")
     ("baa-03-30@ai.mit.edu" "~/prof/grants/2003-06-darpa-lifelog/mail.mail")
     ;; ("lbadger@darpa.mil" "~/prof/grants/darpa-misc.mail")

     ("hgill@darpa.mil" "~/prof/grants/2003-02-itr-medical/mail.mail")

     (".*@determina.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     (".*@vmware.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("lgoergen@snap.org" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("Patrick.Hurley@rl.af.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("alan.akins@rl.af.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("Jacqueline.Smith@rl.af.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("lbadger@darpa.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("lee.badger@darpa.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("jfrank@schafertmd.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("jfrank@snap.org" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("John.Frank.ctr@darpa.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("Laurisa.Goergen.ctr@darpa.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("ac-\\(active\\|advice\\)@\\(lists.\\)?csail.mit.edu" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("Wayne.Bosco@rl.af.mil" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("gregs@csail.mit.edu" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("jaustin@vmware.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     (".*@sparta.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("francis.hannon@baesystems.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("bryan.loyall@baesystems.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("stelios@cs.columbia.edu" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")

     ;; SRS comes after AC, so that AC takes priority.
     ("srslist@tmdlists.com" "~/prof/grants/2003-11-darpa-selfregen/mail.mail")
     ("walt.heimerdinger@honeywell.com" "~/prof/grants/2003-11-darpa-selfregen/mail.mail")
     ("jreel@raba.com" "~/prof/grants/2003-11-darpa-selfregen/mail.mail")

     ("johnspe@microsoft.com" "~/prof/grants/2004-11-microsoft-course/mail.mail")

     ("pierre.oberg@se.abb.com" "~/prof/grants/abb/mail.mail")
     ("brian.p.robinson@us.abb.com" "~/prof/grants/abb/mail.mail")
     ("hudak@MIT.EDU" "~/prof/grants/abb/mail.mail")
     ("vinay.augustine@us.abb.com" "~/prof/grants/abb/mail.mail")

     ;; Accounts
     ;; ("erchoi@csail.mit.edu" nil mernst-csail-address)
     ;; ("snedeker@csail.mit.edu" nil mernst-csail-address)

     ("alexander.ran@nokia.com" "~/prof/grants/2005-06-nokia/mail.mail")

     ("lisaga@MIT.EDU" "~/prof/grants/2005-07-af-certafcs/mail.mail")
     ("mardavij@MIT.EDU" "~/prof/grants/2005-07-af-certafcs/mail.mail")

     ("BAA05-51@DARPA.MIL" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("Raimondas.Lencevicius@nokia.com" "~/prof/grants/nokia/mail.mail")
     ("raimondas@hotmail.com" "~/prof/grants/nokia/mail.mail")
     ("akop@MIT.EDU" "~/prof/grants/nokia/mail.mail")
     ("cfonger@MIT.EDU" "~/prof/grants/nokia/mail.mail")
     ;; sharon@alum.mit.edu overrides the catch-all nokia email address
     ("sharon@alum.mit.edu" "~/corr/jas.mail" mernst-alum-address)
     ("sechang2@gmail.com" "~/corr/jas.mail" mernst-alum-address)
     (".*@nokia.com" "~/prof/grants/nokia/mail.mail")

     ("TOMCHOPPER@aol.com" "~/private/security-clearance/mail.mail")
     (".*@ida.org" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("RA06-05@darpa.mil" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("Robert.Hummel@darpa.mil" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("Benjamin.Mann@darpa.mil" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("swright@schafertmd.com" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("jjacoby@caci.com" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("cssgadmin@itd-webprod.ddlomni.com" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("tmcconnell@schafertmd.com" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("ddrake@sainc.com" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("Jeanne.Breeden.ctr@darpa.mil" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("jerry.lockenour@ngc.com" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
     ("b.mcnamara@starpower.net" "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")

     ("lnatoli@draper.com" "~/prof/grants/2006-01-draper/mail.mail")

     ("suresh.srinivas@intel.com" "~/prof/grants/2006-01-intel/mail.mail")

     ("kandasamy@cbis.ece.drexel.edu" "~/prof/grants/2006-11-muri/mail.mail")

     ("dcr4f@cs.virginia.edu" "~/prof/grants/2006-01-nsf-sod/mail.mail")

     ("dawson@csl.sri.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
     ("JFS1313@aol.com" "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")

     ;; This rule comes last in the grants section
     ;; ("achow@ai.mit.edu" "~/prof/grants/spending.mail")
     ("debideng@ai.mit.edu" "~/prof/grants/spending.mail")

     ;; jikes
     ("ebb9@email.byu.edu" "~/bin/bugs/bug-jikes.mail")
     ("noreply@oss.lotus.com" "~/bin/bugs/bug-jikes.mail")
     ("cabbey@bresnanlink.net" "~/bin/bugs/bug-jikes.mail")
     ("cabbey@chartermi.net" "~/bin/bugs/bug-jikes.mail")
     ("noreply@www-124.southbury.usf.ibm.com" "~/bin/bugs/bug-jikes.mail")

     ;; other
     ("amichail@cse.unsw.edu.au" "~/corr/notkin.mail")
     ("Cathy.Tuttle@seattle.gov" "~/corr/notkin.mail")
     ("cathy.tuttle@gmail.com" "~/corr/notkin.mail")

     ;; Program committees (conferences)
     ;; ("alex.garthwaite@sun.com" "~/prof/referee/nepls/mail.mail")
     ;; ("alex@enkidu.East.Sun.COM" "~/prof/referee/nepls/mail.mail")
     ;; ("alex@east.sun.com" "~/prof/referee/nepls/mail.mail")
     ;; ("talks@nepls.org" "~/prof/referee/nepls/submissions.mail")
     ;; ("fse02-pc@cs.ucsd.edu" "~/prof/referee/fse-2002/mail.mail")
     ;; ("fse10-pc@borbala.com" "~/prof/referee/fse-2002/mail.mail")
     ;; ("fse@borbala.com" "~/prof/referee/fse-2002/mail.mail")
     ;; ("paste02-pc@cs.purdue.edu" "~/prof/referee/paste-2002/mail.mail")
     ;; ("paste02-admin@borbala.com" "~/prof/referee/paste-2002/mail.mail")
     ;; ("havelund@email.arc.nasa.gov" "~/prof/referee/rv/mail.mail")
     ;; ("jcook@cs.nmsu.edu" "~/prof/referee/woda-2003/mail.mail")
     ;; ("Raimondas.Lencevicius@nokia.com" "~/prof/referee/woda-2004/mail.mail")
     ;; ("woda-organizers@cs.virginia.edu" "~/prof/referee/woda-2004/mail.mail")
     ;; ("andrews@csd.uwo.ca" "~/prof/referee/woda-2005/mail.mail")
     ;; ("pollock@cis.udel.edu" "~/prof/referee/woda-2005/mail.mail")
     ;; ("WODA06-PC" "~/prof/referee/woda-2006/mail.mail")
     ;; ("Jacky.Estublier@imag.fr" "~/prof/referee/icse-2004/mail.mail")
     ;; ("icse2004pc@borbala.com" "~/prof/referee/icse-2004/mail.mail")
     ;; ("cormac@cs.ucsc.edu" "~/prof/referee/paste-2004/mail.mail")
     ;; ("paste04-pc@soe.ucsc.edu" "~/prof/referee/paste-2004/mail.mail")
     ;; ("paste@st.cs.uni-sb.de" "~/prof/referee/paste-2004/mail.mail")
     ;; ("abadi@soe.ucsc.edu" "~/prof/referee/popl-2005/mail.mail")
     ;; ("abadi@cs.ucsc.edu" "~/prof/referee/popl-2005/mail.mail")
     ;; ("oege.demoor@magdalen.oxford.ac.uk" "~/prof/referee/etx-2004/mail.mail")
     ;; ("Oege.de.Moor@comlab.ox.ac.uk" "~/prof/referee/etx-2004/mail.mail")
     ;; ("oege@comlab.ox.ac.uk" "~/prof/referee/etx-2004/mail.mail")
     ;; ("brian@bedarra.com" "~/prof/referee/etx-2004/mail.mail")
     ;; ("bardb@us.ibm.com" "~/prof/referee/ibmplday-2004/mail.mail")
     ;; ("shankar@csl.sri.com" "~/prof/referee/ifip-verification/mail.mail")
     ;; ("verified@se.inf.ethz.ch" "~/prof/referee/ifip-verification/mail.mail")
     ;; ("paste2005@lists.csail.mit.edu" "~/prof/referee/paste-2005/mail.mail")
     ;; ("jensen@irisa.fr" "~/prof/referee/paste-2005/mail.mail")
     ;; ("baresi@elet.polimi.it" "~/prof/referee/paste-2005/mail.mail")
     ;; ("peters@paperdyne.com" "~/prof/referee/paste-2005/mail.mail")
     ;; ("co@di.fct.unl.pt" "~/prof/referee/paste-2005/mail.mail")
     ;; ("pezze@disco.unimib.it" "~/prof/referee/issta-2006/mail.mail")
     ;; ("cmt@microsoft.com" "~/prof/referee/bugs05/mail.mail")
     ;; ("cc2006pc@st.cs.uni-sb.de" "~/prof/referee/cc-2006/mail.mail")
     ;; ("dave@bedarra.com" "~/prof/referee/ecoop-2006/mail.mail")
     ;; ("ecoop2006pc@emn.fr" "~/prof/referee/ecoop-2006/mail.mail")
     ;; ("ecooppc2006@emn.fr" "~/prof/referee/ecoop-2006/mail.mail")
     ;; ("Jacques.Malenfant@lip6.fr" "~/prof/referee/ecoop-2006/mail.mail")
     ;; ("PLDI <tball@microsoft.com>" "~/prof/referee/pldi-2006/mail.mail")
     ("tcl@site.uottawa.ca" "~/prof/referee/icse-2007-education/mail.mail")
     ("icse2007-education-chairs@borbala.com" "~/prof/referee/icse-2007-education/mail.mail")
     ;; ("antonia.bertolino@isti.cnr.it" "~/prof/referee/esecfse-2007/mail.mail")
     ("esecfse07pc@isti.cnr.it" "~/prof/referee/esecfse-2007/mail.mail")
     ("issta08pc@st.cs.uni-sb.de" "~/prof/referee/issta-2008/mail.mail")
     ("issta2008@easychair.org" "~/prof/referee/issta-2008/mail.mail")
     ("woda2009@easychair.org" "~/prof/referee/woda-2009/mail.mail")

     ;; Reviewing
     ("toplas@cs.wustl.edu" "~/prof/referee/toplas/toplas.mail")
     ("toplas@cse.wustl.edu" "~/prof/referee/toplas/toplas.mail")
     ;; NSF panels
     ;; ("cwoods@nsf.gov" "~/prof/referee/nsf-career-2002/career-2002.mail")
     ("fanger@nsf.gov" "~/prof/grants/nsf-misc/nsf-funding.mail")
     ("Open.competition.cs@NWO.NL" "~/prof/referee/nwo/mail.mail")
     ;; Refereeing software
     ("stadt@borbala.com" "~/bin/bugs/bug-cyberchair.mail")
     ("cyberchair_info@borbala.com" "~/bin/bugs/bug-cyberchair.mail")
     ("tse@computer.org" "~/prof/referee/tse/tse.mail")

     ("irenelee@MIT.EDU" "~/prof/referee/swe-scholarship.mail")

     ;; Grants, but these come after etx-2004

     ("cmorris@ca.ibm.com" "~/prof/grants/eclipse/mail.mail")
     ("awards@us.ibm.com" "~/prof/grants/eclipse/mail.mail")
     ("scottf@us.ibm.com" "~/prof/grants/eclipse/eclipse-course/mail.mail")
     ("gabbys@us.ibm.com" "~/prof/grants/eclipse/eclipse-course/mail.mail")

     ("csail-pi@lists.csail.mit.edu" "~/class/admin/pi-list.mail")

     ;;; Administration

     ;; Sysadmin
     ;; UW
     ("support@cs.washington.edu" "~/corr/sysadmin.mail")
     ("webmaster@cs.washington.edu" "~/bin/bugs/bug-webpages.mail")
     ("rose@cs.washington.edu" "~/bin/bugs/bug-webpages.mail")
     ("burr@cs.washington.edu" "~/corr/sysadmin.mail")
     ("rprieto@cs.washington.edu" "~/corr/sysadmin.mail")
     ("timss@cs.washington.edu" "~/corr/sysadmin.mail")
     ("oystr@cs.washington.edu" "~/corr/sysadmin.mail")
     ;; TIG/CRS
     ("tig@csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("rt@tig.csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("ftilley@\\(lcs\\|csail\\).mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("mpearrow@\\(csail\\|ai\\).mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("jon@csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("dpo@csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("help@csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("software@csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("wollman@khavrinen.lcs.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("afs-hackers@csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("bug-certificates@csail.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ("bug-inquir@CSAIL.MIT.EDU" "~/corr/sysadmin.mail" mernst-csail-address)
     ("bug-inquir@mintaka.lcs.mit.edu" "~/corr/sysadmin.mail" mernst-csail-address)
     ;; Techsquare
     ("greg@ts.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("greg@techsquare.com" "~/corr/sysadmin.mail")
     ("gshomo@\\(pag\\|geyer\\|theory\\).lcs.mit.edu" "~/corr/sysadmin.mail")
     ("sdg-admin@techsquare.com" "~/corr/sysadmin.mail")
     ("sdg-admin@geyer.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("pag-admin@techsquare.com" "~/corr/sysadmin.mail")
     ("pag-admin@\\(pag\\.\\|geyer\\.\\)?\\(csail\\|lcs\\).mit.edu" "~/corr/sysadmin.mail")
     ("sb@techsquare.com" "~/corr/sysadmin.mail")
     ("ang@theory.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("ang@techsquare.com" "~/corr/sysadmin.mail")
     ("matt@theory.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("admin@theory.csail.mit.edu" "~/corr/sysadmin.mail")
     ;; Others
     ("kpost@cag.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("kent@futura.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("kent@akamai.com" "~/corr/sysadmin.mail")
     ("admin@futura.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("whj@cs.washington.edu" "~/corr/sysadmin.mail")
     ("admin@theory.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("3partysw@mit.edu" "~/corr/sysadmin.mail")
     ("dcurtis@rondo.lcs.mit.edu" "~/corr/sysadmin.mail")
     ("dcurtis@\\(lcs\\|csail\\).mit.edu" "~/corr/sysadmin.mail")
     ("kedar@asacomputers.com" "~/corr/sysadmin.mail")
     ("quinlan@rulequest.com" "~/corr/sysadmin.mail")
     ;; Athena OLC
     ("olc@mit.edu" "~/corr/sysadmin.mail")
     ("olc-unix@mit.edu" "~/corr/sysadmin.mail")
     ("daemon@matisse.mit.edu" "~/corr/sysadmin.mail")
     ;; MIT property office
     ("rschiav@mit.edu" "~/corr/sysadmin.mail")
     ("paulmac@MIT.EDU" "~/corr/sysadmin.mail")

     ;; Travel
     ; UW
     ("tmc@psni.com" "~/corr/travel.mail")
     ; MIT
     ("Donna@tvlcoll.com" "~/corr/travel.mail" mernst-csail-address)
     ("Ginny@tvlcoll.com" "~/corr/travel.mail" mernst-csail-address)
     ; All others
     ("DeltaElectronicTicketReceipt@delta.com" "~/corr/travel.mail" mernst-alum-address)
     ("abratravel@yahoo.com" "~/corr/travel.mail" mernst-alum-address)
     ("asmith@statravel.com" "~/corr/travel.mail" mernst-alum-address)
     ("travelercare@orbitz.com" "~/corr/travel.mail" mernst-alum-address)
     ("flightstatus@orbitz.com" "~/corr/travel.mail" mernst-alum-address)
     ("confirmation@uasupport.com" "~/corr/travel.mail" mernst-alum-address)
     ("UnitedEasycheckin@email.united.com" "~/corr/travel.mail" mernst-alum-address)
     ("NOREPLY@UNITED.COM" "~/corr/travel.mail" mernst-alum-address)
     ("My_NWA_Info@nwa.com" "~/corr/travel.mail" mernst-alum-address)
     ("InstantCheckIn@checkin.info.aa.com" "~/corr/travel.mail" mernst-alum-address)
     ("notify@aa.globalnotifications.com" "~/corr/travel.mail" mernst-alum-address)
     ("travelercare@cheaptickets.com" "~/corr/travel.mail" mernst-alum-address)
     ("mail@jetblueconnect.com" "~/corr/travel.mail" mernst-alum-address)
     ("UnitedEasycheckin@unitedeco.p0.com" "~/corr/travel.mail" mernst-alum-address)

     ;; ("connie@cs.washington.edu" "~/prof/job-search/mail.mail")
     ;; ("vfhs@u.washington.edu" "~/prof/job-search/mail.mail")

     ;; Other
     ("ladd@\\(\\(tower.\\)?lcs\\|csail\\).mit.edu" "~/class/admin/space.mail")
     ("bug-printer@\\(lcs\\|csail\\).mit.edu" "~/class/admin/space.mail")
     ("\\bbob@\\(lcs\\|csail\\).mit.edu" "~/class/admin/tech-square.mail")
     ("oops@csail.mit.edu" "~/class/admin/space.mail")
     ("ops@csail.mit.edu" "~/class/admin/space.mail")
     ("statabugs@csail.mit.edu" "~/class/admin/space.mail")
     ("FACILITIES@MIT.EDU" "~/class/admin/space.mail")
     ("hdlubac@cs.washington.edu" "~/class/admin/space.mail")
     ("tracy@cs.washington.edu" "~/class/admin/space.mail")

     ("ville.skytta@iki.fi" "~/bin/bugs/bug-checklink.mail" mernst-alum-address)
     ("www-validator@w3.org" "~/bin/bugs/bug-checklink.mail" mernst-alum-address)
     ("ot@w3.org" "~/bin/bugs/bug-checklink.mail" mernst-alum-address)

     ("bazaar@lists.canonical.com" "~/bin/bugs/bug-bazaar.mail" mernst-alum-address)

     ;; Exception to the "webmaster@" default rule.
     ("webmaster@peertropolis.de" "~/research/types/mail/jsr305.mail")
     ("webmaster@" "~/bin/bugs/bug-webpages.mail")

     ;;; Academics

     ("mreardon@cs.washington.edu" "~/class/curriculum/curriculum.mail")

     ;; 403 appears below

     ;; MIT Classes
     ; 6.033
     ("6.033-staff@mit.edu" "~/class/6033/mail.mail")
     ("6.033-sec[0-9]+@mit.edu" "~/class/6033/mail.mail")
     ("6.033-students@mit.edu" "~/class/6033/mail.mail")
     ("6.033-tas@mit.edu" "~/class/6033/mail.mail")
     ("6.033-staff-s2004@mit.edu" "~/class/6033/mail.mail")
     ("ctl@mit.edu" "~/class/6033/mail.mail")
     ("kchen25@mit.edu" "~/class/6033/mail.mail")
     ("jastr@mit.edu" "~/class/6033/mail.mail")
     ("vitpv@mit.edu" "~/class/6033/mail.mail")
     ("jat_mew@mit.edu" "~/class/6033/mail.mail")
     ;; ("yipal@mit.edu" "~/class/6033/mail.mail")
     ;; ("jsalz@mit.edu" "~/class/6033/mail.mail")
     ;; ("cyli@mit.edu" "~/class/6033/mail.mail")
     ;; ("saltzer@mit.edu" "~/class/6033/mail.mail")
     ;; ("soyini@mit.edu" "~/class/6033/mail.mail")
     ;; ("murali@MIT.EDU" "~/class/6033/mail.mail")
     ; 6.170
     ("ajfox@MIT.EDU" "~/class/170/mail/sysadmin.mail")
     ("6.170-motd@mit.edu" "~/class/170/mail/motd.mail")
     ("6.170-las@mit.edu" "~/class/170/mail/labs.mail")
     ("6.170-ac-tas@MIT.EDU" "~/class/170/mail/project.mail")
     ("6.170-gb-tas@MIT.EDU" "~/class/170/mail/project.mail")
     ("6.170-antichess@MIT.EDU" "~/class/170/mail/project.mail")
     ("6.170-gb@MIT.EDU" "~/class/170/mail/project.mail")
     ("arashf@MIT.EDU" "~/class/170/mail/project.mail")
     ("beth.simon@gmail.com" "~/class/170/mail/presenter.mail")
     ("esimon@cs.ucsd.edu" "~/class/170/mail/presenter.mail")
     ("board@forum6170.csail.mit.edu" "~/class/170/mail/forum.mail")
     ("6\\.170 Forum" "~/class/170/mail/forum.mail")
     ("\"6.170 Software Engineering Lab\" <vikki@mit.edu>" "~/class/170/mail/forum.mail")
     ;; Just to get the directory right.
     ;; This helped in RMAIL, but VM doesn't respect the directory.
     ("6.170" "~/class/6170/mail/must-set")
     ("nsanch@MIT.EDU" "~/class/170/mail/must-set")
     ("hofma@ai.mit.edu" "~/class/170/mail/must-set")
     ("stefie10@ai.mit.edu" "~/class/170/mail/must-set")
     ("eugene@\\(lcs\\|csail\\).mit.edu" "~/class/170/mail/must-set")
     ("eishih@MIT.EDU" "~/class/170/mail/must-set")
     ;; ("noto@MIT.EDU" "~/class/170/mail/must-set")
     ("jenlouie@MIT.EDU" "~/class/170/mail/must-set")
     ("stefie10@MIT.EDU" "~/class/170/mail/must-set")
     ("paulfitz@csail.MIT.EDU" "~/class/170/mail/mail.mail")
     ("mbolin@MIT.EDU" "~/class/admin/sysadmin.mail")
     ("natan@MIT.EDU" "~/class/admin/sysadmin.mail")
     ("jamesta3@mit.edu" "~/class/170/mail/physics3d.mail")
     ("arashf@MIT.EDU" "~/class/170/mail/physics3d.mail")
     ; 6.821
     ("6.821-staff@psrg.lcs.mit.edu" "~/class/6821/mail.mail")
     ("6.821-staff@geyer.lcs.mit.edu" "~/class/6821/mail.mail")
     ("6.821-forum@psrg.lcs.mit.edu" "~/class/6821/mail.mail")
     ("6.821-students@psrg.lcs.mit.edu" "~/class/6821/mail.mail")
     ("6821-staff@csail.mit.edu" "~/class/6821/mail.mail")
     ("6821-students@csail.mit.edu" "~/class/6821/mail.mail")
     ("gjw@theory.lcs.mit.edu" "~/class/6821/mail.mail")
     ("gjw@mit.edu" "~/class/6821/mail.mail")
     ("darling@mit.edu" "~/class/6821/mail.mail")
     ("sheldon@psrg.lcs.mit.edu" "~/class/6821/mail.mail")
     ("sheldon@alum.mit.edu" "~/class/6821/mail.mail")
     ("adamle@mit.edu" "~/class/6821/mail.mail")
     ("farnaz@mit.edu" "~/class/6821/mail.mail")
     ("cadlerun@csail.mit.edu" "~/class/6821/mail.mail")
     ; 6.893
     ;; ("jksrini@MIT.EDU" "~/class/6893/mail/mail.mail")
     ; 6.897
     ("6.897-students@theory.lcs.mit.edu" "~/class/6897/mail.mail")
     ("6897-students@theory.lcs.mit.edu" "~/class/6897/mail.mail")
     ; 6.883
     ("6883@csail.mit.edu" "~/class/6883/mail.mail")
     ; German class (21F.40X)
     ("djaeger@MIT.EDU" "~/class/21F401/mail.mail")
     ; Spanish class (21F.70X)
     ("ramosjo@MIT.EDU" "~/class/21F701/mail.mail")
     ("einatlev@mit.edu" "~/class/21F701/mail.mail")
     ("rgessa@MIT.EDU" "~/class/21F701/mail.mail")
     ("dmorgen@mit.edu" "~/class/21F701/mail.mail")
     ("adolfo@mag.upv.es" "~/class/21F701/mail.mail")
     ; BCAE
     ("greg.charest@gmail.com" "~/class/bcae.mail" mernst-alum-address)
     ("kzevallos99@hotmail.com" "~/class/bcae.mail" mernst-alum-address)
     ("lc1958@gmail.com" "~/class/bcae.mail" mernst-alum-address)
     ("venktesh_ramnath@yahoo.com" "~/class/bcae.mail" mernst-alum-address)
     ("registration@bcae.org" "~/class/bcae.mail" mernst-alum-address)

     ("6\.?370@mit.edu" "~/class/6370/mail.mail")
     ("6.370-chair@mit.edu" "~/class/6370/mail.mail")
     ("6.370-judges@mit.edu" "~/class/6370/mail.mail")
     ("Brad.Spiers@morganstanley.com" "~/class/6370/mail.mail")
     ;; ("ppham@\\(colony.\\)?mit.edu" "~/class/6370/mail.mail")
     ("aiba@MIT.EDU" "~/class/6370/mail.mail")
     ("dgreensp@MIT.EDU" "~/class/6370/mail.mail")


     ("educue@educue.com" "~/class/upop-dir/upop-2006/software.mail")
     ("@gtcocalcomp.com" "~/class/upop-dir/upop-2006/software.mail")
     ("sikoscow@mit.edu" "~/class/upop-dir/upop-2006/software.mail")
     ("jh@bcpi.com" "~/class/upop-dir/upop-2006/software.mail")
     ("mgebauer@\\(csail\\.\\)?mit.edu" "~/class/upop-dir/upop-2006/software.mail")
     ("dmbiondi@MIT.EDU" "~/class/upop/mail.mail")
     ("cresto@MIT.EDU" "~/class/upop/mail.mail")
     ("emarnold@MIT.EDU" "~/class/upop/mail.mail")
     ("luperfoy@mit.edu" "~/class/upop/mail.mail")
     ("jessicaj@MIT.EDU" "~/class/upop/mail.mail")
     ("upop@mit.edu" "~/class/upop/mail.mail")

     ;; Students
     ("rappts@eecs.mit.edu" "~/class/admin/student-support.mail")
     ("as_cron@sky-app-1.mit.edu" "~/class/admin/student-support.mail")
     ("sfs-it@mit.edu" "~/class/admin/student-support.mail")
     ;; Orna Raz before Daikon students, to override them.
     ("orna.raz@cs.cmu.edu" "~/class/advisees/orna-raz.mail")
     ("orna_raz@yahoo.com" "~/class/advisees/orna-raz.mail")
     ("orna@pelleg.org" "~/class/advisees/orna-raz.mail")
     ("ornar@pelleg.org" "~/class/advisees/orna-raz.mail")

     ("karine.arnout@inf.ethz.ch" "~/class/advisees/karine-arnout.mail")
     ("karine_arnout@hotmail.com" "~/class/advisees/karine-arnout.mail")

     ;; Daikon users
     ;; ("orna.raz@cs.cmu.edu" "~/research/invariants/mail/users.mail")
     ("BWang@Empirix.com" "~/research/invariants/mail/users.mail")
     ("daveho@cs.umd.edu" "~/research/invariants/mail/users.mail")
     ("hubbers@sci.kun.nl" "~/research/invariants/mail/users.mail")
     ("DMILLER9@PARTNERS.ORG" "~/research/invariants/mail/users.mail")

     ;; Daikon students
     ;; ("gustavo@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("jwa@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("emarcus@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("dgarg@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("amdunn@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("blwang@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("rbarrows@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("vkm@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/temporal.mail")
     ;; ("dodoo@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("akcabac@\\(pag.\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("smeghani@\\(\\(pag\\|geyer\\).\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ;; ("nii.dodoo@oracle.com" "~/research/invariants/mail/students.mail")
     ("jwnimmer@\\(lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ("mistere@mit.edu" "~/research/invariants/mail/students.mail")
     ("scruffy@\\(\\(\\(pag\\|geyer\\|theory\\).\\)?lcs.\\)?mit.edu" "~/research/invariants/mail/c-front-end.mail")
     ("scruffy@corgi-power.com" "~/research/invariants/mail/c-front-end.mail")
     ("jsa@edg.com" "~/research/invariants/mail/c-front-end.mail")
     ("mharder@\\(\\(geyer.\\)?lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ("leelin@\\(\\(pag\\|geyer\\).\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/steering.mail")
     ("t-leelin@microsoft.com" "~/research/invariants/mail/students.mail")
     ("leelin@gmail.com" "~/research/invariants/mail/students.mail")
     ("lee@leelin.com" "~/research/invariants/mail/students.mail")
     ("jeffm@\\(\\(pag\\|geyer\\).\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ("minilek@\\(pag.\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ("ten3@u.washington.edu" "~/research/invariants/mail/students.mail")
     ("ten3@csail.mit.edu" "~/research/invariants/mail/students.mail")
     ("pgbovine@\\(\\(pag.\\)?\\(csail\\|lcs\\|alum\\).\\)?mit.edu" "~/research/invariants/mail/c-front-end.mail")
     ;; ("pg@cs.stanford.edu" "~/research/invariants/mail/c-front-end.mail")
     ("charlest@\\(\\(pag.\\)?\\(csail\\|alum\\).\\)?mit.edu" "~/research/invariants/mail/c-front-end.mail")
     ("rudd@\\(\\(pag.\\)?\\(csail\\|alum\\).\\)?mit.edu" "~/research/invariants/mail/c-front-end.mail")
     ("gpickard@\\(pag.\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/steering.mail")
     ("mao@\\(pag.\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/steering.mail")
     ("chenx05@\\(pag.\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ("felly@\\(csail\\.\\)?mit.edu" "~/research/invariants/mail/java-front-end.mail")
     ("brun@\\(\\(pag\\|geyer\\).\\(csail\\|lcs\\).\\|alum.\\)?mit.edu" "~/class/advisees/brun.mail")
     ("ybrun@usc.edu" "~/class/advisees/brun.mail")
     ("brun@cs.washington.edu" "~/class/advisees/brun.mail")
     ("kshih@MIT.EDU" "~/research/invariants/mail/machlearn.mail")
     ("jwyuan@\\(csail\\.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ("juang@\\(\\(pag.\\)?\\(csail\\).\\)?mit.edu" "~/research/invariants/mail/students.mail")

     ;; Move this above Mahmood; when bothe are copied, its likely about Yoav's work
     ("Alex.Potanin@mcs.vuw.ac.nz" "~/class/advisees/yoav-zibin.mail")
     ("paleyli@hotmail.com" "~/class/advisees/yoav-zibin.mail")
     ("zyoav2000@yahoo.com" "~/class/advisees/yoav-zibin.mail")
     ("yoav@come2play.com" "~/class/advisees/yoav-zibin.mail")
     ("yoav.zibin@gmail.com" "~/class/advisees/yoav-zibin.mail")
     ("yoav@zibin.net" "~/class/advisees/yoav-zibin.mail")
     ("zyoav@csail.mit.edu" "~/class/advisees/yoav-zibin.mail")

     ;; JSR 308: annotation syntax
     ("jsr308-edr@lists.csail.mit.edu" "~/research/types/mail/language.mail")
     ("jsr308-discuss@googlegroups.com" "~/research/types/mail/language.mail")
     ("langtools-group@Sun.COM" "~/research/types/mail/language.mail")
     ("cdr@intellij.com" "~/research/types/mail/language.mail")
     ("mr@sun.com" "~/research/types/mail/language.mail")
     ;; Pluggable type-checkers
     ("checker-framework-dev@googlegroups.com" "~/research/types/mail/checkers.mail")
     ("checkers@lists.csail.mit.edu" "~/research/types/mail/checkers.mail")
     ("daniel.wand@gmail.com" "~/research/types/mail/checkers.mail")
     ("subanark@gmail.com" "~/research/types/mail/checkers.mail")
     ("adamwtw@gmail.com" "~/research/types/mail/checkers.mail")
     ("adamw@fusemail.com" "~/research/types/mail/checkers.mail")
     ("adam@warski.org" "~/research/types/mail/checkers.mail")
     ("fausto.spoto@univr.it" "~/research/types/mail/checkers.mail")
     ("spoto@cs.washington.edu" "~/research/types/mail/checkers.mail")
     ("futeboleo@yahoo.com" "~/research/types/mail/checkers.mail")
     ("malaguy@gmail.com" "~/research/types/mail/checkers.mail")
     ("mjollnir@cs.washington.edu" "~/research/types/mail/checkers.mail")
     ("laurent.hubert@irisa.fr" "~/research/types/mail/checkers.mail")
     ("nit-bugs@gforge.inria.fr" "~/research/types/mail/checkers.mail")
     ("nit-featurerequests@gforge.inria.fr" "~/research/types/mail/checkers.mail")
     ("stephan.heiss@gmail.com" "~/research/types/mail/checkers.mail")
     ;; "annotations.mail" doesn't exist any more, but that default does
     ;; set the directory correctly.
     ("ted@tedneward.com" "~/research/types/mail/annotations.mail")
     ("Danny.Coward@sun.com" "~/research/types/mail/annotations.mail")
     ("Alex.Buckley@Sun.COM>" "~/research/types/mail/annotations.mail")
     ("eu@javatx.org" "~/research/types/mail/annotations.mail")
     ("gafter@google.com" "~/research/types/mail/annotations.mail")
     ("trevor@vocaro.com" "~/research/types/mail/annotations.mail")
     ("mpapi@\\(csail.\\)?mit.edu" "~/research/types/mail/annotations.mail")
     ("jsr308\\(-statements\\|-bugs\\)?@\\(lists.\\)?csail.mit.edu" "~/research/types/mail/annotations.mail")
     ("Paul.Rank@sun.com" "~/research/types/mail/annotations.mail")
     ("max.lanfranconi@Sun.COM" "~/research/types/mail/annotations.mail")
     ("Jonathan.Gibbons@Sun.COM" "~/research/types/mail/annotations.mail")
     ("torbjorn@comlab.ox.ac.uk" "~/research/types/mail/annotations.mail")
     ("torbjorn.ekman@comlab.ox.ac.uk" "~/research/types/mail/annotations.mail")
     ("torbjorn.ekman@gmail.com" "~/research/types/mail/annotations.mail")
     ("viq@MIT.EDU" "~/research/types/mail/annotations.mail")
     ;; JSR 305: annotation semantics
     ("jsr-305@\\(mcfeely1.\\)?cs.umd.edu" "~/research/types/mail/jsr305.mail")
     ("jsr-305-bounces@cs.umd.edu" "~/research/types/mail/jsr305.mail")
     ("JSR-305-EG@JCP.ORG" "~/research/types/mail/jsr305.mail")
     ("jsr-305@googlegroups.com" "~/research/types/mail/jsr305.mail")
     ;; Typequals: after JSR 308 so that JSR 308 takes precedence
     ("typequals@csail.mit.edu" "~/research/types/mail/javari.mail")
     ("tschantz@\\(\\(pag.\\(csail\\|lcs\\).\\)?mit.edu\\|gmail.com\\|cs.berkeley.edu\\)" "~/research/types/mail/javari.mail")
     ("jaimeq@\\(\\(pag.\\)?csail.\\)?mit.edu" "~/research/types/mail/javari.mail")
     ("hashproduct\\(\\+csail\\)?@\\(verizon.net\\|gmail.com\\|csail.mit.edu\\)" "~/research/types/mail/javari.mail")
     ("jsendova@MIT.EDU" "~/research/types/mail/javari.mail")
     ("pramook@\\(csail\\.\\)?mit.edu" "~/research/types/mail/javari.mail")
     ("mahmood@MIT.EDU" "~/research/types/mail/checkers.mail")
     ("mali@csail.mit.edu" "~/research/types/mail/checkers.mail")
     ("msaeed43@gmail.com" "~/research/types/mail/checkers.mail")
     ("mgowri@in.ibm.com" "~/research/types/mail/checkers.mail")
     ("ndonuora@mit.edu" "~/research/types/mail/javari.mail")
     ("viq@mit.edu" "~/research/types/mail/javari.mail")
     ("typequals@lists.csail.mit.edu" "~/research/types/mail/javari.mail")
     ("Onno.Kluyt@Sun.COM" "~/research/types/mail/javari.mail")
     ;; ("max@jetbrains.com" "~/research/types/mail/javari.mail") ; JSR expert group
     ("chris@neko.net.nz" "~/research/types/mail/javari.mail") ; James Noble pluggable types
     ("dgreenfi@cs.umd.edu" "~/research/types/mail/javari.mail")
     ("telmo@\\(csail\\.\\)?mit.edu" "~/research/types/mail/javari.mail")
     ("palesandbuckets@gmail.com" "~/research/types/mail/javari.mail")

     ;; Carlos gets prioritized before Tao
     ("cpacheco@\\(\\(pag.\\)?\\(csail\\|lcs\\).\\)?mit.edu" "~/class/advisees/cpacheco.mail")
     ("carlos.pche@gmail.com" "~/class/advisees/cpacheco.mail")
     ("eclat-discussion@lists.csail.mit.edu" "~/class/advisees/cpacheco.mail")
     ("damorim@cs.uiuc.edu" "~/class/advisees/cpacheco.mail")
     ("damorim@csail.mit.edu" "~/class/advisees/cpacheco.mail")
     ("Shuvendu.Lahiri@microsoft.com" "~/class/advisees/cpacheco.mail")
     ("hill@cis.udel.edu" "~/class/advisees/cpacheco.mail")
     ("taoxie@cs.washington.edu" "~/research/invariants/mail/taoxie.mail")

     ;; IOA
     ("gsantos@mit.edu" "~/research/invariants/mail/students.mail")
     ("tohn@\\(\\(pag\\|geyer\\).\\(csail\\|lcs\\).\\)?mit.edu" "~/research/invariants/mail/students.mail")

     ;; Eclipse plug-in (prioritize before Jeff Perkins and others)
     ("cok@frontiernet.net" "~/research/invariants/mail/eclipse.mail")
     ("david.cok@kodak.com" "~/research/invariants/mail/eclipse.mail")

     ;; non-Daikon, non-typequalifiers students
     ;; ("ftliu@mit.edu" "~/class/advisees/ftliu.mail")
     ;; ("arolfe@mit.edu" "~/class/advisees/arolfe.mail")
     ;; Adrian Birka appears at top of list
     ;; ("adbirka@\\(alum.\\)?mit.edu" "~/class/advisees/adbirka.mail")
     ;; ("rseater@haverford.edu" "~/class/advisees/rseater.mail")
     ;; ("rseater@\\(\\(\\(pag\\|geyer\\).\\)?\\(lcs\\|csail\\).\\)?mit.edu" "~/class/advisees/rseater.mail")
     ;; ("newton@mit.edu" "~/class/advisees/newton.mail")
     ;; ("iuliuv@\\(\\(pag\\|geyer\\).\\(csail\\|lcs\\).\\)?mit.edu" "~/class/advisees/iuliuv.mail")
     ;; ("iuliuv@computer.org" "~/class/advisees/iuliuv.mail")
     ;; ("iuliuv@xnet.ro" "~/class/advisees/iuliuv.mail")
     ;; ("iuliu.vasilescu@distinto.ro" "~/class/advisees/iuliuv.mail")
     ;; ("punya@\\(pag.\\(csail\\|lcs\\).\\)?mit.edu" "~/class/advisees/adonovan.mail")
     ;; ("mhao@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/old-students.mail")
     ;; ("wanchung@\\(geyer.lcs.\\)?mit.edu" "~/research/invariants/mail/students.mail")
     ("lashari@hotmail.com" "~/class/advisees/adbirka.mail")
     ("ghulam@sgi.com" "~/class/advisees/adbirka.mail")
     ("drayside@swen.uwaterloo.ca" "~/class/advisees/drayside.mail")
     ;; ("drayside@\\(\\(\\(pag\\|geyer\\).\\)?\\(lcs\\|csail\\).\\)?mit.edu" "~/class/advisees/drayside.mail")
     ("saff@\\(csail.\\)?MIT.EDU" "~/class/advisees/saff.mail")
     ("david.saff@gmail.com" "~/class/advisees/saff.mail")
     ("AVI@zurich.ibm.com" "~/class/advisees/saff.mail")
     ("continuous-testing-plugin-discuss-request@lists.csail.mit.edu" "~/class/advisees/saff.mail")
     ("continuous-testing-plugin-discuss@lists.csail.mit.edu" "~/class/advisees/saff.mail")
     ("kcheval@\\(pag.\\(csail\\|lcs\\).\\)?mit.edu" "~/class/advisees/saff.mail")
     ("mbridge@mit.edu" "~/class/advisees/saff.mail")
     ("mbridge@gmail.com" "~/class/advisees/saff.mail")
     ("adayal@mit.edu" "~/class/advisees/saff.mail")
     ("adonovan@\\(\\(pag\\|geyer\\).\\)?\\(lcs\\|csail\\).mit.edu" "~/class/advisees/archive/adonovan.mail")
     ("alan.donovan@arm.com" "~/class/advisees/archive/adonovan.mail")
     ("adonovan@google.com" "~/class/advisees/archive/adonovan.mail")
     ("amy@\\(\\(cag.\\)?\\(lcs\\|csail\\).\\)?mit.edu" "~/class/advisees/archive/amy-williams.mail")
     ("alw@mit.edu" "~/class/advisees/amy-williams.mail")
     ("artzi@rafael.co.il" "~/class/advisees/shay-artzi.mail")
     ("artzi@\\(csail\\.\\)?MIT.EDU" "~/class/advisees/shay-artzi.mail")
     ("sartzi@us.ibm.com" "~/class/advisees/shay-artzi.mail")
     ("Shay.Artzi@gmail.com" "~/class/advisees/shay-artzi.mail")
     ("adam.kiezun@gmx.net" "~/class/advisees/kiezun.mail")
     ("Adam_Kiezun@oti.com" "~/class/advisees/kiezun.mail")
     ("akiezun@\\(\\(\\(pag\\|rozz\\).\\)?\\(csail\\|lcs\\).\\)?mit.edu" "~/class/advisees/kiezun.mail")
     ("akiezun@us.ibm.com" "~/class/advisees/kiezun.mail")
     ("akiezun@gmail.com" "~/class/advisees/kiezun.mail")
     ("dharv720@mit.edu" "~/class/advisees/kiezun.mail")
     ;; Dave Glasser comes after Shay Artzi and Adam Kiezun
     ("glasser@\\(\\(csail\\.\\)?mit.edu\\|davidglasser.net\\)" "~/class/advisees/glasser.mail")
     ("mdb@cs.washington.edu" "~/class/advisees/mdb.mail")
     ("tws@cs.washington.edu" "~/class/advisees/tws.mail")
     ("racezhang@gmail.com" "~/class/advisees/sai-zhang.mail")
     ("szhang@cs.washington.edu" "~/class/advisees/sai-zhang.mail")
     ("csgordon@cs.washington.edu" "~/class/advisees/colin-gordon.mail")
     ("ahunter@cs.hmc.edu" "~/class/advisees/ahunter.mail")


     ("garland@\\(pag\\|theory\\.\\)?\\(lcs\\|csail\\).mit.edu" "~/class/advisees/archive/garland.mail")
     ("asgarl@csail.mit.edu" "~/class/advisees/archive/garland.mail")
     ("sjl@csail.mit.edu" "~/class/advisees/archive/sjl.mail")
     ("sjloosemore@frogsonice.com" "~/class/advisees/archive/sjl.mail")
     ("sandra@frogsonice.com" "~/class/advisees/archive/sjl.mail")
     ("ACRAinc@aol.com" "~/class/advisees/archive/sjl.mail")
     ("friends@frogsonice.com" "~/class/advisees/archive/sjl.mail")
     ("sramer@post.harvard.edu" "~/class/advisees/archive/sjl.mail")
     ("sarahjramer@yahoo.com" "~/class/advisees/archive/sjl.mail")
     ("bdemsky@\\(csail\\.\\)?mit.edu" "~/class/advisees/archive/brian-demsky.mail")
     ("bdemsky@uci.edu" "~/class/advisees/archive/brian-demsky.mail")
     ("hunkim@gmail.com" "~/class/advisees/sung-kim.mail")
     ("hunkim@csail.mit.edu" "~/class/advisees/sung-kim.mail")
     ("mips942@gmail.com" "~/class/advisees/sung-kim.mail")
     ("ygg@soe.ucsc.edu" "~/class/advisees/sung-kim.mail")
     ("bug_talks@googlegroups.com" "~/class/advisees/sung-kim.mail")
     ("recrash@googlegroups.com" "~/class/advisees/sung-kim.mail")
     ("gregs@sulliwood.org" "~/class/advisees/greg-sullivan.mail")
     ;; More senior people go at end, because they're more likely to be
     ;; cc:ed on topics that most relate to others.
     ("smcc@CS\\(UA\\)?.Berkeley.EDU" "~/class/advisees/smcc.mail")
     ("smcc@\\(\\(pag.\\|geyer.\\)?\\(lcs\\|csail\\).\\|csail\\.\\)?mit.edu" "~/class/advisees/smcc.mail")
     ("jhp@\\(pag\\.\\)?\\(lcs\\|csail\\).mit.edu" "~/class/advisees/jhp.mail")
     ("jhp@wwnez.com" "~/class/advisees/jhp.mail")

     ("dannydig@gmail.com" "~/class/advisees/danny-dig.mail")
     ("dig@uiuc.edu" "~/class/advisees/danny-dig.mail")
     ("dig@illinois.edu" "~/class/advisees/danny-dig.mail")
     ("dannydig@csail.mit.edu" "~/class/advisees/danny-dig.mail")
     ("monikadig@gmail.com" "~/class/advisees/danny-dig.mail")
     ("marrero@csail.mit.edu" "~/class/advisees/danny-dig.mail")

     ("snowg@cs.washington.edu" "~/research/testing/mutation/mail.mail")

     ("diegog@dc.uba.ar" "~/class/advisees/diego-garbervetsky.mail")

     ("pag@csail.mit.edu" "~/class/advisees/pag.mail")

     ;; IEEE IAP programming contest
     ;; Comment this out because he also belongs in "~/research/invariants/mail/students.mail"
     ;; ("arjunrn@MIT.EDU" "~/class/iap-prog-contest.mail")

     ;; Aero-Astro faculty search
     ("imm@mit.edu" "~/class/admin/faculty-search/aa-faculty-search.mail")
     ("cski@mit.edu" "~/class/admin/faculty-search/aa-faculty-search.mail")
     ("deyst@mit.edu" "~/class/admin/faculty-search/aa-faculty-search.mail")
     ("murman@mit.edu" "~/class/admin/faculty-search/aa-faculty-search.mail")
     ("robins@mit.edu" "~/class/admin/faculty-search/aa-faculty-search.mail")
     ("tpaneth@MIT.EDU" "~/class/admin/faculty-search/aa-faculty-search.mail")

     ("admitscom@ai.mit.edu" "~/class/admin/grad-admissions.mail")
     ("grad-adm03@amsterdam.lcs.mit.edu" "~/class/admin/grad-admissions.mail")
     ("Adm04-comm@amsterdam.lcs.mit.edu" "~/class/admin/grad-admissions.mail")
     ("grad-admin07@pdos.csail.mit.edu" "~/class/admin/grad-admissions.mail")

     ;; Advisees
     ("specialist@altoids.mit.edu" "~/class/admin/ugrad-advising.mail")
     ;; audits
     ("helens@eecs.mit.edu" "~/class/admin/ugrad-advising.mail")
     ("afrazer@mit.edu" "~/class/admin/ugrad-advising.mail")
     ;; The advisees themselves
     ;; Remove, due to 6.170
     ;; ("mbolin@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("tbronder@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("oranje@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("ocarter@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("radascii@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("douglass@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("kdonison@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("sofichan@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("dylanh@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("mike74@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("janetlai@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("lebowitz@\\(tea-party\\.\\)?mit.edu" "~/class/admin/ugrad-advising.mail")
     ("surreal@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("vivienne@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("lentz@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("lentz@media.mit.edu" "~/class/admin/ugrad-advising.mail")
     ("lipon@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("rlipon@sgi.com" "~/class/admin/ugrad-advising.mail")
     ("rikky@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("rahnev@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("krahnev@gmail.com" "~/class/admin/ugrad-advising.mail")
     ("niceguy@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("niceguy5150@hotmail.com" "~/class/admin/ugrad-advising.mail")
     ("alicesuh@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("saunders@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("pneucell@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("erictak@gmail.com" "~/class/admin/ugrad-advising.mail")
     ("etoro@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("Eddroid@aol.com" "~/class/admin/ugrad-advising.mail")
     ("gtw@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("cwurts@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("edmwang@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ;; New crop, starts September 2004
     ("adamrose@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("bhowell@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("dbw@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("eefi@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("kjhollen@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("lavaboy@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("rafael@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("ldraco@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("nizam@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("rccooper@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("rccooper@gmail.com" "~/class/admin/ugrad-advising.mail")
     ("shall07@mit.edu" "~/class/admin/ugrad-advising.mail")
     ("sostler@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("smustin@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("LizardQueenjc@aol.com" "~/class/admin/ugrad-advising.mail")
     ("gbelote@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("bmanley@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("issao@MIT.EDU" "~/class/admin/ugrad-advising.mail")
     ("widagdos@MIT.EDU" "~/class/admin/ugrad-advising.mail")

     ;; When editing this list, also edit mailing list ugrad-advisees in
     ;; ~/private/addresses.tex

     ("conansaunders@alum.mit.edu" "~/prof/recommendations/2002/saunders-200202.mail")
     ("dalexand@mac.com" "~/prof/recommendations/alexander-200509.mail")

     ("pwhesley@csail.mit.edu" "~/class/admin/finances.mail")

     ;; This is after other administrative because she might be copied
     ;; on various mail.
     ("lyall@\\(\\(larch\\|amsterdam\\).\\)?\\(lcs\\|csail\\).mit.edu" "~/corr/admin.mail")
     ("mr@\\(hq.\\)?csail.mit.edu" "~/corr/admin.mail")
     ("mariarebelo@comcast.net" "~/corr/admin.mail")
     ("melody@cs.washington.edu" "~/corr/admin.mail")

     ;; This is after Maria Rebelo, because he is her supervisor and
     ;; mesages on which she is copied may not be about sysadmin issues.
     ("jackc@\\(csail\\|ai\\).mit.edu" "~/corr/sysadmin.mail")


     ("prod_sfao@mitsis.mit.edu" "~/class/admin/student-support.mail")
     ("OWNER-SAP001-L@MITVMA.MIT.EDU" "~/class/admin/student-support.mail")
     ("lohehir@mitsis.mit.edu" "~/class/admin/student-support.mail")
     ("karen@hq.lcs.mit.edu" "~/class/admin/student-support.mail")

     ("benefits-www@mit.edu" "~/class/admin/benefits.mail")
     ("asi@asiflex.com" "~/class/admin/benefits.mail" mernst-alum-address)
     ("bounce@asiflex.com" "~/class/admin/benefits.mail" mernst-alum-address)
     ("jjhw@u.washington.edu" "~/class/admin/benefits.mail")
     ("benefits@u.washington.edu" "~/class/admin/benefits.mail")

     ;;; Bug reports
     ("bug-gnu-emacs@gnu.org" "~/emacs/bug/bug-emacs.mail" mernst-alum-address)
     ("bug-lisp-manual@gnu.org" "~/emacs/bug/bug-emacs.mail" mernst-alum-address)
     ("stevens@kdstevens.com" "~/emacs/bug/bug-emacs.mail" mernst-alum-address)
     ("emacs-pretest-bug@gnu.org" "~/emacs/bug/bug-emacs.mail" mernst-alum-address)
     ;; This doesn't work.
     ("[0-9]+@emacsbugs.donarmstrong.com" "~/emacs/bug/bug-emacs.mail" mernst-alum-address)
     ("roland.winkler@physik.uni-erlangen.de" "~/emacs/bug/bug-packages.mail" mernst-alum-address) ; bibtex.el
     ("bug-vm@wonderworks.com" "~/emacs/bug/bug-vm.mail" mernst-alum-address)
     ("hack@robf.de" "~/emacs/bug/bug-vm.mail" mernst-alum-address)
     ("IncidentDaemon@sun.com" "~/bin/bugs/bug-jdk.mail")
     ("IncidentUpdateDaemon@sun.com" "~/bin/bugs/bug-jdk.mail")
     ("dl-feedback@acm.org" "~/bin/bugs/bug-webpages.mail")
     ("portal-feedback@hq.acm.org" "~/bin/bugs/bug-webpages.mail")
     ("rodkin@hq.acm.org" "~/bin/bugs/bug-webpages.mail") ; ACM digital library

     ;;; Consulting
     ("ERI400@mcleodusa.net" "~/random/consulting/consulting.mail" mernst-alum-address)
     ("bethbradbury@expertresources.com" "~/random/consulting/consulting.mail" mernst-alum-address)
     ("ERI@expertresources.com" "~/random/consulting/consulting.mail" mernst-alum-address)
     ("@roundtablegroup.com" "~/random/consulting/consulting.mail" mernst-alum-address)
     ("LUDINGTON@law.duke.edu" "~/random/consulting/consulting.mail" mernst-alum-address)
     ("@techpertsinc.com" "~/random/consulting/consulting.mail" mernst-alum-address)
     ;; Systolic Networks
     (".*@systolicnetworks.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     (".*@adtran.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("janet-lee@rcn.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("psanta@rcn.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("pgupta@sandw.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("jay-at-granitestream@nerd-marrow.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("granite-stream-engineering@nerd-marrow.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("sn-green@geyer.lcs.mit.edu" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("sn-red@geyer.lcs.mit.edu" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("sn-oversight@geyer.lcs.mit.edu" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("engineering@granitestream.com" "~/random/consulting/systolic-networks/mail.mail" sn-oversight-address)
     ("@mercury.com" "~/corr/mercury.mail")
     ("hank.levy@gmail.com" "~/corr/mercury.mail")
     ("kwmessi@super.org" "~/private/security-clearance/mail.mail" mernst-alum-address)
     ("fran@super.org" "~/prof/consulting/ccs/mail.mail" mernst-alum-address)
     ("dskr@mac.com" "~/prof/consulting/ccs/mail.mail" mernst-alum-address)
     (".*@super.org" "~/prof/consulting/ccs/mail.mail" mernst-alum-address)
     ("Nazgol_Moussavi@mckinsey.com" "~/prof/consulting/mckinsey/mail.mail" mernst-alum-address)
     ("marian@readingplus.com" "~/prof/consulting/consulting.mail" mernst-alum-address)
     ("@sugarman.com" "~/prof/consulting/sugarman/mail.mail" mernst-alum-address)

     ;; MPI
     ("office@mpi-sws.org" "~/corr/sabbatical.mail")
     ("sws-office@mpi-sws.mpg.de" "~/corr/sabbatical.mail")
     ("crichter@mpi-sws.mpg.de" "~/corr/sabbatical.mail")
     ("hansen@mpi-sb.mpg.de" "~/corr/sabbatical.mail")
     ("Helpdesk@mpi-sb.mpg.de" "~/corr/sabbatical.mail" mernst-mpi-address)
     ("helpdesk@mpi-inf.mpg.de" "~/corr/sabbatical.mail" mernst-mpi-address)
     ("mail@mpi-inf.mpg.de" "~/corr/sabbatical.mail" mernst-mpi-address)
     ("faculty@mpi-sws.mpg.de" "~/corr/sabbatical.mail")
     ("faculty@mpi-sws.org" "~/corr/sabbatical.mail")
     ("sws-science@mpi-sws.mpg.de" "~/corr/sabbatical.mail")
     ("anemone.dieblume@web.de" "~/corr/sabbatical.mail")
     ("andrzej.wasylkowski@gmail.com" "~/corr/sabbatical.mail")
     ("mpi-all@mpi-sb.mpg.de" "~/corr/sabbatical.mail")
     ("ms_kosaraju@yahoo.com" "~/corr/sabbatical.mail")
     ;; U. of Saarland (UdS)
     ("salim@virtualuna.de" "~/corr/sabbatical.mail")
     ("s9ahdoos@stud.uni-saarland.de" "~/corr/sabbatical.mail")
     ("wasylkowski@st.cs.uni-sb.de" "~/corr/sabbatical.mail")
     ("kerstin.reese@googlemail.com" "~/corr/sabbatical.mail")
     ("mayakuznetsova@googlemail.com" "~/corr/sabbatical.mail")
     ("hack@cs.uni-sb.de" "~/corr/sabbatical.mail")
     ("fellows@st.cs.uni-sb.de" "~/corr/sabbatical.mail")
     ("all@st.cs.uni-sb.de" "~/corr/sabbatical.mail")

     ;;; Personal

     ;; Nonprofits
     ("@aclu-wa.org" "~/corr/aclu.mail" mernst-alum-address)
     ("@aclu-mass.org" "~/corr/aclu.mail" mernst-alum-address)
     ("@aclum.org" "~/corr/aclu.mail" mernst-alum-address)
     ("knobe@worldnet.att.net" "~/corr/aclu.mail" mernst-alum-address)
     ("knobe@alum.mit.edu" "~/corr/aclu.mail" mernst-alum-address)
     ("hgunner@attbi.com" "~/corr/aclu.mail" mernst-alum-address)
     ("aclum@democracyinaction.org" "~/corr/aclu.mail" mernst-alum-address)
     ("Ron@cystic-l.org" "~/corr/advocacy/cf-web.mail" mernst-alum-address)
     ("@n2nma.org" "~/corr/advocacy/neighbortoneighbor.mail" mernst-alum-address)

     ;; TEPs
     ;; crock rule is first, to detour mail from specific individuals
     ("crock@tep.org" "~/corr/crock.mail" mernst-alum-address)
     ("crock@mit.edu" "~/corr/crock.mail" mernst-alum-address)
     ("finagler@speakeasy.org" "~/corr/ladybug.mail" mernst-alum-address)
     ("rhodes@media.mit.edu" "~/corr/ladybug.mail" mernst-alum-address)
     ("janielo@gmail.com" "~/corr/ladybug.mail" mernst-alum-address)
     ("bsagarin@niu.edu" "~/corr/brad-sagarin.mail" mernst-alum-address)
     ("bsagarin@apocalypse.org" "~/corr/brad-sagarin.mail" mernst-alum-address)
     ("mjs@alum.mit.edu" "~/corr/smeg.mail" mernst-alum-address)
     ("mamakim@uswest.net" "~/corr/smeg.mail" mernst-alum-address)
     ("gokimgo@msn.com" "~/corr/smeg.mail" mernst-alum-address)
     ("mamakim@bigplanet.com" "~/corr/smeg.mail" mernst-alum-address)
     ("mamakim@comcast.net" "~/corr/smeg.mail" mernst-alum-address)
     ("msmerekanych@comcast.net" "~/corr/smeg.mail" mernst-alum-address)
     ("juliolara@earthlink.net" "~/corr/juliolara.mail" mernst-alum-address)
     ("larajulio@comcast.net" "~/corr/juliolara.mail" mernst-alum-address)
     ("larajulio@mac.com" "~/corr/juliolara.mail" mernst-alum-address)
     ("juliof@geol.umd.edu" "~/corr/juliolara.mail" mernst-alum-address)
     ("friedmann2@llnl.gov" "~/corr/juliolara.mail" mernst-alum-address)
     ("juliolara@alum.mit.edu" "~/corr/juliolara.mail" mernst-alum-address)
     ("nehrlich@alum.mit.edu" "~/corr/perlick.mail" mernst-alum-address)
     ("nehrlich@speakeasy.net" "~/corr/perlick.mail" mernst-alum-address)
     ("eric@nehrlich.com" "~/corr/perlick.mail" mernst-alum-address)
     ("rico@alum.mit.edu" "~/corr/rico.mail")
     ("rico@vanu.com" "~/corr/rico.mail")
     ("loril@alum.mit.edu" "~/corr/rico.mail")
     ("mak@pacific.mps.ohio-state.edu" "~/corr/skeezix.mail" mernst-alum-address)
     ("rcalhoun@alum.mit.edu" "~/corr/leper.mail" mernst-alum-address)
     ("rob@calhoun.net" "~/corr/leper.mail" mernst-alum-address)
     ("Elizabeth.Wilmer@oberlin.edu" "~/corr/leper.mail" mernst-alum-address)
     ("fwilmer@oberlin.edu" "~/corr/leper.mail" mernst-alum-address)
     ("wilmer@splefty.com" "~/corr/leper.mail" mernst-alum-address)
     ("lckah@utk.edu" "~/corr/leper.mail" mernst-alum-address)
     ("jderoo@ati.com" "~/corr/u5.mail" mernst-alum-address)
     ("jderoo@nauticusnet.com" "~/corr/u5.mail" mernst-alum-address)
     ("jderoo@alum.mit.edu" "~/corr/u5.mail" mernst-alum-address)
     ("jderoo@alumxi.com" "~/corr/u5.mail" mernst-alum-address)
     ("jderoo@eitronix.com" "~/corr/u5.mail" mernst-alum-address)
     ("jderoo@bellatlantic.net" "~/corr/u5.mail" mernst-alum-address)
     ("jderoo@verizon.net" "~/corr/u5.mail" mernst-alum-address)
     ("rll29@verizon.net" "~/corr/u5.mail" mernst-alum-address)
     ("kositzke.2@osu.edu" "~/corr/skeezix.mail" mernst-alum-address)
     ("paulkrause88@alum.mit.edu" "~/corr/paul-krause.mail" mernst-alum-address)
     ("pkrause@soundbite.com" "~/corr/paul-krause.mail" mernst-alum-address)
     ("paulkrause1@mediaone.net" "~/corr/paul-krause.mail" mernst-alum-address)
     ("paulkrause1@comcast.net" "~/corr/paul-krause.mail" mernst-alum-address)
     ("Deborah.Krause@lhsl.com" "~/corr/paul-krause.mail" mernst-alum-address)
     ("Deborah.Krause@scansoft.com" "~/corr/paul-krause.mail" mernst-alum-address)
     ("Deborah.Krause@dictaphone.com" "~/corr/paul-krause.mail" mernst-alum-address)
     ("dsk3@georgetown.edu" "~/corr/paul-krause.mail" mernst-alum-address)
     ("debbie.krause@comcast.net" "~/corr/paul-krause.mail" mernst-alum-address)
     ("slclark2@mediaone.net" "~/corr/rajsuri.mail" mernst-alum-address)
     ("rajsuri@alum.mit.edu" "~/corr/rajsuri.mail" mernst-alum-address)
     ("rajsuri21@yahoo.com" "~/corr/rajsuri.mail" mernst-alum-address)
     ("Rajiv_Suri@bose.com" "~/corr/rajsuri.mail" mernst-alum-address)
     ("semblance@earthlink.net" "~/corr/probe.mail" mernst-alum-address)
     ("semblance@alum.mit.edu" "~/corr/probe.mail" mernst-alum-address)
     ("emil@lookinglass.us" "~/corr/probe.mail" mernst-alum-address)
     ("topol1@earthlink.net" "~/corr/probe.mail" mernst-alum-address)
     ;; Those without individual mail files
     ("Carl_Kraenzel@lotus.com" "~/corr/teps.mail" mernst-alum-address)
     ("pooter@us.ibm.com" "~/corr/teps.mail" mernst-alum-address)
     ("batman@bhelf.whirpon.com" "~/corr/teps.mail" mernst-alum-address)
     ("batman@flitterfly.whirpon.com" "~/corr/teps.mail" mernst-alum-address)
     ("batman@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("kohta@buildingscience.com" "~/corr/teps.mail" mernst-alum-address)
     ("bats22@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("joe@world.std.com" "~/corr/teps.mail" mernst-alum-address)
     ("joe@TheWorld.com" "~/corr/teps.mail" mernst-alum-address)
     ("joechapman@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("zilles@cs.wisc.edu" "~/corr/teps.mail")
     ("jforbess@MIT.EDU" "~/corr/teps.mail" mernst-alum-address)
     ("jforbess@vanu.com" "~/corr/teps.mail" mernst-alum-address)
     ("canida@MIT.EDU" "~/corr/teps.mail" mernst-alum-address)
     ("ajstrong@MIT.EDU" "~/corr/teps.mail" mernst-alum-address)
     ("eric@me3.net" "~/corr/teps.mail" mernst-alum-address)
     ("rajsuri@usa.net" "~/corr/rajsuri.mail" mernst-alum-address)
     ("sam.jansen@foxinternet.net" "~/corr/teps.mail" mernst-alum-address)
     ("sculgin@mtruant.com" "~/corr/teps.mail" mernst-alum-address)
     ("jdunham@sfis.com" "~/corr/teps.mail" mernst-alum-address)
     ("jwdunham@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("rebar@purpleturtle.com" "~/corr/teps.mail" mernst-alum-address)
     ("devlin@planet-save.com" "~/corr/teps.mail" mernst-alum-address)
     ("rebar@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("catherinedevlin@toast.net" "~/corr/teps.mail" mernst-alum-address)
     ("catherine.devlin@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("jpierre@sukidog.com" "~/corr/teps.mail" mernst-alum-address)
     ("sculgin@mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("jofish@cornell.edu" "~/corr/teps.mail" mernst-alum-address)
     ("bhelf@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("bhelf@flitterfly.whirpon.com" "~/corr/teps.mail" mernst-alum-address)
     ("marketa@MIT.EDU" "~/corr/teps.mail" mernst-alum-address)
     ("mmt@MIT.EDU" "~/corr/teps.mail" mernst-alum-address)
     ("mim@ee.columbia.edu" "~/corr/teps.mail" mernst-alum-address)
     ("mistertraub@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("msiverd@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("blantz@\\(loki.\\)?stanford.edu" "~/corr/lantz.mail" mernst-alum-address)
     ("jlantz@gmail.com" "~/corr/lantz.mail" mernst-alum-address)
     ("del@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("cen29786@centurytel.net" "~/corr/teps.mail" mernst-alum-address)
     ("wes@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("thatwesguy@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("ppham@local-box.org" "~/corr/teps.mail" mernst-alum-address)
     ("brown.mit.dr@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("edown@mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("paladin@alum.mit.edu" "~/corr/teps.mail" mernst-alum-address)
     ("pvillars@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("evarmint22@gmail.com" "~/corr/teps.mail" mernst-alum-address)
     ("ericjorg@microsoft.com" "~/corr/teps.mail" mernst-alum-address)
     ("moore@bricoworks.com" "~/corr/tim-moore.mail" mernst-alum-address)

     ;; eit comes last, so I correctly file email from specific individuals
     ("eit@tep.org" "~/corr/teps.mail" mernst-alum-address)
     ("drul@tep.org" "~/corr/teps.mail" mernst-alum-address)
     ("drool@mit.edu" "~/corr/teps.mail" mernst-alum-address)

     ;; UW and Seattle people
     ("todd@cs.washington.edu" "~/corr/todd-millstein.mail")
     ("todd@cs.ucla.edu" "~/corr/todd-millstein.mail")
     ("millstein@gmail.com" "~/corr/todd-millstein.mail")
     ("millstein@alumni.brown.edu" "~/corr/todd-millstein.mail")
     ("dawnw@microsoft.com" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("dawn@wrightangle.\\(org\\|com\\)" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("dawn1@athenet.net" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("Dawn.Wright@microsoft.com" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("eric@wrightangle.\\(org\\|com\\)" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("jdm@mocainc.com" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("joemag@logicaldatastructures.com" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("jmaguire@revivio.com" "~/corr/dawn-werner.mail" mernst-alum-address)
     ("rap@cs.ubc.ca" "~/corr/rap.mail" mernst-alum-address)
     ("wolf@cs.ubc.ca" "~/corr/rap.mail" mernst-alum-address)
     ("rap@cs.washington.edu" "~/corr/rap.mail" mernst-alum-address)
     ("wolf@cs.washington.edu" "~/corr/rap.mail" mernst-alum-address)
     ("wolfman@merl.com" "~/corr/rap.mail" mernst-alum-address)
     ("kimhayman@hotmail.com" "~/corr/rap.mail" mernst-alum-address)
     ("kshaw@richmond.edu" "~/corr/rap.mail" mernst-alum-address)
     ("ola@cs.duke.edu" "~/corr/rap.mail")
     ("rundaingean@yahoo.com" "~/corr/rap.mail")
     ("lsills@providencehealth.bc.ca" "~/corr/rap.mail" mernst-alum-address)
     ("sbogart@\\(shore\\.\\)?ctc.edu" "~/corr/bogie.mail" mernst-alum-address)
     ("sbogart18@juno.com" "~/corr/bogie.mail" mernst-alum-address)
     ("sbogart@shoreline.edu" "~/corr/bogie.mail" mernst-alum-address)
     ("pcrowley@cs.washington.edu" "~/corr/pcrowley.mail")
     ("pcrowley@wustl.edu" "~/corr/pcrowley.mail")
     ("laura.crowley@shorelineschools.org" "~/corr/pcrowley.mail" mernst-alum-address)
     ("lauramarie717@yahoo.com" "~/corr/pcrowley.mail" mernst-alum-address)
     ;; ("lazowska@cs.washington.edu" "~/corr/ed-lazowska.mail")
     ("chandanas@hotmail.com" "~/corr/chandana-surlu.mail" mernst-alum-address)
     ("chandans@exmsft.com" "~/corr/chandana-surlu.mail" mernst-alum-address)
     ("josephj@microsoft.com" "~/corr/chandana-surlu.mail" mernst-alum-address)
     ("josephj@windows.microsoft.com" "~/corr/chandana-surlu.mail" mernst-alum-address)
     ("bam@phoenixfire.org" "~/corr/bam.mail" mernst-alum-address)
     ("stebbi@cs.washington.edu" "~/corr/stebbi.mail" mernst-alum-address)
     ("sean.sandys@olin.edu" "~/corr/sean-sandys.mail" mernst-alum-address)
     ("sds@cs.williams.edu" "~/corr/sean-sandys.mail" mernst-alum-address)
     ("sds@cs.washington.edu" "~/corr/sean-sandys.mail" mernst-alum-address)
     ("ssandys@alumni.williams.edu" "~/corr/sean-sandys.mail" mernst-alum-address)
     ("sean@onemanposse.com" "~/corr/sean-sandys.mail" mernst-alum-address)
     ("whantoj@yahoo.com" "~/corr/ed-praitis.mail" mernst-alum-address)
     ("miguel@cs.washington.edu" "~/corr/miguel-figueroa.mail")
     ("CHernandez@appliant.com" "~/corr/miguel-figueroa.mail" mernst-alum-address)
     ("mifiguer@udec.cl" "~/corr/miguel-figueroa.mail")
     ("miguel.figueroa@udec.cl" "~/corr/miguel-figueroa.mail")
     ("peymano@ics.uci.edu" "~/corr/archive/peymano.mail" mernst-alum-address)
     ("peymano@yahoo.com" "~/corr/archive/peymano.mail" mernst-alum-address)
     ("peymano@gmail.com" "~/corr/archive/peymano.mail" mernst-alum-address)
     ("cskaplan@cs.uwaterloo.ca" "~/corr/csk.mail")
     ("csk@\\(\\(mud.\\)?cgl.\\)?uwaterloo.ca" "~/corr/csk.mail")
     ("csk@cs.washington.edu" "~/corr/csk.mail")
     ("ldusseault@xythos.com" "~/corr/csk.mail")
     ("nnasr@ofb.net" "~/corr/csk.mail")
     ("thingo@thingo.net" "~/corr/csk.mail")
     ("crew@CS.Stanford.EDU" "~/corr/rfc.mail" mernst-alum-address)
     ("rfc@cs.stanford.edu" "~/corr/rfc.mail" mernst-alum-address)
     ("emma@primenet.com" "~/corr/rfc.mail" mernst-alum-address)
     ("emma@ipomoea.org" "~/corr/rfc.mail" mernst-alum-address)
     ("kepart@cs.washington.edu" "~/corr/kurt-partridge.mail")
     ("tlau@cs.washington.edu" "~/corr/tessa-lau.mail")
     ("tessalau@us.ibm.com" "~/corr/tessa-lau.mail")
     ("wilson@cs.utah.edu" "~/corr/wilson-hsieh.mail")
     ("wilson.hsieh@gmail.com" "~/corr/wilson-hsieh.mail")
     ("corin@the4cs.com" "~/corr/uw-cs.mail")
     ("paul@byz.org" "~/corr/uw-cs.mail" mernst-alum-address)
     ("kgajos@cs.washington.edu" "~/corr/uw-cs.mail" mernst-alum-address)
     ("pauldf@gmail.com" "~/corr/uw-cs.mail" mernst-alum-address)
     ("ldenisep@yahoo.com" "~/corr/uw-cs.mail" mernst-alum-address)
     ("atiwary_home@yahoo.com" "~/corr/uw-cs.mail" mernst-alum-address)
     ("gretta@google.com" "~/corr/uw-cs.mail" mernst-alum-address)
     ;; ("rgalverson@alverson.net" "~/corr/uw-cs.mail" mernst-alum-address)
     ("psinha@mit.edu" "~/corr/mit-cs.mail" mernst-alum-address)
     ("rajeev@dujari.net" "~/corr/mit-cs.mail" mernst-alum-address)
     ("rajeev.dujari@gmail.com" "~/corr/mit-cs.mail" mernst-alum-address)

     ;; Relatives
     ("the-ernsts@earthlink.net" "~/corr/home.mail" mernst-alum-address)
     ("candpernst@earthlink.net" "~/corr/home.mail" mernst-alum-address)
     ("caernst@juno.com" "~/corr/home.mail" mernst-alum-address)
     ("carolyn.ernst@f1servicesinc.com" "~/corr/home.mail" mernst-alum-address)
     ("david.ernst2@verizon.net" "~/corr/home.mail" mernst-alum-address)
     ("dernst2005@charter.net" "~/corr/home.mail" mernst-alum-address)
     ("David.Ernst@trs-rentelco.com" "~/corr/home.mail" mernst-alum-address)
     ("tracy_ernst518@hotmail.com" "~/corr/home.mail" mernst-alum-address)
     ("trayce1968@hotmail.com" "~/corr/home.mail" mernst-alum-address)
     ("trayce1968@inbox.com" "~/corr/home.mail" mernst-alum-address)
     ("suzie250@aol.com" "~/corr/home.mail" mernst-alum-address)
     ("Amossmanjr@aol.com" "~/corr/home.mail" mernst-alum-address)
     ("nbirkhimer@yahoo.com" "~/corr/nancy-birkhimer.mail" mernst-alum-address)
     ("birkb@pa.net" "~/corr/nancy-birkhimer.mail" mernst-alum-address)
     ("baribirk@yahoo.com" "~/corr/nancy-birkhimer.mail" mernst-alum-address)
     ("ebrenden@\\(email.\\)?msn.com" "~/corr/nancy-birkhimer.mail" mernst-alum-address)
     ("ebrenden@comcast.net" "~/corr/nancy-birkhimer.mail" mernst-alum-address)
     ("shoemakj@vrinet.com" "~/corr/joe-shoemaker.mail" mernst-alum-address)
     ("shoemakc@ix.netcom.com" "~/corr/joe-shoemaker.mail" mernst-alum-address)
     ("joe.shoemaker@altarum.org" "~/corr/joe-shoemaker.mail" mernst-alum-address)
     ("jshoemaker@tmci-international.com" "~/corr/joe-shoemaker.mail" mernst-alum-address)
     ("cshoemaker@tmci-international.com" "~/corr/joe-shoemaker.mail" mernst-alum-address)
     ("shoemaker@hsnp.com" "~/corr/bob-shoemaker.mail" mernst-alum-address)
     ("bob.shoemaker@sbcglobal.net" "~/corr/bob-shoemaker.mail" mernst-alum-address)
     ("brenner0492@msn.com" "~/corr/home.mail" mernst-alum-address)
     ("bella_mcq@hotmail.com" "~/corr/home.mail" mernst-alum-address)
     ("gabriela.mcquade@gmail.com" "~/corr/home.mail" mernst-alum-address)

     ;; Dancers (without individual mail files)
     ("Flytii@aol.com" "~/corr/dance.mail" mernst-alum-address)
     ("mamittel@hewitt.com" "~/corr/dance.mail" mernst-alum-address)
     ("amittelstet@hotmail.com" "~/corr/dance.mail" mernst-alum-address)
     ("alschuler@hotmail.com" "~/corr/dance.mail" mernst-alum-address)
     ("shwoo@u.washington.edu" "~/corr/dance.mail" mernst-alum-address)
     ("susannamschultz@yahoo.com" "~/corr/dance.mail" mernst-alum-address)
     ("dpalacio@ix.netcom.com" "~/corr/dance.mail" mernst-alum-address)
     ("Deb.Palacios@nordstrom.com" "~/corr/dance.mail" mernst-alum-address)
     ("debpalacios@hotmail.com" "~/corr/dance.mail" mernst-alum-address)
     ("Debpalacios@aol.com" "~/corr/dance.mail" mernst-alum-address)
     ("DebstirAU@aol.com" "~/corr/dance.mail" mernst-alum-address)
     ("debstirau@optusnet.com.au" "~/corr/dance.mail" mernst-alum-address)
     ("fallen@start.com.au" "~/corr/dance.mail" mernst-alum-address)
     ("info@HopToTheBeat.com" "~/corr/dance.mail" mernst-alum-address)
     ("Halfsuit@aol.com" "~/corr/dance.mail" mernst-alum-address)
     ("ravitte@lindybaby.com" "~/corr/dance.mail" mernst-alum-address)
     ("tvseattle@home.com" "~/corr/dance.mail" mernst-alum-address)
     ("charlie@ociw.edu" "~/corr/dance.mail" mernst-alum-address)
     ("umstott@mpi.com" "~/corr/dance.mail" mernst-alum-address)
     ("kellyumstott@aol.com" "~/corr/dance.mail" mernst-alum-address)
     ("hollys@u.washington.edu" "~/corr/dance.mail" mernst-alum-address)
     ("82tbar@home.com" "~/corr/dance.mail" mernst-alum-address)
     ("82tbar@attbi.com" "~/corr/dance.mail" mernst-alum-address)
     ("82tbar@comcast.net" "~/corr/dance.mail" mernst-alum-address)
     ("guinness@javanet.com" "~/corr/dance.mail" mernst-alum-address)
     ("patrice_carlson@enumclaw.wednet.edu" "~/corr/dance.mail" mernst-alum-address)
     ("lindabober@altavista.com" "~/corr/dance.mail" mernst-alum-address)
     ("lindabober@netzero.net" "~/corr/dance.mail" mernst-alum-address)
     ("hhumphreys@imagicians.com" "~/corr/dance.mail" mernst-alum-address)
     ("hfh@imagicians.com" "~/corr/dance.mail" mernst-alum-address)
     ("BelleFlamenco@aol.com" "~/corr/dance.mail" mernst-alum-address)
     ("lwicks@CCBN.com" "~/corr/dance.mail" mernst-alum-address)
     ("subasta@rcn.com" "~/corr/dance.mail" mernst-alum-address)
     ("sue_basta@wellesley.mec.edu" "~/corr/dance.mail" mernst-alum-address)
     ("marilee88@yahoo.com" "~/corr/dance.mail" mernst-alum-address)
     ("jburdick@meditech.com" "~/corr/dance.mail" mernst-alum-address)

     ;; Others
     ("jkeysor@bu.edu" "~/corr/jkeysor.mail" mernst-alum-address)
     ("lkimball@foxinternet.net" "~/corr/laura-kimball.mail" mernst-alum-address)
     ("laurakimball@hotmail.com" "~/corr/laura-kimball.mail" mernst-alum-address)
     ("laura@newmanlaw.com" "~/corr/laura-kimball.mail" mernst-alum-address)
     ("sk@cs.brown.edu" "~/corr/shriram.mail")
     ("shriram@gmail.com" "~/corr/shriram.mail")
     ("kfisler@cs.wpi.edu" "~/corr/shriram.mail")
     ("kfisler@gmail.com" "~/corr/shriram.mail")
     ("sharon@alum.mit.edu" "~/corr/jas.mail" mernst-alum-address)
     ("sharon@netgen.com" "~/corr/jas.mail" mernst-alum-address)
     ("sechang@mediaone.net" "~/corr/jas.mail" mernst-alum-address)
     ("sechang@attbi.com" "~/corr/jas.mail" mernst-alum-address)
     ("sechang@comcast.net" "~/corr/jas.mail" mernst-alum-address)
     ("sechang2@gmail.com" "~/corr/jas.mail" mernst-alum-address)
     ("jamey@alum.mit.edu" "~/corr/jas.mail" mernst-alum-address)
     ("jamey@crl.dec.com" "~/corr/jas.mail" mernst-alum-address)
     ("jamey.hicks@compaq.com" "~/corr/jas.mail" mernst-alum-address)
     ("Jamey.Hicks@hp.com" "~/corr/jas.mail" mernst-alum-address)
     ("jamey.hicks@gmail.com" "~/corr/jas.mail" mernst-alum-address)
     ("soyoung@andantedairy.com" "~/corr/jamie-scanlan.mail" mernst-alum-address)
     ("scanlan@alum.mit.edu" "~/corr/jamie-scanlan.mail" mernst-alum-address)
     ("jscanlan@cd3o.com" "~/corr/jamie-scanlan.mail" mernst-alum-address)
     ("jscanlan@reddline.com" "~/corr/jamie-scanlan.mail" mernst-alum-address)
     ("jscanlan@atengo.net" "~/corr/jamie-scanlan.mail" mernst-alum-address)
     ("jscanlan@carbonoffset.mn" "~/corr/jamie-scanlan.mail" mernst-alum-address)
     ("jscanlan@road-warriors-r.us" "~/corr/jamie-scanlan.mail" mernst-alum-address)
     ("zugasti@opcenter.thecia.net" "~/corr/gonzalez-zugasti.mail" mernst-alum-address)
     ("jzugasti@msn.com" "~/corr/gonzalez-zugasti.mail" mernst-alum-address)
     ("javiergz@alum.mit.edu" "~/corr/gonzalez-zugasti.mail" mernst-alum-address)
     ("zugasti@comcast.net" "~/corr/gonzalez-zugasti.mail" mernst-alum-address)
     ("markle@microsoft.com" "~/corr/markle.mail" mernst-alum-address)
     ("markle11@hotmail.com" "~/corr/markle.mail" mernst-alum-address)
     ("markle11@gmail.com" "~/corr/markle.mail" mernst-alum-address)
     ("markle@alum.mit.edu" "~/corr/markle.mail" mernst-alum-address)
     ("veamark@earthlink.net" "~/corr/markle.mail" mernst-alum-address)
     ("vanessalevine@gmail.com" "~/corr/markle.mail" mernst-alum-address)
     ("llevine@ATVCAPITAL.com" "~/corr/markle.mail" mernst-alum-address)
     ("chayahl@comcast.net" "~/corr/markle.mail" mernst-alum-address)
     ("momrhoda27@hotmail.com" "~/corr/markle.mail" mernst-alum-address)
     ("psi@valis.com" "~/corr/psi.mail" mernst-alum-address)
     ("andi@eng.sun.com" "~/corr/psi.mail" mernst-alum-address)
     ("andi@perdition.eng.sun.com" "~/corr/psi.mail" mernst-alum-address)
     ("Andrea.Mankoski@eng.sun.com" "~/corr/psi.mail" mernst-alum-address)
     ("andi@valis.com" "~/corr/psi.mail" mernst-alum-address)
     ("helloandi@gmail.com" "~/corr/psi.mail" mernst-alum-address)
     ("jroberts@microsoft.com" "~/corr/jeff-roberts.mail" mernst-alum-address)
     ("jroberts@exchange.microsoft.com" "~/corr/jeff-roberts.mail" mernst-alum-address)
     ("jroberts@windows.microsoft.com" "~/corr/jeff-roberts.mail" mernst-alum-address)
     ("tamarar@gimli.nu" "~/corr/jeff-roberts.mail" mernst-alum-address)
     ("brk@alum.mit.edu" "~/corr/brian-krause.mail" mernst-alum-address)
     ("brk@adducive.com" "~/corr/brian-krause.mail" mernst-alum-address)
     ("brian@adducive.com" "~/corr/brian-krause.mail" mernst-alum-address)
     ("bkrause@gmail.com" "~/corr/brian-krause.mail" mernst-alum-address)
     ("brkrause@i2r.a-star.edu.sg" "~/corr/brian-krause.mail" mernst-alum-address)
     ("kribs@math.uta.edu" "~/corr/kribs.mail" mernst-alum-address)
     ("kribs@mathed.uta.edu" "~/corr/kribs.mail" mernst-alum-address)
     ("kribs@uta.edu" "~/corr/kribs.mail" mernst-alum-address)
     ("cmkz@mathed.uta.edu" "~/corr/kribs.mail" mernst-alum-address)
     ("veronica@mathed.uta.edu" "~/corr/kribs.mail" mernst-alum-address)
     ("adelfo@mathed.uta.edu" "~/corr/kribs.mail" mernst-alum-address)
     ("scollins@5549.com" "~/corr/cistercian.mail" mernst-alum-address)
     ("JWatkins@reitlerbrown.com" "~/corr/cistercian.mail" mernst-alum-address)
     ("fr-roch@cistercian.org" "~/corr/cistercian.mail" mernst-alum-address)
     ("ckstjohn@gmail.com" "~/corr/cistercian.mail" mernst-alum-address)
     ("dbrodrick@comcast.net" "~/corr/cistercian.mail" mernst-alum-address)
     ("djbrodrick@gmail.com" "~/corr/cistercian.mail" mernst-alum-address)
     ("drkopf@drkopf.com" "~/corr/cistercian.mail" mernst-alum-address)
     ("DrKopf@hawaii.rr.com" "~/corr/cistercian.mail" mernst-alum-address)
     ("ltao@girlhacker.com" "~/corr/lilly.mail" mernst-alum-address)
     ("avivacqua@alum.mit.edu" "~/corr/adriana.mail" mernst-alum-address)
     ("avivacqua@att.net" "~/corr/adriana.mail" mernst-alum-address)
     ("AVivacqua@quark.com" "~/corr/adriana.mail" mernst-alum-address)
     ("avivacqua@worldnet.att.net" "~/corr/adriana.mail" mernst-alum-address)
     ("avivacqua@alternex.com.br" "~/corr/adriana.mail" mernst-alum-address)
     ("woody@umb.edu" "~/corr/woody.mail" mernst-alum-address)
     ("woody@ems.umb.edu" "~/corr/woody.mail" mernst-alum-address)
     ("gann@electricknowledge.com" "~/corr/gann.mail" mernst-alum-address)
     ("gann@pobox.com" "~/corr/gann.mail" mernst-alum-address)
     ("dpw@almaden.ibm.com" "~/corr/david-williamson.mail")
     ("edwardp@\\(windows.\\)?microsoft.com" "~/corr/ed-praitis.mail" mernst-alum-address)
     ("knobe@crl.dec.com" "~/corr/kath-knobe.mail")
     ("Kath.Knobe@compaq.com" "~/corr/kath-knobe.mail")
     ("Kath.Knobe@hp.com" "~/corr/kath-knobe.mail")
     ("kath.knobe@intel.com" "~/corr/kath-knobe.mail")
     ("kath.knobe@alum.mit.edu" "~/corr/kath-knobe.mail" mernst-alum-address)
     ("fturbak@wellesley.edu" "~/corr/franklyn-turbak.mail")
     ("\\bnick@mit.edu" "~/corr/nick-papadakis.mail" mernst-alum-address)
     ("ceder@fas.harvard.edu" "~/corr/wgg.mail" mernst-alum-address)
     ("mfeeley@\\(weber.\\)?ucsd.edu" "~/corr/wgg.mail" mernst-alum-address)
     ("spertus@mills.edu" "~/corr/ellens.mail" mernst-alum-address)
     ("ellen.spertus@gmail.com" "~/corr/ellens.mail" mernst-alum-address)
     ("osgood@alum.mit.edu" "~/corr/nate-osgood.mail" mernst-alum-address)
     ("nosgood@MIT.EDU" "~/corr/nate-osgood.mail" mernst-alum-address)
     ("osgood@cs.usask.ca" "~/corr/nate-osgood.mail" mernst-alum-address)
     ("heaterhere@gmail.com" "~/corr/archive/heather-ralph.mail" mernst-alum-address)

     ;; Some people aren't in this file because I correspond with them on
     ;; too many issues:  David Notkin, John Chapin, etc.

     ;; Random
     ("tuna@cisco.com" "~/corr/random.mail" mernst-alum-address)
     ("soha@eecs.tufts.edu" "~/corr/random.mail")
     ("soha@cs.tufts.edu" "~/corr/random.mail")
     ("jems@zurich.ai.mit.edu" "~/corr/random.mail")
     ("jems@alum.mit.edu" "~/corr/random.mail")
     ("sharon@google.com" "~/corr/random.mail")
     ("sharon.perl@gmail.com" "~/corr/random.mail")
     ("hazelsct@mit.edu" "~/corr/random.mail")
     ("hazelsct@comcast.net" "~/corr/random.mail")
     ("apowell@opennovation.com" "~/corr/random.mail")
     ("hazelsct@alum.mit.edu" "~/corr/random.mail")
     ("baniasse@cs.tcd.ie" "~/corr/random.mail")
     ("ebeth928@yahoo.com" "~/corr/random.mail")
     ("b-flinchbaugh@ti.com" "~/corr/random.mail")

     ("HomeRuns@HomeRuns.com" "~/corr/commerce.mail" mernst-alum-address)
     ("service@barnesandnoble.com" "~/corr/commerce.mail" mernst-alum-address)
     ;; I'm doing work with Martin Schaef and Bernd Mathiske of Amazon
     ;; (".*@amazon.com" "~/corr/commerce.mail" mernst-alum-address)
     ("amazonorders@fantasticshopping.com" "~/corr/commerce.mail" mernst-alum-address)
     ("versandbestaetigung@amazon.de" "~/corr/commerce.mail" mernst-alum-address)
     ("summaries@esummary.info.aa.com" "~/corr/commerce.mail" mernst-alum-address)
     ("esummary@aadvantage.info.aa.com" "~/corr/commerce.mail" mernst-alum-address)
     ("American.Airlines@aa.com" "~/corr/commerce.mail" mernst-alum-address)
     ("notify@aa.globalnotifications.com" "~/corr/commerce.mail" mernst-alum-address)
     ("ezpass@adm.com" "~/corr/commerce.mail" mernst-alum-address)
     ("ezpass@mtg2.adm.com" "~/corr/commerce.mail" mernst-alum-address)
     ("ezpass@isecurus.com" "~/corr/commerce.mail" mernst-alum-address)
     ("iot-internet@wcom.com" "~/corr/commerce.mail" mernst-alum-address)
     ("orderstatus@intuit.com" "~/corr/commerce.mail" mernst-alum-address)
     ("Shop_Intuit@intuit.com" "~/corr/commerce.mail" mernst-alum-address)
     ("taxservice@intuit.com" "~/corr/commerce.mail" mernst-alum-address)
     ("turbotaxcustomercare@info.turbotax.com" "~/corr/commerce.mail" mernst-alum-address)
     ("TurboTax@renew.turbotax.com" "~/corr/commerce.mail" mernst-alum-address)
     ("TurboTax@info.turbotax.com" "~/corr/commerce.mail" mernst-alum-address)
     ("TurboTax@info1.turbotax.com" "~/corr/commerce.mail" mernst-alum-address)
     ("_ShippingInformation@intuit.com" "~/corr/commerce.mail" mernst-alum-address)
     ("TurboTax@intuit.com" "~/corr/commerce.mail" mernst-alum-address)
     (".*@fidelity.com" "~/corr/commerce.mail" mernst-alum-address)
     (".*@fmr.com" "~/corr/commerce.mail" mernst-alum-address)
     ("idm?@ProxyVote.com" "~/corr/commerce.mail" mernst-alum-address)
     ("idm@2votefundproxy.com" "~/corr/commerce.mail" mernst-alum-address)
     ("Performance@citynet.net" "~/corr/commerce.mail" mernst-alum-address)
     ("CustomerService@performanceinc.com" "~/corr/commerce.mail" mernst-alum-address)
     (".*@vonage.com" "~/corr/commerce.mail" mernst-alum-address)
     ("sam@samstailor.com" "~/corr/commerce.mail" mernst-alum-address)
     ("customer_?service@att.com" "~/corr/commerce.mail" mernst-alum-address)
     ("mpdiningB@mailer.idine.com" "~/corr/commerce.mail" mernst-alum-address)
     ("DMPromotions@usairways.com" "~/corr/commerce.mail" mernst-alum-address)
     ("dividendmiles@myusairways.com" "~/corr/commerce.mail" mernst-alum-address)
     ("HiEd_Order@dell.com" "~/corr/commerce.mail" mernst-alum-address)
     ("notifications@landsend.rsc02.com" "~/corr/commerce.mail" mernst-alum-address)
     ("custserv@nashbar.com" "~/corr/commerce.mail" mernst-alum-address)
     ("customerservice@nashbar.com" "~/corr/commerce.mail" mernst-alum-address)
     ("service@paypal.com" "~/corr/commerce.mail" mernst-alum-address)
     ("announcements-account@paypal.com" "~/corr/commerce.mail" mernst-alum-address)
     ("paypal@email.paypal.com" "~/corr/commerce.mail" mernst-alum-address)
     ("paypal@notifications.paypal.com" "~/corr/commerce.mail" mernst-alum-address)
     ("customer-service@campmor.com" "~/corr/commerce.mail" mernst-alum-address)
     ("custserv@gap.com" "~/corr/commerce.mail" mernst-alum-address)
     ("Comcast_Paydirect@comcast.net" "~/corr/commerce.mail" mernst-alum-address)
     ("Itinerary@production.priceline.com" "~/corr/commerce.mail" mernst-alum-address)
     ("technicalsupport@dell.com" "~/corr/commerce.mail" mernst-alum-address)
     ("mitfcu@mit.edu" "~/corr/commerce.mail" mernst-alum-address)
     ("customer-service@rei.com" "~/corr/commerce.mail" mernst-alum-address)
     ("service@REI.COM" "~/corr/commerce.mail" mernst-alum-address)
     ("REI_Coop_Membership@email.rei.com" "~/corr/commerce.mail" mernst-alum-address)
     ("noreply@share.skype.net" "~/corr/commerce.mail" mernst-alum-address)
     ("support@skype.net" "~/corr/commerce.mail" mernst-alum-address)
     ("noreply@news.skype.com" "~/corr/commerce.mail" mernst-alum-address)
     ("customerservice@clarkcolor.com" "~/corr/commerce.mail" mernst-alum-address)
     ("sales@shentech.com" "~/corr/commerce.mail" mernst-alum-address)
     ("rebates@thecoop.com" "~/corr/commerce.mail" mernst-alum-address)
     ("iCare@Cingular.com" "~/corr/commerce.mail" mernst-alum-address)
     ("gophone_donotreply@bevocal.com" "~/corr/commerce.mail" mernst-alum-address)
     ("cingular@ecustomersupport.com" "~/corr/commerce.mail" mernst-alum-address)
     ("customerservice@shop.com" "~/corr/commerce.mail" mernst-alum-address)
     ("service@qpb.com" "~/corr/commerce.mail" mernst-alum-address)
     (".*@nwemail.nwa.com" "~/corr/commerce.mail" mernst-alum-address)
     ("renewal_receipt@acm.org" "~/corr/commerce.mail" mernst-alum-address)
     ("mailer@thecoop.com" "~/corr/commerce.mail" mernst-alum-address)
     ("orderconfirm@express.ebay.com" "~/corr/commerce.mail" mernst-alum-address)
     ("sm-member@ebay.com" "~/corr/commerce.mail" mernst-alum-address)
     ("nstarebillnotification@nstar.com" "~/corr/commerce.mail" mernst-alum-address)
     ("mail@jetbluepromotions.com" "~/corr/commerce.mail" mernst-alum-address)
     ("Gophone@ecustomersupport.com" "~/corr/commerce.mail" mernst-alum-address)
     ("noreply@checkout.l.google.com" "~/corr/commerce.mail" mernst-alum-address)
     ("huskycrd@u.washington.edu" "~/corr/commerce.mail" mernst-alum-address)
     ("info@newegg.com" "~/corr/commerce.mail" mernst-alum-address)
     ("stocking@windermere.com" "~/corr/commerce.mail" mernst-alum-address)
     (".*@firsttechcu.com" "~/corr/commerce.mail" mernst-alum-address)
     ("alerts@cs.usbank-email.com" "~/corr/commerce.mail" mernst-alum-address)
     ("CityOfSeattleSUEBP-DoNotReply@seattle.gov" "~/corr/commerce.mail" mernst-alum-address)

     ("Leslie.Graves@homestreet.com" "~/random/fin/mortgage/mail.mail" mernst-alum-address)
     ("\\(becu\\)?homeloans@becu.org" "~/random/fin/mortgage/mail.mail" mernst-alum-address)
     ("Pshafer@metlifehomeloans.com" "~/random/fin/mortgage/mail.mail" mernst-alum-address)
     ("pcrandall@mtb.com" "~/random/fin/mortgage/mail.mail" mernst-alum-address)
     ("tmanowski@mtb.com" "~/random/fin/mortgage/mail.mail" mernst-alum-address)
     ("Lucia_Glasper@KeyBank.com" "~/random/fin/mortgage/mail.mail" mernst-alum-address)
     ("steven_alonzi@hotmail.com" "~/random/fin/sublet-laurel/mail.mail")
     ("lingalonzi@gmail.com" "~/random/fin/sublet-laurel/mail.mail")

     ;; Medical
     ("PatientOnline@med.mit.edu" "~/corr/medical.mail" mernst-alum-address)

     ;; Cat; prioritized last because she's often cc'ed on mail to others
     ("kisp_72@hotmail.com" "~/corr/wedding.mail" mernst-alum-address)
     ("lisa_vancans@newton.mec.edu" "~/corr/york-terrace.mail" mernst-alum-address)
     ("barkmanrealtygroup@hotmail.com" "~/corr/york-terrace.mail" mernst-alum-address)
     ("vinalstreet@googlegroups.com" "~/corr/york-terrace.mail" mernst-alum-address)
     ("vinaltrustees@googlegroups.com" "~/corr/york-terrace.mail" mernst-alum-address)
     ("barkmanrealtygroup@comcast.net" "~/corr/york-terrace.mail" mernst-alum-address)
     ("John@phoenixrealty.org" "~/corr/scath.mail" mernst-alum-address)
     ("scath@bu.edu" "~/corr/scath.mail" mernst-alum-address)
     ("scath@alumni.duke.edu" "~/corr/scath.mail" mernst-alum-address)
     ("cath@literacy-source.org" "~/corr/scath.mail" mernst-alum-address)
     ("schowell@gmail.com" "~/corr/scath.mail" mernst-alum-address)
     ("Cat Howell's blog" "~/corr/scath.mail" mernst-alum-address)
     ;; Childcare
     (".*@childlearning.org" "~/corr/scath.mail" mernst-alum-address)
     ("dcelder@MIT.EDU" "~/corr/scath.mail" mernst-alum-address)
     ;; Family
     ("noellwilson@yahoo.com" "~/corr/scath.mail" mernst-alum-address)
     ("nwilson2@midsouth.rr.com" "~/corr/scath.mail" mernst-alum-address)
     ("noellwilson@comcast.net" "~/corr/scath.mail" mernst-alum-address)
     ("nrwilson@olemiss.edu" "~/corr/scath.mail" mernst-alum-address)
     ("whowell4@hotmail.com" "~/corr/scath.mail" mernst-alum-address)
     ("whowell4@earthlink.net" "~/corr/scath.mail" mernst-alum-address)
     ("s_howell@juno.com" "~/corr/scath.mail" mernst-alum-address)
     ("smchowell@charter.net" "~/corr/scath.mail" mernst-alum-address)
     ("gwilson@llpf.com" "~/corr/scath.mail" mernst-alum-address)
     ("winborne2@bellsouth.com" "~/corr/scath.mail" mernst-alum-address)
     ("winborne2@bellsouth.net" "~/corr/scath.mail" mernst-alum-address)
     ("Gracat@etinternet.net" "~/corr/scath.mail" mernst-alum-address)
     ("steamwhistles@bellsouth.net" "~/corr/scath.mail" mernst-alum-address)
     ("runner@i-55.com" "~/corr/scath.mail" mernst-alum-address)
     ("moritz.huetten@uni-jena.de" "~/corr/scath.mail" mernst-alum-address)
     ("sabinehuetten@yahoo.de" "~/corr/scath.mail" mernst-alum-address)
     ("lsat@bellsouth.net" "~/corr/scath.mail" mernst-alum-address)
     ;; Friends
     ("amy.blair@alumni.duke.edu" "~/corr/scath.mail" mernst-alum-address)
     ("amy.blair@marquette.edu" "~/corr/scath.mail" mernst-alum-address)
     ("heatherl@bu.edu" "~/corr/scath.mail" mernst-alum-address)
     ("heatherl@alum.bu.edu" "~/corr/scath.mail" mernst-alum-address)
     ("heatherl@fastmail.fm" "~/corr/scath.mail" mernst-alum-address)
     ("KiraLMarch@aol.com" "~/corr/scath.mail" mernst-alum-address)
     ("kiramarch@gmail.com" "~/corr/scath.mail" mernst-alum-address)
     ("sjeffers@fas.harvard.edu" "~/corr/scath.mail" mernst-alum-address)
     ("kathrynskaplan@yahoo.com" "~/corr/scath.mail" mernst-alum-address)
     ("safish@bu.edu" "~/corr/scath.mail" mernst-alum-address)
     ("dweinsto@chem.bu.edu" "~/corr/scath.mail" mernst-alum-address)
     ("dweinsto@rci.rutgers.edu" "~/corr/scath.mail" mernst-alum-address)
     ("Heidi.Brooks@bridgespangroup.org" "~/corr/scath.mail" mernst-alum-address)
     ("Heidi.Brooks@bridgespan.org" "~/corr/scath.mail" mernst-alum-address)
     ("hbrooks@mba2003.hbs.edu" "~/corr/scath.mail" mernst-alum-address)
     ("jo_kap@yahoo.com" "~/corr/scath.mail" mernst-alum-address)
     ("m_herzog@bellsouth.net" "~/corr/scath.mail" mernst-alum-address)
     ;; Colleagues
     ("shanley@bu.edu" "~/corr/scath.mail" mernst-alum-address)
     ;; Wallingford
     ("alexxandra@juno.com" "~/corr/scath.mail" mernst-alum-address)
     ;; Vinal St.
     ("vinalstreet@googlegroups.com" "~/corr/scath.mail" mernst-alum-address)
     ("jlysy@hotmail.com" "~/corr/scath.mail" mernst-alum-address)
     ("klackey@bu.edu" "~/corr/scath.mail" mernst-alum-address)
     ;; Others
     ("kisp_72@yahoo.com" "~/corr/scath.mail" mernst-alum-address)
     ("jere@u.washington.edu" "~/corr/scath.mail" mernst-alum-address)
     ;; Not sure
     ("rahp52@aol.com" "~/corr/scath.mail" mernst-alum-address)
     ))


(defvar subj-info-list nil)
(setq subj-info-list
  ;; All these are active only in the "Subject:" field.  Put them at the
  ;; beginning of the list (end of this file) so they take effect before
  ;; rules based on email addresses.
  '(
    ;; Classes
    ("590p" . "~/class/590p/mail.mail")
    ("590n" . "~/class/590n/mail.mail")
    ("590MSR" . "~/class/590n/mail.mail")
    ("\\b\\(CSE ?\\)?403" . "~/class/403/mail.mail")
    ("curriculum[- ]\\(revision\\|update\\)" . "~/class/curriculum/curriculum.mail")
    ;; MIT Classes
    ;; ("6.005" . "~/class/curriculum.mail")
    ("6.033" . "~/class/6033/mail.mail")
    ("6.370" . "~/class/6370/mail.mail")
    ("Battlecode" . "~/class/6370/mail.mail")
    ;; 6.170; more specific rules come first
    ("\\bPS ?[0-6]\\b" . "~/class/170/mail/psets.mail")
    ("\\bProblem Set [0-6]\\b" . "~/class/170/mail/psets.mail")
    ("\\bLab ?[1-4]\\b" . "~/class/170/mail/labs.mail")
    ("\\b\\(Gizmoball\\|Antichess\\)\\b" . "~/class/170/mail/project.mail")
    ;; Undesirable, as this overrides rules for From lines
    ;; ("6.?170" . "~/class/170/mail/mail.mail")
    ("6.170 MOTD" . "~/class/170/mail/motd.mail")
    ("6.?821" . "~/class/821/mail.mail")
    ("6.?883" . "~/class/883/mail.mail")
    ("6.893" . "~/class/893/mail/mail.mail")
    ("6.897" . "~/class/897/mail.mail")

    ;; Program committees
    ;; ("Setting a date for the fifth NEPLS meeting" .  "~/prof/referee/nepls/dates.mail")
    ("\\[woda-pc\\]" . "~/prof/referee/woda-2004/mail.mail")
    ("WODA 2006" . "~/prof/referee/woda-2006/mail.mail")
    ("WODA 2009" . "~/prof/referee/woda-2009/mail.mail")
    ("Paste04" . "~/prof/referee/paste-2004/mail.mail")
    ("\\bPASTE\\( ?\\(20\\)?05\\)\\b" . "~/prof/referee/paste-2005/mail.mail")
    ("\\bPASTE\\( ?\\(20\\)?07\\)\\b" . "~/prof/referee/paste-2007/mail.mail")
    ("\\bPASTE\\( ?\\(20\\)?08\\)?\\b" . "~/prof/referee/paste-2008/mail.mail")
    ("Bugs ?05" . "~/prof/referee/bugs05/mail.mail")
    ;; ("FTfJP" . "~/prof/referee/ftfjp-2004/mail.mail")
    ("\\[POPL 05\\]" . "~/prof/referee/popl-2005/mail.mail")
    ;; ("\\bPLDI\\(\\(20\\)?06\\)?\\b" . "~/prof/referee/pldi-2006/mail.mail")
    ("\\bCC 2006" . "~/prof/referee/cc-2006/mail.mail")
    ("ECOOP 2006" . "~/prof/referee/ecoop-2006/mail.mail")
    ("FSE ?2007" . "~/prof/referee/esecfse-2007/mail.mail")
    ("ESEC ?2007" . "~/prof/referee/esecfse-2007/mail.mail")
    ("P070548" . "~/prof/referee/nsf-cpa-2006/mail.mail")
    ("HotSWUp'08" . "~/prof/referee/hotswup-2008/mail.mail")
    ("P091142" . "~/prof/referee/nsf-ccf-2009/mail.mail")
    ("P091133" . "~/prof/referee/nsf-ccf-2009/mail.mail")
    ("SHFSMPL" . "~/prof/referee/nsf-ccf-2009/mail.mail")
    ("TSE-20[0-9][0-9]-" . "~/prof/referee/tse/tse.mail")
    ("TDSC-20[0-9][0-9]-" . "~/prof/referee/tdsc/mail.mail")

    ;; Grants
    ("6895566" . "~/prof/grants/2003-02-itr-medical/mail.mail")
    ("0325283" . "~/prof/grants/2003-02-itr-medical/mail.mail")
    ("BAA ?03-44" . "~/prof/grants/2003-11-darpa-selfregen/mail.mail")
    ("DARPA SRS" . "~/prof/grants/2003-11-darpa-selfregen/mail.mail")
    ("SRSLIST" . "~/prof/grants/2003-11-darpa-selfregen/mail.mail")
    ;; Also "Software Requirements Specification" from CSE 403
    ;; ("\\bSRS\\b" . "~/prof/grants/2003-11-darpa-selfregen/mail.mail")
    ("FA8750-04-2-0254" . "~/prof/grants/2003-11-darpa-selfregen/mail.mail")
    ("HDCP" . "~/prof/grants/hdcp/mail.mail")
    ("0234651" . "~/prof/grants/hdcp/mail.mail")
    ("CS[2S][PG]" . "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
    ("CS Study \\(Group\\|Panel\\)" . "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
    ("HR0011-06-1-0017" . "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
    ("HR0011-07-1-0023" . "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
    ("06-19-F263" . "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
    ("6899358" . "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
    ("6915285" . "~/prof/grants/2005-11-darpa-csstudypanel/mail.mail")
    ("FA8750-06-2-0189" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("Application Communities" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("ClearView" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("\\bAC2?\\b" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("\\bCLSR\\b" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("BAA ?05-51" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("Determina" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("DARPATech" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("6899762" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("A49436" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("FA42683" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("5710002617" . "~/prof/grants/2005-10-darpa-appcommunities/mail.mail")
    ("0613793" . "~/prof/grants/2006-01-nsf-sod/mail.mail")
    ("\\bJava ?DIS\\b" . "~/prof/grants/abb/mail.mail")
    ("\\bABB\\b" . "~/prof/grants/abb/mail.mail")
    ("6918193" . "~/prof/grants/abb/mail.mail")
    ("6918194" . "~/prof/grants/abb/mail.mail")
    ("0855252" . "~/prof/grants/2007-12-nsf-cpa/mail.mail")
    ("A43256" . "~/prof/grants/2007-12-nsf-cpa/mail.mail")
    ("\\bARRA\\b" . "~/prof/grants/2007-12-nsf-cpa/mail.mail")
    ("Letter of support for Daikon grant proposal" . "~/prof/grants/2009-08-nsf-daikon/mail.mail")
    ("A51870" . "~/prof/grants/2009-08-nsf-daikon/mail.mail")
    ("0958218" . "~/prof/grants/2009-08-nsf-daikon/mail.mail")
    ("A52324" . "~/prof/grants/2009-08-nsf-speculate/mail.mail")
    ("0963757" . "~/prof/grants/2009-08-nsf-speculate/mail.mail")

    ;; Other
    ;; ("\\(progress\\|status\\|weekly\\)\\(/dilemma\\)?[- ]\\(report\\|update\\)" . "~/research/invariants/mail/progress-reports.mail")
    ("\\[crock\\]" . "~/corr/crock.mail")
    ("\\[ACLU Priv. & Tech.\\]" . "~/corr/aclu.mail")
    ("MIT-1281" . "~/corr/prospective-staff.mail")
    ("Mit-00004412" . "~/corr/prospective-staff.mail")
    ("tqe oral" . "~/class/admin/area-exams.mail")
    ("RQE" . "~/class/admin/area-exams.mail")
    ("Projected EECS Audit" . "~/class/admin/ugrad-advising.mail")
    ("\\[ escjava-Bugs" . "~/bin/bugs/bug-escjava.mail")
    ("\\[ findbugs-Bugs" . "~/bin/bugs/bug-findbugs.mail")
    ("JSR[- ]?305" . "~/research/types/mail/jsr305.mail")
    ("JSR[- ]?308" . "~/research/types/mail/language.mail")
    ("Checker Framework" . "~/research/types/mail/checkers.mail")
    ("Monthly Debian software resync" . "~/corr/sysadmin.mail")
    ("^\\(Re: \\)*\\[wreq #[0-9]+\\]" . "~/corr/sysadmin.mail")
    ("\\breCrash\\b" . "~/class/advisees/sung-kim.mail")
    ("\\bCrashma\\b" . "~/class/advisees/sung-kim.mail")
    ("ical4j-Patches" . "~/bin/bugs/bug-ical4j.mail")
    ("ical4j-Bugs" . "~/bin/bugs/bug-ical4j.mail")
    ("Ardilla" . "~/class/advisees/kiezun.mail")
    ("refactoring using type constraints" . "~/class/advisees/kiezun.mail")
    ("Javari" . "~/research/types/mail/javari.mail")
    ))


;; Put at top of list (bottom of this file) to take precedence (e.g., over
;; Subject lines).
;; Note that lines in this list take precedence in REVERSE ORDER -- later
;; lines take precedence over earlier ones.
(defvar addr-info-list-2 nil)
(setq addr-info-list-2
  '(
    ("adbirka@\\(alum.\\)?mit.edu" "~/class/advisees/adbirka.mail")
    ("adbirka@microsoft.com" "~/class/advisees/adbirka.mail")
    ("ajmani@\\(\\(lcs\\|csail\\).mit.edu\\|gmail.com\\)" "~/class/advisees/sameer-ajmani.mail")
    ("mgebauer@mit.edu" "~/class/upop-dir/upop-2006/software.mail")
    ("\"6.170 Software Engineering Lab\" <vikki@mit.edu>" "~/class/170/mail/forum.mail")
    ("salim@virtualuna.de" "~/corr/sabbatical.mail")
    ("igorp@us.ibm.com" "~/research/types/mail/language.mail")

    ("cse590n@cs.washington.edu" "~/class/590n/mail.mail")

    ;; Project groups take precedence over the "403" subject line, but if
    ;; 403ta is copied, then it shouldn't go into the groups.mail file.
    ;; Project groups
    ("cse403-2d-game-admin@cs.washington.edu" "~/class/403/groups.mail")
    ("cse403-cooking-special-admin@cs.washington.edu" "~/class/403/groups.mail")
    ("cse403-firbi-admin@cs.washington.edu" "~/class/403/groups.mail")
    ("cse403-graduation-station-admin@cs.washington.edu" "~/class/403/groups.mail")
    ("cse403-my-u-textbook-admin@cs.washington.edu" "~/class/403/groups.mail")
    ("cse403-nestor-admin@cs.washington.edu" "~/class/403/groups.mail")
    ("cse403-robot-rock-admin@cs.washington.edu" "~/class/403/groups.mail")
    ("cse403-traffic-planning-admin@cs.washington.edu" "~/class/403/groups.mail")
    ;; If one of these appears, then the email is not private to the
    ;; project group and should go in the main 403 mail file.
    ("jibb@cs.washington.edu" "~/class/403/mail.mail")
    ("tannewt@cs.washington.edu" "~/class/403/mail.mail")
    ("cse403-ta@cs.washington.edu" "~/class/403/mail.mail")
    ("cse403a_sp09@u.washington.edu" "~/class/403/mail.mail")

    ;; Administrative matters related to JSR 308
    ("pmo@jcp.org" "~/research/types/mail/language.mail")
    ("admin@jcp.org" "~/research/types/mail/language.mail")
    ("Liz@jcp.org" "~/research/types/mail/language.mail")
    ("Liz.Kiener@Sun.COM" "~/research/types/mail/language.mail")
    ("SPECLEADS@JCP.ORG" "~/research/types/mail/language.mail")
    ("Corina.Ulescu@SUN.COM" "~/research/types/mail/language.mail")
    ("JCP-MEMBERS@JCP.ORG" "~/research/types/mail/language.mail")

    ))


;; Debugging.
;;  1. Disable "if nil"
;;  2. Save this file (which automatically loads it)
;;  3. Debug `vm-auto-select-folder'.  If  C-c f  doesn't navigate to it, try
;;     /usr/share/emacs/site-lisp/vm/vm-save.el
(if nil
    (progn
      (setq addr-info-list-1
	    ;; This is a list of two-or-three-element lists.
	    ;; The sublists contain: email-regexp, output-file, my-from-address.
	    ;; If my-from-address is a symbol, it is evaluated (to a string).
	    '(
	      ("eric@wrightangle.org" "~/corr/dawn-werner.mail" mernst-alum-address)
	      ;; ("stocking@windermere.com" "~/corr/commerce.mail" mernst-alum-address)
	      ;; ("artzi@\\(csail\\.\\)?MIT.EDU" "~/class/advisees/shay-artzi.mail")
	      ;; ("mahmood@MIT.EDU" "~/research/types/mail/javari.mail")
	      ;; ("jsr308\\(-statements\\|-bugs\\)?@\\(lists.\\)?csail.mit.edu" "~/research/types/mail/annotations.mail")
	      ;; ("\"6.170 Software Engineering Lab\" <vikki@mit.edu>" "~/class/170/mail/forum.mail")
	      ))
      (setq subj-info-list '())
      (setq addr-info-list-2
	    '(
	      ))
      )
)


;; (In Emacs 21, this should perhaps use variable
;; `rmail-automatic-folder-directives' instead.)

;; rmail-output-file-alist is used by `rmail-output-read-rmail-file-name',
;;   which I redefine below.
;; sendmail-from-address-alist is used by `check-from-address'.

(defvar mernst-csail-address "Michael Ernst <mernst@csail.mit.edu>")
(defvar mernst-alum-address "Michael Ernst <mernst@alum.mit.edu>")
(defvar mernst-uwashington-address "Michael Ernst <mernst@u.washington.edu>")
(defvar mernst-cse-address "Michael Ernst <mernst@cs.washington.edu>")
(defvar mernst-mpi-address "Michael Ernst <mernst@mpi-sws.mpg.de>")
(defvar sn-oversight-address "Michael Ernst <sn-oversight@geyer.lcs.mit.edu>")

(defvar correspondent-field-regexp
  "^\\(CC\\|From\\|\\(Apparently-\\)?To\\|\\(X-\\)?Sender\\|Reply-To\\):")

;; Side-effects sendmail-from-address-alist and rmail-output-file-alist
;; (which are dynamically bound).
(defun addr-info-to-rmail (addr-info)
  (let ((addr (cl-first addr-info))
	(output-file (cl-second addr-info))
	(from-addr (cl-third addr-info)))
    (if output-file
	(push
	 (cons
	  ;; I'm not sure whether this is an efficient
	  ;; regexp; perhaps it's good enough.
	  ;; (concat "^\\(CC\\|From\\|To\\):.*\\(\n.*\\)*" (car addr-file-pair))
	  ;; It was not efficient enough!
	  (concat correspondent-field-regexp ".*\\(\n[ \t].*\\)?\\b" addr)
	  output-file)
	 rmail-output-file-alist))))

;; This is rather inefficient, because vm-auto-folder-alist requires each
;; HEADER-NAME-REGEXP to match exactly one header.  Perhaps make it better
;; later.
;; Example:
;; (setq vm-auto-folder-alist
;;       '(("Sender:" ("owner-j-body@example.org" . "~/mail/J-BODIES"))
;;         ("From:" ("katana@example.com" . "~/mail/ZOCALO"))
;;         ("Sender:" ("owner-honda-perf@example.edu" . "~/mail/HONDA"))))


(defun addr-info-to-vm (addr-info)
  (let ((addr (cl-first addr-info))
	(output-file (cl-second addr-info))
	(from-addr (cl-third addr-info)))
    (if output-file
	(let ((addr-file-pair (cons addr output-file)))
	  (progn
	    (push (list "Cc:" addr-file-pair) vm-auto-folder-alist)
	    (push (list "From:" addr-file-pair) vm-auto-folder-alist)
	    (push (list "To:" addr-file-pair) vm-auto-folder-alist)
	    (push (list "Apparently-To:" addr-file-pair) vm-auto-folder-alist)
	    ;; Previously commented out for efficiency
	    (push (list "\\(X-\\)?Sender:" addr-file-pair) vm-auto-folder-alist)
	    (push (list "Reply-To::" addr-file-pair) vm-auto-folder-alist)
	    )))))

(defun addr-info-to-sendmail (addr-info)
  (let ((addr (cl-first addr-info))
	(output-file (cl-second addr-info))
	(from-addr (cl-third addr-info)))
    (if from-addr
	(push
	 (cons
	  (concat "^\\(CC\\|To\\):.*\\(\n[ \t].*\\)?\\b" addr)
	  ;; (if (symbolp from-addr) (eval from-addr) from-addr)
	  from-addr)
	 sendmail-from-address-alist))))

(defun subj-info-to-rmail (subj-file-pair)
  (let ((subj (car subj-file-pair))
	(output-file (cdr subj-file-pair)))
      (push
       (cons
	(concat "^\\(Subject\\):.*\\(\n[ \t].*\\)?" subj)
	output-file)
       rmail-output-file-alist)))

(defun all-subj-info-to-vm (subj-file-pair-list)
  (push
   (cons "Subject:" subj-file-pair-list)
   vm-auto-folder-alist))


(defun setup-addresses ()
  ;; RMAIL
  (progn
    (setq rmail-output-file-alist '())
    (mapc (function addr-info-to-rmail)
	  addr-info-list-1)
    (setq rmail-output-file-alist (nreverse rmail-output-file-alist))
    (mapc (function subj-info-to-rmail)
	  subj-info-list)
    (mapc (function addr-info-to-rmail)
	  addr-info-list-2))

  ;; VM
  (progn
    (setq vm-auto-folder-alist '())
    (mapc (function addr-info-to-vm)
	  addr-info-list-1)
    (setq vm-auto-folder-alist (nreverse vm-auto-folder-alist))
    (all-subj-info-to-vm subj-info-list)
    (mapc (function addr-info-to-vm)
	  addr-info-list-2))

  ;; Sendmail
  (progn
    (setq sendmail-from-address-alist '())
    (mapc (function addr-info-to-sendmail)
	  addr-info-list-1)
    (setq sendmail-from-address-alist (nreverse sendmail-from-address-alist))
    (mapc (function addr-info-to-sendmail)
	  addr-info-list-2))
  )
(setup-addresses)


(provide 'rmail-addresses-mde)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read RMAIL output file name
;;;


;; Differs from the original in rmailout.el in that it bounds the search,
;; which may be time-consuming.

(with-eval-after-load "rmailout"
  (defun rmail-output-read-file-name ()
    "Read the file name to use for `rmail-output'.
Set `rmail-default-file' to this name as well as returning it."
    ;; (message "Here I am.")
    (let ((default-file
	   (let ((buf-size (save-excursion
			     (if (eq major-mode 'rmail-summary-mode)
				 (set-buffer rmail-buffer))
			     (- (point-max) (point-min))))
		 answer tail)
	     (setq tail rmail-output-file-alist)
             ;; Suggest a file based on a pattern match.
	     (while (and tail (not answer))
	       (save-excursion
		 (if (> buf-size 20000)
		     (setq answer (find-in-buffer-eval tail))
		   (progn
		     (goto-char (point-min))
		     (if (re-search-forward (car (car tail)) nil t)
			 (setq answer (eval (cdr (car tail))))))))
	       (setq tail (cdr tail)))
             ;; If no suggestion, use same file as last time.
	     (or answer rmail-default-file))))
      (let ((read-file
	     (expand-file-name
	      (read-file-name
	       (concat "Output message to Unix mail file: (default "
		       (file-name-nondirectory default-file)
		       ") ")
	       (file-name-directory default-file)
	       (abbreviate-file-name default-file))
	      (file-name-directory default-file))))
	(setq rmail-default-file
	      (if (file-directory-p read-file)
		  (expand-file-name (file-name-nondirectory default-file)
				    read-file)
		(expand-file-name
		 (or read-file (file-name-nondirectory default-file))
		 (file-name-directory default-file)))))))

  (defun rmail-output-read-rmail-file-name ()
    "Read the file name to use for `rmail-output-to-rmail-file'.
Set `rmail-default-rmail-file' to this name as well as returning it."
    (let ((default-file
	   (let ((buf-size (save-excursion
			     (if (eq major-mode 'rmail-summary-mode)
				 (set-buffer rmail-buffer))
			     (- (point-max) (point-min))))
		 answer tail)
	     (setq tail rmail-output-file-alist)
             ;; Suggest a file based on a pattern match.
	     (while (and tail (not answer))
	       (save-excursion
		 (if (eq major-mode 'rmail-summary-mode)
		     (set-buffer rmail-buffer))
		 (if (> buf-size 20000)
		     (setq answer (find-in-buffer-eval tail))
		   (progn
		     (goto-char (point-min))
		     (if (re-search-forward (car (car tail)) nil t)
			 (setq answer (eval (cdr (car tail)))))))
		 (setq tail (cdr tail))))
             ;; If no suggestions, use same file as last time.
	     (expand-file-name (or answer rmail-default-rmail-file)))))
      (let ((read-file
	     (expand-file-name
	      (read-file-name
	       (concat "Output message to Rmail file: (default "
		       (file-name-nondirectory default-file)
		       ") ")
	       (file-name-directory default-file)
	       (abbreviate-file-name default-file))
	      (file-name-directory default-file))))
	;; If the user enters just a directory,
	;; use the name within that directory chosen by the default.
	(setq rmail-default-rmail-file
	      (if (file-directory-p read-file)
		  (expand-file-name (file-name-nondirectory default-file)
				    read-file)
		read-file))))))


;; Takes argument "tail" to make it as much like the original functions as
;; possible.
(defun find-in-buffer-eval (tail)
  (goto-char (point-min))
  (if (re-search-forward (car (car tail)) (+ (point) 10000) t)
      (eval (cdr (car tail)))
    (progn
      (goto-char (point-max))
      (if (re-search-backward (car (car tail)) (- (point) 10000) t)
	  (eval (cdr (car tail)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

;;; Local Variables:
;;; eval: (make-local-variable 'after-save-hook)
;;; eval: (add-hook 'after-save-hook #'(lambda () (eval-buffer)))
;;; end:

;;; rmail-addresses-mde.el ends here
