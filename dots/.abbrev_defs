;;; -*- Mode:  Emacs-Lisp -*-
;;-*-coding: utf-8;-*-

;; An advantage of editing this file directly is that Emacs's
;; "write-abbrev-file" writes the abbrevs in arbitrary order (resulting
;; from reading them from a hash table).

;; For R documentation keywords
(define-abbrev-table 'Rd-mode-abbrev-table
  '(
    ("`ag" "\\arguments" nil 0)
    ("`al" "\\alias" nil 0)
    ("`au" "\\author" nil 0)
    ("`bf" "\\bold" nil 0)
    ("`co" "\\code" nil 0)
    ("`de" "\\describe" nil 0)
    ("`dn" "\\description" nil 0)
    ("`dt" "\\details" nil 0)
    ("`em" "\\emph" nil 0)
    ("`en" "\\enumerate" nil 0)
    ("`ex" "\\examples" nil 0)
    ("`fi" "\\file" nil 0)
    ("`fo" "\\format" nil 0)
    ("`it" "\\item" nil 0)
    ("`iz" "\\itemize" nil 0)
    ("`kw" "\\keyword" nil 0)
    ("`li" "\\link" nil 0)
    ("`me" "\\method" nil 0)
    ("`na" "\\name" nil 0)
    ("`no" "\\note" nil 0)
    ("`re" "\\references" nil 0)
    ("`sa" "\\seealso" nil 0)
    ("`se" "\\section" nil 0)
    ("`so" "\\source" nil 0)
    ("`ss" "\\subsection" nil 0)
    ("`sy" "\\synopsis" nil 0)
    ("`ta" "\\tabular" nil 0)
    ("`ti" "\\title" nil 0)
    ("`us" "\\usage" nil 0)
    ("`va" "\\value" nil 0)
    ))

(define-abbrev-table 'comint-mode-abbrev-table '(
                                                 ))

(define-abbrev-table 'completion-list-mode-abbrev-table '(
                                                          ))

(define-abbrev-table 'debugger-mode-abbrev-table '(
                                                   ))

(define-abbrev-table 'diff-mode-abbrev-table '(
                                               ))

(define-abbrev-table 'edebug-eval-mode-abbrev-table '(
                                                      ))

(define-abbrev-table 'edit-abbrevs-mode-abbrev-table '(
                                                       ))

(define-abbrev-table 'elisp-byte-code-mode-abbrev-table '(
                                                          ))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '(
                                                     ))

(define-abbrev-table 'fundamental-mode-abbrev-table '(
                                                      ))

(define-abbrev-table
  'global-abbrev-table
  (append
   '(
     ("hlx" "===========================================================================" nil 0)
     ("abotu" "about" nil 0)
     ("absense" "absence" nil 0)
     ("abstinance" "abstinence" nil 0)
     ("accellerate" "accelerate" nil 0)
     ("accomodate" "accommodate" nil 0)
     ("accomodated" "accommodated" nil 0)
     ("accomodates" "accommodates" nil 0)
     ("accomodation" "accommodation" nil 0)
     ("acknowledgement" "acknowledgment" nil 0)
     ("acknowledgements" "acknowledgments" nil 0)
     ("aquire" "aquire" nil 0)
     ("aquired" "aquired" nil 0)
     ("aquires" "aquires" nil 0)
     ("aquiring" "aquiring" nil 0)
     ("aquisition" "acquisition" nil 0)
     ("aquisitions" "acquisitions" nil 0)
     ("aclux" "American Civil Liberties Union" nil 0)
     ("acutally" "actually" nil 0)
     ("acutal" "actual" nil 0)
     ("adn" "and" nil 0)
     ("afux" "Annotation File Utilities" nil 0)
     ("algoritm" "algorithm" nil 0)
     ("algoritms" "algorithms" nil 0)
     ("aslo" "also" nil 0)
     ("alphebetize" "alphabetize" nil 0)
     ("alphebetized" "alphabetized" nil 0)
     ("alphebetizing" "alphabetizing" nil 0)
     ("alrady" "already" nil 0)
     ("alwasy" "always" nil 0)
     ("anlysis" "analysis" nil 0)
     ("annoation" "annotation" nil 0)
     ("annoations" "annotations" nil 0)
     ("annoattion" "annotation" nil 0)
     ("annoattions" "annotations" nil 0)
     ("annotaion" "annotation" nil 0)
     ("annotaions" "annotations" nil 0)
     ("annotaiton" "annotation" nil 0)
     ("annotatin" "annotation" nil 0)
     ("annotatino" "annotation" nil 0)
     ("annotatinos" "annotations" nil 0)
     ("annotatins" "annotations" nil 0)
     ("annotatoin" "annotation" nil 0)
     ("annotatoins" "annotations" nil 0)
     ("anotation" "annotation" nil 0)
     ("anotations" "annotations" nil 0)
     ("applicaton" "application" nil 0)
     ;; I would prefer that only "AJ" expand to "A.J.", but abbrev-expand
     ;; doesn't seem to do anything for that.
     ;; ("AJ" "A.J." nil 0) ; don't like this; my convention is abbrevs end in x
     ;; ("ajx" "A.J." nil 0)
     ("anohter" "another" nil 0)
     ("anotehr" "another" nil 0)
     ("antoher" "another" nil 0)
     ("anythign" "anything" nil 0)
     ("anyting" "anything" nil 0)
     ("archtypical" "archtypal" nil 0)
     ("arguement" "argument" nil 0)
     ("arguemetn" "argument" nil 0)
     ("arguemnt" "argument" nil 0)
     ("arguemnts" "arguments" nil 0)
     ("argumetn" "argument" nil 0)
     ("argumnt" "argument" nil 0)
     ("aroudn" "around" nil 0)
     ("atrx" "automated target recognition" nil 0)
     ("autamate" "automate" nil 0)
     ("autamated" "automated" nil 0)
     ("avaialable" "available" nil 0)
     ("awth" "what" nil 0)
     ("bakc" "back" nil 0)
     ("baloon" "balloon" nil 0)
     ("becuase" "because" nil 0)
     ("becaues" "because" nil 0)
     ("beign" "being" nil 0)
     ("beginnig" "beginning" nil 0)
     ("bibtexx" "BibTeX" nil 0)
     ("Bissandye" "Bissyande" nil 0)
     ("blacklungx" "pneumonoultramicroscopicsilicovolcanoconiosis" nil 0)
     ("boudned" "bounded" nil 0)
     ("bax" "Buenos Aires" nil 0)
     ("cacmx" "Communications of the ACM" nil 0)
     ("caffiene" "caffeine" nil 0)
     ("cfx" "Checker Framework" nil 0)
     ("cfxs" "Checker Framework's" nil 0)
     ("cfissuex" "https://tinyurl.com/cfissue/" nil 0)
     ("cfwebx" "https://checkerframework.org/" nil 0)
     ("cifx" "Checker Inference Framework" nil 0)
     ("cahnge" "change" nil 0)
     ("cahnges" "changes" nil 0)
     ("chnage" "change" nil 0)
     ("chnages" "changes" nil 0)
     ("chastize" "chastise" nil 0)
     ("chastized" "chastised" nil 0)
     ("cehcker" "checker" nil 0)
     ("chceker" "checker" nil 0)
     ("cheif" "chief" nil 0)
     ("cna" "can" nil 0)
     ("comisserate" "commiserate" nil 0)
     ("commitee" "committee" nil 0)
     ("committment" "commitment" nil 0)
     ("comparision" "comparison" nil 0)
     ("comparisions" "comparisons" nil 0)
     ("copmare" "compare" nil 0)
     ("copmile" "compile" nil 0)
     ("ocmpile" "compile" nil 0)
     ("copmiler" "compiler" nil 0)
     ("copmlete" "complete" nil 0)
     ("copmuter" "computer" nil 0)
     ("computatino" "computation" nil 0)
     ("computatoin" "computation" nil 0)
     ("coudl" "could" nil 0)
     ("coudln" "couldn" nil 0)
     ("copule" "couple" nil 0)
     ("cpsx" "Cistercian Preparatory School" nil 0)
     ("crsuher" "crusher" nil 0)
     ("cruhser" "crusher" nil 0)
     ("crusehr" "crusher" nil 0)
     ("csailx" "Computer Science & Artificial Intelligence Lab" nil 0)
     ("curhser" "crusher" nil 0)
     ("cursher" "crusher" nil 0)
     ("csailx" "Computer Science and Artificial Intelligence Lab")
     ;; ("csex" "common subexpression elimination")
     ("csex" "Computer Science & Engineering")
     ("cuthere" "---------------- cut here ----------------" nil 0)
     ("d-px" "data-parallel" nil 0)
     ("d'ouvres" "d'oeuvres" nil 0)
     ("Daghstuhl" "Dagstuhl" nil 0)
     ;; ("dbx" "database" nil 0)
     ("d4jx" "Defects4J" nil 0)
     ("defien" "define" nil 0)
     ("definiton" "definition" nil 0)
     ("deifne" "define" nil 0)
     ("denoument" "denouement" nil 0)
     ("didnt" "didn't" nil 0)
     ("didnt'" "didn't" nil 0)
     ("differnt" "different" nil 0)
     ("dlbclx" "diffuse large B-cell lymphoma" nil 0)
     ("documenattion" "documentation" nil 0)
     ("doen" "done" nil 0)
     ("dont'" "don't" nil 0)
     ("dwon" "down" nil 0)
     ;; ("dwebx" "http://pag.csail.mit.edu/daikon/")
     ("eecsx" "Electrical Engineering and Computer Science" nil 0)
     ("egx" "i.e.," nil 0)
     ("embarass" "embarrass" nil 0)
     ("embarassed" "embarrassed" nil 0)
     ("embarassing" "embarrassing" nil 0)
     ("embarassment" "embarrassment" nil 0)
     ("emrnst" "mernst" nil 0)
     ("ewnh" "when" nil 0)
     ("ewreh" "where" nil 0)
     ("everythign" "everything" nil 0)
     ("exagerate" "exaggerate" nil 0)
     ("exagerated" "exaggerated" nil 0)
     ("exapmle" "example" nil 0)
     ("exmaple" "example" nil 0)
     ("examlpe" "example" nil 0)
     ("existant" "existent" nil 0)
     ("Ersnt" "Ernst" nil 0)
     ("fairx" "Fairness and Accuracy in Reporting" nil 0)
     ;; don't capitalize in this file
     ("feburary" "february" nil 0)
     ("flase" "false" nil 0)
     ;; ("fo" "of" nil 0)	; I too often type "fo r" for "for"
     ("followon" "follow-on" nil 0)
     ("forard" "forward" nil 0)
     ("forwad" "forward" nil 0)
     ("foward" "forward" nil 0)
     ("ofrward" "forward" nil 0)
     ("froward" "forward" nil 0)
     ("foudn" "found" nil 0)
     ;; This substitution doesn't work, for reasons I don't understand
     ("Fredo" "Frédo" nil 0)
     ("Fridya" "Friday" nil 0)
     ("fulfil" "fulfill" nil 0)
     ("funciton" "function" nil 0)
     ("funcitons" "functions" nil 0)
     ("functoin" "function" nil 0)
     ("functoins" "functions" nil 0)
     ("furhter" "further" nil 0)
     ("Futhermore" "Furthermore" nil 0)
     ("futhermore" "furthermore" nil 0)
     ("Garuav" "Gaurav" nil 0)
     ("gelcdx" "GNU Emacs Lisp Code Directory" nil 0)
     ("guage" "gauge" nil 0)
     ("jist" "gist" nil 0)
     ("godo" "good" nil 0)
     ("gsocx" "Google Summer of Code" nil 0)
     ("grey" "gray" nil 0)
     ("gridx" "GRiD" nil 0)
     ("gropu" "group" nil 0)
     ("gropus" "groups" nil 0)
     ("ahd" "had" nil 0)
     ("ahve" "have" nil 0)
     ("hasnt'" "hasn't" nil 0)
     ("haev" "have" nil 0)
     ("hlep" "help" nil 0)
     ("Helenex" "Hélène" nil 0)
     ("helenex" "Hélène" nil 0)
     ("howver" "however" nil 0)
     ("howeer" "however" nil 0)
     ("sohuld" "should" nil 0)
     ("sholud" "should" nil 0)
     ("hsould" "should" nil 0)
     ("htan" "than" nil 0)
     ("THanks" "Thanks" nil 0)		; this doesn't seem to work
     ("Tahnsk" "Thanks" nil 0)
     ("Thnaks" "Thanks" nil 0)
     ("Tahnsk" "thanks" nil 0)
     ("Thansk" "Thanks" nil 0)
     ("Thnaks" "Thanks" nil 0)
     ("Thakns" "Thanks" nil 0)
     ("htanks" "thanks" nil 0)
     ("tahnsk" "thanks" nil 0)
     ("thansk" "thanks" nil 0)
     ("thnaks" "thanks" nil 0)
     ("thakns" "thanks" nil 0)
     ("htat" "that" nil 0)
     ("hte" "the" nil 0)
     ("hteir" "their" nil 0)
     ("htem" "them" nil 0)
     ("hten" "then" nil 0)
     ("htere" "there" nil 0)
     ("htese" "these" nil 0)
     ("htey" "they" nil 0)
     ("htier" "their" nil 0)
     ("htig" "thing" nil 0)
     ("hting" "thing" nil 0)
     ("htings" "things" nil 0)
     ("htink" "think" nil 0)
     ("htis" "this" nil 0)
     ("tihs" "this" nil 0)
     ("threshhold" "threshold" nil 0)
     ("htough" "though" nil 0)
     ("througout" "throughout" nil 0)
     ("hvae" "have" nil 0)
     ("hwen" "when" nil 0)
     ("hwere" "where" nil 0)
     ("hwihc" "which" nil 0)
     ("hwich" "which" nil 0)
     ("whcih" "which" nil 0)
     ("hwo" "how" nil 0)
     ("htmltocx" "<p>Contents:</p>
<!-- start toc.  do not edit; run html-update-toc instead -->
<!-- end toc -->" nil 0)
     ("ideosyncracy" "idiosyncrasy" nil 0)
     ("ideosyncrasy" "idiosyncrasy" nil 0)
     ("idiosyncracy" "idiosyncrasy" nil 0)
     ("ideosyncrasies" "idiosyncrasies" nil 0)
     ("ideosyncracies" "idiosyncrasies" nil 0)
     ("ideosyncratic" "idiosyncratic" nil 0)
     ("ifthe" "if the" nil 0)
     ("ifhte" "if the" nil 0)
     ;; ("im" "I'm" nil 0)	; no!  intramural
     ("impedence" "impedance" nil 0)
     ("implementtaion" "implementation" nil 0)
     ("importnnx" "import org.checkerframework.checker.nullness.qual.NonNull" nil 0)
     ("importnblex" "import org.checkerframework.checker.nullness.qual.NonNull" nil 0)
     ("informatino" "information" nil 0)
     ("innoculation" "inoculation" nil 0)
     ("inot" "into" nil 0)
     ("instnace" "instance" nil 0)
     ("inthe" "in the" nil 0)
     ("intersting" "interesting" nil 0)
     ("intractible" "intractable" nil 0)
     ("invairant" "invariant" nil 0)
     ("invairants" "invariants" nil 0)
     ("invaraint" "invariant" nil 0)
     ("invaraints" "invariants" nil 0)
     ("invoction" "invocation" nil 0)
     ("ipx" "intellectual property" nil 0)
     ("irrelevent" "irrelevant" nil 0)
     ("isnt" "isn't" nil 0)
     ("itslef" "itself" nil 0)
     ("itermize" "itemize" nil 0)
     ("wmx" "Waldenstrom's macroglobulinemia" nil 0)
     ("iwht" "with" nil 0)
     ("iwth" "with" nil 0)
     ("withotu" "without" nil 0)
     ("jaques" "Jacques" nil 0)
     ("jheadx" "public class Someclass {

  public static void main(String[] args) {

  }

}" nil 0)
     ("javaheadx" "public class Someclass {

  public static void main(String[] args) {

  }

}" nil 0)
     ("jas" "Jamey and Sharon" nil 0)
     ;; ("maj" "Mel and Julie" nil 0)
     ;; The capitalized version does not work; must use the lowercase one
     ;; ("Jeremh" "Jeremy" nil 0)
     ("jeremh" "jeremy" nil 0)
     ("josex" "José" nil 0)
     ("jsut" "just" nil 0)
     ("judgement" "judgment" nil 0)
     ("judgements" "judgments" nil 0)
     ("knwo" "know" nil 0)
     ("konw" "know" nil 0)
     ("knowledgable" "knowledgeable" nil 0)
     ("labelled" "labeled" nil 0)
     ("labmda" "lambda" nil 0)
     ("langauge" "language" nil 0)
     ("lanugage" "language" nil 0)
     ("langauges" "languages" nil 0)
     ("lsat" "last" nil 0)
     ("latexx" "LaTeX" nil 0)
     ("latex2htmlx" "LaTeX2HTML" nil 0)
     ("latexx2html" "LaTeX2HTML" nil 0)
     ("latexx2htmlx" "LaTeX2HTML" nil 0)
     ;; This doesn't do the trick; it expands to Latex, not LaTeX
     ;; ("Latexx" "LaTeX" nil 0)
     ("bdx" "\\begin{description}\n\\item[]" nil 0)
     ("edx" "\\end{description}" nil 0)
     ("bex" "\\begin{enumerate}\n\\item" nil 0)
     ("eex" "\\end{enumerate}" nil 0)
     ("bxx" "\\begin{exercise}" nil 0)
     ("exx" "\\end{exercise}" nil 0)
     ("bfx" "\\begin{frame}\n\\frametitle{" nil 0)
     ("efx" "\\end{frame}" nil 0)
     ("bix" "\\begin{itemize}\n\\item" nil 0)
     ("eix" "\\end{itemize}" nil 0)
     ("bsx" "\\begin{solution}" nil 0)
     ("esx" "\\end{solution}" nil 0)
     ("bvx" "\\begin{Verbatim}" nil 0)
     ("bvxc" "\\begin{Verbatim}[commandchars=\\\\\\{\\}]" nil 0)
     ("evx" "\\end{Verbatim}" nil 0)
     ("itemsepx" "\\itemsep 0pt \\parskip 0pt" nil 0)
     ("Jspecify" "JSpecify" nil 0)
     ("lheadx" "% -*- Mode: LaTeX -*-
\\documentclass{article} % 10pt if not specified
\\usepackage[T1]{fontenc}
\\usepackage{pslatex}
\\usepackage{microtype}
\\usepackage{fullpage}
\\usepackage{relsize}
\\usepackage{url}

\\begin{document}" nil 0)
     ("Lazarox" "Lázaro" nil 0)
     ("lcsx" "Laboratory for Computer Science" nil 0)
     ("lcex" "loop common expression" nil 0)
     ("lces" "loop common expressions" nil 0)
     ("lcesx" "loop common expressions" nil 0)
     ("lenght" "length" nil 0)
     ("liason" "liaison" nil 0)
     ("liasons" "liaisons" nil 0)
     ("liek" "like" nil 0)
     ("lpfx" "League for Programming Freedom" nil 0)
     ("lscx" "Lecture Series Committee" nil 0)
     ("massachussetts" "Massachusetts" nil 0)
     ;; does not work if first (misspelled) word is capitalized
     ("menrst" "mernst" nil 0)
     ("mdex" "Michael D. Ernst" nil 0)
     ("mex" "Michael Ernst" nil 0)
     ("miek" "Mike" nil 0)
     ("mgiht" "might" nil 0)
     ("mitx" "Massachusetts Institute of Technology" nil 0)
     ("migth" "might" nil 0)
     ("mroe" "more" nil 0)
     ("msut" "must" nil 0)
     ("muhc" "much" nil 0)
     ("non-sensical" "nonsensical" nil 0)
     ("norht" "north" nil 0)
     ("nothign" "nothing" nil 0)
     ("nto" "not" nil 0)
     ("nubmer" "number" nil 0)
     ("nubmers" "numbers" nil 0)
     ("numers" "numbers" nil 0)
     ("nytx" "New York Times" nil 0)
     ("occurrance" "occurrence" nil 0)
     ("occured" "occurred" nil 0)
     ("occurence" "occurrence" nil 0)
     ("occurences" "occurrences" nil 0)
     ("ofthe" "of the" nil 0)
     ("ohter" "other" nil 0)
     ("ohters" "others" nil 0)
     ("ot" "to" nil 0)
     ("otehr" "other" nil 0)
     ("otehrs" "others" nil 0)
     ;; also a typo for "appear":  ("apper" "paper" nil 0)
     ("parallle" "parallel" nil 0)
     ("paralllel" "parallel" nil 0)
     ("parseable" "parsable" nil 0)
     ("pepole" "people" nil 0)
     ("plsex" "programming languages and software engineering" nil 0)
     ("peopel" "people" nil 0)
     ("poeple" "people" nil 0)
     ("perahps" "perhaps" nil 0)
     ("perhpas" "perhaps" nil 0)
     ("publicaton" "publication" nil 0)
     ("publicatons" "publications" nil 0)
     ("preferrable" "preferable" nil 0)
     ("preferrably" "preferably" nil 0)
     ("prefering" "preferring" nil 0)
     ("prot" "port" nil 0)
     ("queston" "question" nil 0)
     ("questons" "questions" nil 0)
     ("questino" "question" nil 0)
     ("questinos" "questions" nil 0)
     ("Randop" "Randoop" nil 0)
     ("Ranier" "Rainier" nil 0)
     ("recieve" "receive" nil 0)
     ("refernce" "reference" nil 0)
     ("renex" "René" nil 0)
     ("renex's" "René's" nil 0)
     ("renes" "René's" nil 0)
     ("renejust" "René Just" nil 0)
     ("represetn" "represent" nil 0)
     ("reprot" "report" nil 0)
     ("reserach" "research" nil 0)
     ("rserach" "research" nil 0)
     ("rsearch" "research" nil 0)
     ("reseach" "research" nil 0)
     ;; ("rmsx" "Richard Stallman" nil 0)
     ("rpogram" "program" nil 0)
     ("sabattical" "sabbatical" nil 0)
     ("scaleable" "scalable" nil 0)
     ("sdx" "superdescriptor" nil 0)
     ("seciton" "section" nil 0)
     ("sectino" "section" nil 0)
     ("sectoin" "section" nil 0)
     ("sectin" "section" nil 0)
     ("seperate" "separate" nil 0)
     ("settab" "tabset" nil 0)
     ("shoudl" "should" nil 0)
     ("shoudln" "shouldn" nil 0)
     ("shiram" "shriram" nil 0)
     ("singal" "signal" nil 0)
     ("sizeable" "sizable" nil 0)
     ("softare" "software" nil 0)
     ("soem" "some" nil 0)
     ("smoe" "some" nil 0)
     ("soemthig" "something" nil 0)
     ("soemthign" "something" nil 0)
     ("soemthing" "something" nil 0)
     ("somehting" "something" nil 0)
     ("somethig" "something" nil 0)
     ("somethign" "something" nil 0)
     ("somethng" "something" nil 0)
     ("someting" "something" nil 0)
     ("everythig" "everything" nil 0)
     ("everyting" "everything" nil 0)
     ("everyhting" "everything" nil 0)
     ("sengx" "software engineering" nil 0)
     ;; ("srex" "summer renter" nil 0)
     ("stirng" "string" nil 0)
     ("studnet" "student" nil 0)
     ("studnets" "students" nil 0)
     ("stundent" "student" nil 0)
     ("stundents" "students" nil 0)
     ("subsidary" "subsidiary" nil 0)
     ("supercede" "supersede" nil 0)
     ("superceded" "superseded" nil 0)
     ("supercededby" "supersededby" nil 0) ; for BibTeX
     ("supercedes" "supersedes" nil 0)
     ("superceding" "superseding" nil 0)
     ("syncophant" "sycophant" nil 0)
     ("tahn" "than" nil 0)
     ("tahnks" "thanks" nil 0)
     ("taht" "that" nil 0)
     ("taht's" "that's" nil 0)
     ;; ("tat" "that" nil 0)	; tit for tat
     ("techique" "technique" nil 0)
     ("tecynique" "technique" nil 0)
     ("teh" "the" nil 0)
     ("tehm" "them" nil 0)
     ("tehn" "then" nil 0)
     ("tehre" "there" nil 0)
     ("tehy" "they" nil 0)
     ("teptex" "TE$\\Phi$" nil 0)
     ("teptx" "TE$\\Phi$" nil 0)
     ("tepx" "Tau Epsilon Phi" nil 0)
     ("texx" "TeX" nil 0)
     ("THe" "The" nil 0)
     ("thats" "that's" nil 0)
     ("theer" "there" nil 0)
     ("thier" "their" nil 0)
     ("thig" "thing" nil 0)
     ("thikn" "think" nil 0)
     ("thinkg" "think" nil 0)
     ("thna" "than" nil 0)
     ("thne" "then" nil 0)
     ("thnik" "think" nil 0)
     ("thoguh" "though" nil 0)
     ("thsi" "this" nil 0)
     ("thta" "that" nil 0)
     ("thye" "they" nil 0)
     ("tiem" "time" nil 0)
     ("tihnk" "think" nil 0)
     ("tiself" "itself" nil 0)
     ("tnah" "than" nil 0)
     ("toher" "other" nil 0)
     ("tractible" "tractable" nil 0)
     ("trajedy" "tragedy" nil 0)
     ("ubax" "University of Buenos Aires" nil 0)
     ("underly" "underlie" nil 0)
     ("undersand" "understand" nil 0)
     ("undersanding" "understanding" nil 0)
     ("usax" "United States of America" nil 0)
     ("usx" "United States" nil 0)
     ("uwx" "University of Washington" nil 0)
     ("unmoveable" "unmovable" nil 0)
     ("useable" "usable" nil 0)
     ("varaible" "variable" nil 0)
     ("varaibles" "variables" nil 0)
     ("verbage" "verbiage" nil 0)      ; "verbage" is correct but rare
     ("versino" "version" nil 0)
     ("verfication" "verification" nil 0)
     ("vicotry" "victory" nil 0)
     ("VisAssert" "VizAssert" nil 0)
     ("voidemail" "voicemail" nil 0)
     ("waht" "what" nil 0)
     ("wahtever" "whatever" nil 0)
     ("watier" "waiter" nil 0)
     ("whcih" "which" nil 0)
     ("wihch" "which" nil 0)
     ("wehn" "when" nil 0)
     ("wehre" "where" nil 0)
     ("wellx" "Wellesley" nil 0)
     ("whereever" "wherever" nil 0)
     ("whehter" "whether" nil 0)
     ("wehther" "whether" nil 0)
     ("wehtehr" "whether" nil 0)
     ("whta" "what" nil 0)
     ("whne" "when" nil 0)
     ("whihc" "which" nil 0)
     ("wierd" "weird" nil 0)
     ("iwll" "will" nil 0)
     ("wiht" "with" nil 0)
     ("wnat" "want" nil 0)
     ("wneh" "when" nil 0)
     ("wont'" "won't" nil 0)
     ("woudl" "would" nil 0)
     ("woudln" "wouldn" nil 0)
     ("woudln't" "wouldn't" nil 0)
     ("wreeh" "where" nil 0)
     ("wrtx" "with respect to" nil 0)
     ("wsa" "was" nil 0)
     ("wtih" "with" nil 0)
     ;; ("wx" "Wellesley" nil 0)
     ("yoru" "your" nil 0)
     ("youre" "you're" nil 0)
     )

   `(
     ;;
     ;; Signatures and email-related
     ;;

     ;; both URLs work as of fall 2007
     ("mwebx" "http://homes.cs.washington.edu/~mernst/")
     ("mwebcx" "My calendar is in Google Apps as mernst@cs.washington.edu or at http://homes.cs.washington.edu/~mernst/calendar.html .")
     ("mwebdx" "http://homes.cs.washington.edu/~mernst/directions.html")
     ("catwebx" "http://cathowell.blogspot.com/")
     ("catblogx" "http://cathowell.blogspot.com/")
     ("twopdfx" "I have included the same coments in two different forms, because different people prefer different formats.")

     ;; ("mnlx" "mernst-nonlocal" nil 0)
     ("malx" "mernst@alum.mit.edu")
     ("mgx" "michael.ernst@gmail.com")
     ("mcfx" "mernst@cf-web.org")
     ("mrx" "mernst@cs.rice.edu")
     ("mwx" "mernst@cs.washington.edu")
     ;; ("mlx" "mernst@csail.mit.edu")
     ;; ("mcsx" "mernst@csail.mit.edu")
     ("mwsx" "                    -Michael Ernst
                     mernst@cs.washington.edu" nil 0)
     ("cmesx" ,(cond ((and (boundp 'system-site)
                           (or (eq system-site 'wash-cs)
                               (eq system-site 'csail)))
		      "                     Cheers,

                    -Michael Ernst
                     mernst@cs.washington.edu")
		     (t
		      "                     Cheers,

                    -Michael Ernst"))
      nil 0)
     ("tmesx" ,(cond ((and (boundp 'system-site)
                           (or (eq system-site 'wash-cs)
                               (eq system-site 'csail)))
		      "                     Thanks,

                    -Michael Ernst
                     mernst@cs.washington.edu")
		     (t
		      "                     Thanks,

                    -Michael Ernst"))
      nil 0)
     ("rmalx" "Reply-To: mernst@alum.mit.edu" nil 0)
     ;; ("fmlcsx" "From: Michael Ernst <mernst@csail.mit.edu>" nil 0)
     ;; was fmcsx
     ;; ("fmmx" "From: Michael Ernst <mernst@csail.mit.edu>" nil 0)
     ("fmwx" "From: Michael Ernst <mernst@cs.washington.edu>" nil 0)
     ("fmalx" "From: Michael Ernst <mernst@alum.mit.edu>" nil 0)
     ("fmswsx" "From: Michael Ernst <mernst@mpi-sws.mpg.de>" nil 0)
     ("fmsx" "From: Michael Ernst <mernst@mpi-sws.mpg.de>" nil 0)
     ;; ("fmsnx" "From: Michael Ernst <sn-oversight@geyer.lcs.mit.edu>" nil 0)
     ("rmcfx" "Reply-To: mernst@cf-web.org" nil 0)
     ("fmcfx" "From: Michael Ernst <mernst@cf-web.org>" nil 0)
     ("mesx" ,(cond ((and (boundp 'system-site)
                          (or (eq system-site 'wash-cs)
                              (eq system-site 'csail)))
		     "                    -Michael Ernst
                     mernst@cs.washington.edu")
		    (t
		     "                    -Michael Ernst"))
      nil 0)
     ;; ("mesnx" "                    -Michael Ernst
     ;;                  sn-oversight@geyer.lcs.mit.edu"
     ;;  nil 0)
     ("mpsx" "--
Michael Ernst
Professor, U. of Washington Computer Science & Engineering
mernst@cs.washington.edu   http://homes.cs.washington.edu/~mernst/"
      nil 0)
     ("msigx" "                    -Mike" nil 0)
     ("msx" "                    -Mike" nil 0)
     ("cmsx" "                     Cheers,

                    -Mike" nil 0)
     ("tmsx" "                     Thanks,

                    -Mike" nil 0)
     ("tmxs" "                     Thanks,

                    -Mike" nil 0)
     ("lmsx" "                     Love,

                    -Mike" nil 0)
     ("csx" "                    -Crusher" nil 0)
     ("ccsx" "                     Cheers,

                    -Crusher" nil 0)
     ("tcsx" "                     Thanks,

                    -Crusher" nil 0)
     ("chx" "                     Cheers," nil 0)


     ;;     ;; FAIR propaganda
     ;;     ("helpfair" "If you would like to help FAIR, please consider a tax-deductible donation
     ;; \(send to 130 W. 25th St., New York, NY 10001\) or a subscription to EXTRA!,
     ;; FAIR's award-winning magazine.  Call 1-800-847-3993 and use a credit card
     ;; to subscribe for only $19 per year." nil 0)
     ;;     ("fairsub" "I don't have access to subscription or mail order information; that is
     ;; handled by an outside company.  You can call 1-800-847-3993 to check on the
     ;; status." nil 0)

     ;;     ;; LPF propaganda
     ;;     ("lafxx" "By the way, what do you think of the look-and-feel lawsuits?" nil 0)
     ;;     ("lafx" "By the way, what do you think about look and feel lawsuits?  Do you think
     ;; it is a good thing for interfaces to be copyrighted and for algorithms to
     ;; be patented?" nil 0)
     ;;     ("lpfsig" "Member of the League for Programming Freedom --- send e-mail for details" nil 77)
     ;;     ("lpffx" "(Fight interface copyrights and software patents.
     ;;  Join the League for Programming Freedom: lpf@uunet.uu.net)" nil 0)
     ;;     ("lafxxx" "By the way, on another topic, how do you feel about look and feel
     ;; lawsuits?  Do you think it is a good thing for interfaces to be
     ;; copyrighted?" nil 0)
     ;;     ("patx" "By the way, on another topic, how do you feel about patents on algorithms
     ;; and software features?  Do you think it is a good thing for such patents
     ;; to be granted?" nil 0)
     )))

(define-abbrev-table 'help-mode-abbrev-table '(
                                               ))

(define-abbrev-table 'lisp-mode-abbrev-table '(
                                               ))

(define-abbrev-table 'messages-buffer-mode-abbrev-table '(
                                                          ))

(define-abbrev-table 'occur-edit-mode-abbrev-table '(
                                                     ))

(define-abbrev-table 'occur-mode-abbrev-table '(
                                                ))

(define-abbrev-table 'outline-mode-abbrev-table '(
                                                  ))

(define-abbrev-table 'package-menu-mode-abbrev-table '(
                                                       ))

(define-abbrev-table 'process-menu-mode-abbrev-table '(
                                                       ))

(define-abbrev-table 'prog-mode-abbrev-table '(
                                               ))

(define-abbrev-table 'shell-mode-abbrev-table '(
                                                ))

(define-abbrev-table 'special-mode-abbrev-table '(
                                                  ))

(define-abbrev-table 'tabulated-list-mode-abbrev-table '(
                                                         ))

(define-abbrev-table 'text-mode-abbrev-table '(
                                               ))

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '(
                                                          ))

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '(
                                                          ))

(define-abbrev-table 'vc-git-region-history-mode-abbrev-table '(
                                                                ))

