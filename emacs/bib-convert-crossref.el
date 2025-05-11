;;; -*- lexical-binding: t -*-

;;; This file helps to convert a BibTeX file to use crossrefs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For creating @Proceedings entries
;;; 

(query-replace-regexp
 ;; 1=AAAI2000; 2=AAAI; 3=2000; 4=longtitle; 5=shorttitle
 ;; 6=month; 7=ignoredate
 ;; 8=address; 9=ignore
 "@string{\\(\\([A-Za-z]+\\)\\([0-9]+\\).*\\) = \"\\([^\"]*\\)\"[ \t\n]*=[ \t\n]*\"\\([^\"]*\\)\"}
@string{\\1date = \\([a-z][a-z][a-z]\\)\\( [#=] .*\\)?}
@string{\\1addr = \\(\"[^\"]*\"\\|[A-Za-z]+\\)\\( = .*\\)?}
"
"@Proceedings{\\1,
  title = 	 \"\\5: \\4\",
  booktitle = 	 \"\\5: \\4\",
  year = 	 \\3,
  address = 	 \\8,
  month = 	 \\6,
}
")

;; same as above, with addr before date
(query-replace-regexp
 ;; 1=AAAI2000; 2=AAAI; 3=2000; 4=longtitle; 5=shorttitle
 ;; 6=address; 7=ignore
 ;; 8=month; 9=ignoredate
 "@string{\\(\\([A-Za-z]+\\)\\([0-9]+\\).*\\) *= *\"\\([^\"]*\\)\"[ \t\n]*=[ \t\n]*\"\\([^\"]*\\)\"}
@string{\\1addr = \\(\"[^\"]*\"\\|[A-Za-z]+\\)\\( = .*\\)?}
@string{\\1date = \\([a-z][a-z][a-z]\\)\\( [#=] .*\\)?}
"
"@Proceedings{\\1,
  title = 	 \"\\5: \\4\",
  booktitle = 	 \"\\5: \\4\",
  year = 	 \\3,
  address = 	 \\6,
  month = 	 \\8,
}
")

;; No short title, address comes first
(query-replace-regexp
 ;; 1=AAAI2000; 2=AAAI; 3=2000; 4=longtitle; 5=shorttitle
 ;; 6=address; 7=ignore
 ;; 8=month; 9=ignoredate
 "@string{\\(\\([A-Za-z]+\\)\\([0-9]+\\).*\\) = \"\\([^\"]*\\)\"\\(\\)}
@string{\\1addr = \\(\"[^\"]*\"\\|[A-Za-z]+\\)\\( = .*\\)?}
@string{\\1date = \\([a-z][a-z][a-z]\\)\\( [#=] .*\\)?}
"
"@Proceedings{\\1,
  title = 	 \"\\4\",
  booktitle = 	 \"\\4\",
  year = 	 \\3,
  address = 	 \\6,
  month = 	 \\8,
}
")

;; No short title, date comes first
(query-replace-regexp
 ;; 1=AAAI2000; 2=AAAI; 3=2000; 4=longtitle; 5=shorttitle
 ;; 6=month; 7=ignoredate
 ;; 8=address; 9=ignore
 "@string{\\(\\([A-Za-z]+\\)\\([0-9]+\\).*\\) = \"\\([^\"]*\\)\"\\(\\)}
@string{\\1date = \\([a-z][a-z][a-z]\\)\\( [#=] .*\\)?}
@string{\\1addr = \\(\"[^\"]*\"\\|[A-Za-z]+\\)\\( = .*\\)?}
"
"@Proceedings{\\1,
  title = 	 \"\\4\",
  booktitle = 	 \"\\4\",
  year = 	 \\3,
  address = 	 \\8,
  month = 	 \\6,
}
")

(query-replace-regexp "\"\\(.*\\): \\1[ '0-9]*[:,] " "\"\\1: ")

(query-replace-regexp "\\(  year =[ \t]*\\)\\([0-9][0-9],\\)" "\\119\\2")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For updating @InProceedings entries
;;;

;; pages appears before month and address
(let ((case-fold-search t))
(tags-query-replace
"^[ \t]*booktitle[ \t]*=[ \t]*\\([A-Za-z0-9]+\\),
\\(?:[ \t]*year[ \t]*=[ \t]*[\"{]?[0-9]+[\"}]?,
\\)?\\([ \t]*\\(NEED\\|NO\\)?[Pp]ages[ \t]*=[ \t]*[\"{].*[\"}],\\)
\\(?:[ \t]*year[ \t]*=[ \t]*[\"{]?[0-9]+[\"}]?,
\\)?\\(?:[ \t]*address[ \t]*=[ \t]*\\1addr,
[ \t]*month[ \t]*=.*,?
\\|[ \t]*month[ \t]*=.*,
[ \t]*address[ \t]*=[ \t]*\\1addr,?
\\)\\(?:[ \t]*year[ \t]*=[ \t]*[\"{]?[0-9]+[\"}]?,?
\\)?"
"  crossref =     \"\\1\",
\\2
")
)

;; pages appears after month and address
(let ((case-fold-search t))
(tags-query-replace
"^[ \t]*booktitle[ \t]*=[ \t]*\\([A-Za-z0-9]+\\),
\\(?:[ \t]*year[ \t]*=[ \t]*[\"{]?[0-9]+[\"}]?,
\\)?\\(?:[ \t]*address[ \t]*=[ \t]*\\1addr,
[ \t]*month[ \t]*=.*,?
\\|[ \t]*month[ \t]*=.*,
[ \t]*address[ \t]*=[ \t]*\\1addr,
\\)\\(?:[ \t]*year[ \t]*=[ \t]*[\"{]?[0-9]+[\"}]?,
\\)?\\([ \t]*\\(NEED\\|NO\\)?[Pp]ages[ \t]*=[ \t]*[\"{].*[\"}],?\\)
\\(?:[ \t]*year[ \t]*=[ \t]*[\"{]?[0-9]+[\"}]?,?
\\)?"
"  crossref =     \"\\1\",
\\2
")
)

;; Only need to do this once ever
(tags-query-replace
"^\\([ \t]*pages[ \t]*=.*\\)
\\([ \t]*booktitle[ \t]*=[ \t]*\\(\"[^\"]*\"\\|{[^}]*}\\|[A-Za-z0-9]+\\),?\\)
"
"\\2
\\1
")


;; Add year to shortname
(tags-query-replace "{\\([A-Za-z]+\\)\\([0-9][0-9][0-9][0-9]\\)\\(,
.*\"\\)\\1\\([:,].*\\(?:
.*\\)?\\(?:
.*\\)?
.*\"\\)\\1\\([:,]\\)" "{\\1\\2\\3\\1 \\2\\4\\1 \\2\\5")
(tags-query-replace "{\\([A-Za-z]+\\)\\([0-9][0-9]\\)\\(,
.*\"\\)\\1\\([:,].*\\(?:
.*\\)?\\(?:
.*\\)?
.*\"\\)\\1\\([:,]\\)" "{\\1\\2\\3\\1 '\\2\\4\\1 '\\2\\5")

