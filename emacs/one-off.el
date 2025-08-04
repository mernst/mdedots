;;; -*- lexical-binding: t -*-

;;; This file contains Emacs Lisp functions that were useful once but
;;; might never be used again.  Also see file bulk-mail.el .

(eval-when-compile
  (require 'etags)
  (require 'cc-cmds))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl
;;;

(defun perl-unused-vars (string)
  "Indicates which of the words in STRING are not used in the current buffer."
  (let ((pos 0)
	(result '()))
    (while (string-match "[a-zA-Z0-9_]+" string pos)
      (setq pos (match-end 0))
      (let ((word (match-string 0 string)))
	(save-excursion
	  (goto-char (point-min))
	  (if (not (re-search-forward
		    (concat "^[^#]*\\<" (regexp-quote word) "\\>")
		    nil t))
	      (setq result (cons word result))))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LaTeX
;;;

(defun convert-tex-commented ()
  "Convert TeX comments into \\begin{texcommented} ... \\end{texcommented}."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "%"))
      (progn
	(forward-line)
	(if (not (looking-at "%"))
	    (error "Where's the TeX comment?"))))
  (insert "\\begin{texcommented}\n")
  (while (looking-at "%+ ?")
    (replace-match "")
    (forward-line 1))
  (insert "\\end{texcommented}\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML
;;;

(defun html-eliminate-sans-serif-font ()
  (while (search-forward "<font face=\"Lucida,Verdana,Arial,sans-serif,Helvetica\">" nil t)
    (delete-region (match-beginning 0) (match-end 0))
    (search-forward "</font>")
    (delete-region (match-beginning 0) (match-end 0))))


(defun remove-matching-tags (tag1 tag2 &optional replacement)
  (goto-char (point-min))
  (let ((tag1-completed
	 (if (string-match ">$" tag1)
	     tag1
	   (concat tag1 ".*?>"))))
    (while (re-search-forward
	    (concat "<" tag1-completed "\\(.*?\\)<" tag2 ".*?>")
	    nil t)
      (if replacement
	  (replace-match replacement)
        (replace-match "\\1"))
      (goto-char (point-min)))))


(defun openoffice-slides-html-to-indented-text ()
  (interactive)
  (goto-char (point-min))
  (delete-matching-lines "^<p[^\n<>]*>$")
  (goto-char (point-min))
  (replace-string-noninteractive "</P>" "")
  (goto-char (point-min))
  (while (re-search-forward "<FONT" nil t)
    (if (not (looking-at ".*</FONT>"))
	(progn
	  (end-of-line)
	  (insert " ")
	  (delete-char 1)
	  (goto-char (point-min)))))
  (remove-matching-tags "FONT FACE" "/FONT")
  (remove-matching-tags "SPAN" "/SPAN")
  (remove-matching-tags "FONT COLOR=\"#000000\"" "/FONT")  ; black
  (remove-matching-tags "FONT COLOR=\"#3333cc\"" "/FONT")  ; blue
  (remove-matching-tags "FONT FACE=\"Times New Roman, serif\"" "/FONT")
  ;; Size:
  ;; <FONT SIZE=7 STYLE="font-size: 44pt">
  ;; <FONT SIZE=7 STYLE="font-size: 32pt">
  ;; <FONT SIZE=6>
  ;; <FONT SIZE=6 STYLE="font-size: 28pt">
  ;; <FONT SIZE=5 STYLE="font-size: 20pt">
  ;; <FONT SIZE=5>
  (remove-matching-tags "FONT SIZE=7 STYLE=\"font-size: 44pt\">" "/FONT" " \\1")
  (remove-matching-tags "FONT SIZE=7 STYLE=\"font-size: 40pt\">" "/FONT" "  \\1")
  (remove-matching-tags "FONT SIZE=7 STYLE=\"font-size: 32pt\">" "/FONT" "   \\1")
  (remove-matching-tags "FONT SIZE=6 STYLE=\"font-size: 28pt\">" "/FONT" "    \\1")
  (remove-matching-tags "FONT SIZE=6>" "/FONT"                           "     \\1")
  (remove-matching-tags "FONT SIZE=5 STYLE=\"font-size: 20pt\">" "/FONT" "      \\1")
  (remove-matching-tags "FONT SIZE=5>" "/FONT"                           "       \\1")

  (goto-char (point-min))
  (replace-regexp-noninteractive "<BR>\n\n" "")
  (goto-char (point-min))
  (replace-regexp-noninteractive "^ <B>\\(.*\\)</B>" "\n \\1") ; do not anchor with "$"
  (remove-matching-tags "FONT COLOR=\"#993300\"" "/FONT" "\\\\smaller{\\1}")

  )


;; To change URLs like
;; https://annotation-tools.googlecode.com/hg
;; to
;; https://code.google.com/p/annotation-tools/
;; 
;; cd
;; findfile hgrc > ~/find-hgrc
;; etags ~/.mvc-checkouts `cat ~/find-hgrc`
;; # In Emacs:
;; (visit-tags-table "~/TAGS" nil)
;; ### This needs to be tested!
;; (tags-query-replace "https://wiki\\.\\(.*\\).googlecode.com/hg/?$" "https://code.google.com/p/\\1.wiki/")
;; (tags-query-replace "https://\\(.*\\).googlecode.com/hg/?$" "https://code.google.com/p/\\1/")


(defun update-checker-framework-urls ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (query-replace "http://types.cs.washington.edu/checker-framework/current/checkers-manual.html" "https://checkerframework.org/manual/")
    (goto-char (point-min))
    (query-replace-regexp "https?://types.cs.washington.edu/annotation-file-utilities/" "https://checkerframework.org/annotation-file-utilities/")
    (goto-char (point-min))
    (query-replace-regexp "https?://types.cs.washington.edu/checker-framework/current/checker-framework-manual.html" "https://checkerframework.org/manual/")
    (goto-char (point-min))
    (query-replace "http://types.cs.washington.edu/checker-framework/current/api/" "https://checkerframework.org/api/")
    (goto-char (point-min))
    (query-replace-regexp "https?://types.cs.washington.edu/checker-framework/(current/)?api/" "https://checkerframework.org/api/")
    (goto-char (point-min))
    (query-replace "http://types.cs.washington.edu/checker-framework/tutorial/" "https://checkerframework.org/tutorial/")
    (goto-char (point-min))
    (query-replace "http://types.cs.washington.edu/checker-framework/" "https://checkerframework.org/")
    (goto-char (point-min))
    (query-replace "http://types.cs.washington.edu/jsr308/" "https://checkerframework.org/jsr308/")
    (goto-char (point-min))
    (query-replace "http://checkerframework.org/" "https://checkerframework.org/")
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Address labels
;;;

;; example: (address-labels "xmas")
(defun address-labels (tag)
  "Generate a buffer containing entries from addresses.tex that match regexp TAG."
  (interactive "sRegexp: ")
  (let ((buf (generate-new-buffer "address-labels")))
    (pop-to-buffer buf))
  (insert-file-contents (expand-file-name "~/private/addresses.tex"))
  ;; Delete non-matching paragraphs
  (goto-char (point-min))
  (delete-non-matching-paragraphs tag)
  ;; needed due to bug in delete-non-matching-paragraphs
  (if (search-forward "% -*- Mode: text -*-\n" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
  ;; Do the real work
  (address-labels-helper))

(defun address-labels-helper ()
  "Call this in a buffer containing entries from addresses.tex."
  ;; Remove post-address info
  (goto-char (point-min))
  (while (re-search-forward "^[-A-Za-z.]+\\( [-A-Za-z.]+\\( [-A-Za-z.]+\\)?\\)?, [A-Z][A-Z]  ?\\([0-9][0-9][0-9][0-9][0-9]\\(-[0-9][0-9][0-9][0-9]\\)?\\|[A-Z][0-9][A-Z] [0-9][A-Z][0-9]\nCanada\\|[0-9][0-9][0-9][0-9]\nAustralia\\)\n" nil t)
    (kill-paragraph 1))
  ;; Try to put last names after first names
  (goto-char (point-min))
  (while (not (eobp))
    (skip-chars-forward "\n")
    (while (looking-at ".*\\( `[^']*'\\)")
      (delete-region (match-beginning 1) (match-end 1)))
    (while (looking-at ".*\\( (nee .*)\\| [A-Z]\\.\\)\\($\\| &\\)")
      (delete-region (match-beginning 1) (match-end 1)))
    (let ((spouse (and (looking-at ".*\\( & \\(.*\\) (no nee)\\)$")
		       (prog1 (match-string 2)
			 (delete-region (match-beginning 1) (match-end 1))))))
      (cond ((looking-at "[-A-Za-z]+\\( [-A-Za-z.]+\\)?, ")
	     (goto-char (match-end 0))
	     (backward-delete-char 2)
	     (let* ((firstname-start (point))
		    (firstname-end (progn (end-of-line) (point)))
		    (firstname (buffer-substring firstname-start firstname-end)))
	       (delete-region firstname-start firstname-end)
	       (beginning-of-line)
	       (insert firstname " ")))
	    )
      (if spouse
	  (progn
	    (end-of-line)
	    (insert " & " spouse)))
      (forward-paragraph)
      )
    )
  )

(defun address-labels-remove-children ()
  "Remove any line between the first one and the street address."
  (interactive)
  (while (re-search-forward "\n\n" nil t)
    (forward-line 1)
    (let ((children-start (point)))
      (re-search-forward "^[0-9]")
      (delete-region children-start (match-beginning 0)))))

(defun address-labels-ensure-children ()
  "Ensure there is a line after the parents."
  (interactive)
  (while (re-search-forward "\n\n" nil t)
    (forward-line 1)
    (if (looking-at "^[0-9]")
	(insert ".\n"))))

(defun address-labels-map (tag)
  "TAG is, for example, \"wallingford-cookies-east\"."
  (address-labels tag)
  (goto-char 0)
  (delete-matching-paragraphs "address-labels-map")
  (goto-char 0)
  (insert "\n")
  (goto-char 0)
  ;; Instead of removing children, add a column for them.
  ;; (address-labels-remove-children)
  (address-labels-ensure-children)
  (goto-char 0)
  (delete-blank-lines)		   ; first invocation leaves one line
  (delete-blank-lines)		   ; second invocation removes the one line
  (convert-newlines-to-tab-separated)
  (goto-char 0)
  (insert "Name\tKids\tStreet\tCity\n")
  (delete-trailing-whitespace)
  (write-file (concat tag ".tsv")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistics
;;;

(defun precision-recall-f-measure (tp fp fn)
  "Arguments are: true positives, true negatives, false negatives."
  ;; Uses of "0.0" are to force floating-point computation.
  (let* ((precision (/ tp (+ tp fp 0.0)))
	 (recall (/ tp (+ tp fn 0.0)))
	 (f-measure (* 2 (/ (* precision recall) (+ precision recall)))))
    (list precision recall f-measure)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java
;;;

(defun ensure-in-same-file (regex1 regex2)
  "Ensure that if regex1 is in any file, then regex2 is in that file too."
  (tags-search regex1)
  (while t
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regex2))
    (fileloop-continue)))
;; (ensure-in-same-file "\n *@Target" "import java.lang.annotation.Target;")
;; (ensure-in-same-file "import java.lang.annotation.Annotation;" " extends Annotation\\b\\|\\bAnnotation\\.class\\|(Annotation ")
;; (ensure-in-same-file "import java.lang.annotation.Documented;" "@Documented\\b")
;; (ensure-in-same-file "import java.lang.annotation.ElementType;" "@Target(.*\\(\n.*\\)?ElementType\\.\\|ElementType#\\|<ElementType>")
;; (ensure-in-same-file "import java.lang.annotation.Inherited;" "@Inherited\\b")
;; (ensure-in-same-file "import java.lang.annotation.Repeatable;" "@Repeatable\\b")
;; (ensure-in-same-file "import java.lang.annotation.Retention;" "@Retention\\b")
;; (ensure-in-same-file "import java.lang.annotation.RetentionPolicy;" "@Retention(.*\\bRetentionPolicy\\b")
;; (ensure-in-same-file "import java.lang.annotation.Target;" "@Target\\b\\|\\bTarget\\.class")


(defun add-import-if-not-present (regex1 string2)
  "Ensure that if regex1 is in any file, then string2 is in that file too."
  (tags-search regex1)
  (while t
    (save-excursion
      (goto-char (point-min))
      (if (and (not (search-forward string2 nil t))
	       (equal ".java" (substring (buffer-file-name) -5)))
	  (progn
	    (or (re-search-forward "\\`import\\|\n\nimport" nil t)
		(re-search-forward "^package.*\n" nil t))
	    (beginning-of-line)
	    (insert string2)
	    (newline-and-indent))))
    (fileloop-continue)))
(if nil
    (progn
      (add-import-if-not-present "\n *@Target" "import java.lang.annotation.Target;")
      (add-import-if-not-present "\n *@Retention" "import java.lang.annotation.Retention;")
      (add-import-if-not-present "\n *@Target" "import java.lang.annotation.Target;")
      (add-import-if-not-present "\n *@Repeatable" "import java.lang.annotation.Repeatable;")
      (add-import-if-not-present "\n *@Inherited\\b" "import java.lang.annotation.Inherited;")
      (add-import-if-not-present "@Retention.*RetentionPolicy\." "import java.lang.annotation.RetentionPolicy;")
      (add-import-if-not-present "@Target.*ElementType\." "import java.lang.annotation.ElementType;")
      (add-import-if-not-present "\n *@PolymorphicQualifier\\b" "import org.checkerframework.framework.qual.PolymorphicQualifier;")
      (add-import-if-not-present "\n *@DefaultQualifier\\b" "import org.checkerframework.framework.qual.DefaultQualifier;")
      (add-import-if-not-present "\n *@DefaultQualifiers\\b" "import org.checkerframework.framework.qual.DefaultQualifiers;")
      (add-import-if-not-present "\n *@DefaultQualifierInHierarchy\\b" "import org.checkerframework.framework.qual.DefaultQualifierInHierarchy;")
      (add-import-if-not-present "\n *@Dependent\\b" "import org.checkerframework.framework.qual.Dependent;")
      (add-import-if-not-present "\n *@InvisibleQualifier\\b" "import org.checkerframework.framework.qual.InvisibleQualifier;")
      (add-import-if-not-present "\n *@SubtypeOf\\b" "import org.checkerframework.framework.qual.SubtypeOf;")
      (add-import-if-not-present "\n *@FieldIsExpression\\b" "import org.checkerframework.framework.qual.FieldIsExpression;")
      (add-import-if-not-present "\n *@ImplicitFor\\b" "import org.checkerframework.framework.qual.ImplicitFor;")
      (add-import-if-not-present "\n *@PolyAll\\b" "import org.checkerframework.framework.qual.PolyAll;")
      (add-import-if-not-present "\\bLiteralKind\\." "import org.checkerframework.framework.qual.LiteralKind;")

      (add-import-if-not-present "[={] *TypeUseLocation\\." "import org.checkerframework.framework.qual.TypeUseLocation;")
      (add-import-if-not-present "\n *@AnnotatedFor\\b" "import org.checkerframework.framework.qual.AnnotatedFor;")
      (add-import-if-not-present "\n *@Unused\\b" "import org.checkerframework.framework.qual.Unused;")
      (add-import-if-not-present "\n *@EnsuresQualifier\\b" "import org.checkerframework.framework.qual.EnsuresQualifier;")
      (add-import-if-not-present "\n *@EnsuresQualifiers\\b" "import org.checkerframework.framework.qual.EnsuresQualifiers;")
      (add-import-if-not-present "\n *@EnsuresQualifierIf\\b" "import org.checkerframework.framework.qual.EnsuresQualifierIf;")
      (add-import-if-not-present "\n *@EnsuresQualifiersIf\\b" "import org.checkerframework.framework.qual.EnsuresQualifiersIf;")
      (add-import-if-not-present "\n *@RequiresQualifier\\b" "import org.checkerframework.framework.qual.RequiresQualifier;")
      (add-import-if-not-present "\n *@RequiresQualifiers\\b" "import org.checkerframework.framework.qual.RequiresQualifiers;")
      (add-import-if-not-present "^ *[^ /*].*[ (]AbstractCollection<" "import java.util.AbstractCollection;")
      (add-import-if-not-present "^ *[^ /*].*[ (]ArrayList<" "import java.util.ArrayList;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Collection<" "import java.util.Collection;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Comparator<" "import java.util.Comparator;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Dictionary<" "import java.util.Dictionary;")
      (add-import-if-not-present "^ *[^ /*].*[ (]HashMap<" "import java.util.HashMap;")
      (add-import-if-not-present "^ *[^ /*].*[ (]HashSet<" "import java.util.HashSet;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Iterator<" "import java.util.Iterator;")
      (add-import-if-not-present "^ *[^ /*].*[ (]LinkedHashMap<" "import java.util.LinkedHashMap;")
      (add-import-if-not-present "^ *[^ /*].*[ (]LinkedHashSet<" "import java.util.LinkedHashSet;")
      (add-import-if-not-present "^ *[^ /*].*[ (]LinkedList<" "import java.util.LinkedList;")
      (add-import-if-not-present "^ *[^ /*].*[ (]List<" "import java.util.List;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Map<" "import java.util.Map;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Map.Entry<" "import java.util.Map;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Set<" "import java.util.Set;")
      (add-import-if-not-present "^ *[^ /*].*[ (]SortedMap<" "import java.util.SortedMap;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Stack<" "import java.util.Stack;")
      (add-import-if-not-present "^ *[^ /*].*[ (]TreeSet<" "import java.util.TreeSet;")
      (add-import-if-not-present "^ *[^ /*].*[ (]Vector<" "import java.util.Vector;")
      (add-import-if-not-present "^ *[^ /*].*\\bCollections.\\(emptySet\\|unmodifiableSet\\|sort\\|<.*>emptyList\\|singleton\\)(" "import java.util.Collections;")
      (add-import-if-not-present "^ *[^ /*].*\\bCollections.EMTPY_SET\\b" "import java.util.Collections;")
      (add-import-if-not-present "^ *[^ /*].*\\bnew Properties()" "import java.util.Properties;")
      (add-import-if-not-present "^ *[^ /*].*\\bnew NoSuchElementException()" "import java.util.NoSuchElementException;")
      (add-import-if-not-present "^ *[^ /*].*\\bResourceBundle.getBundle(" "import java.util.ResourceBundle;")
      (add-import-if-not-present "^ *[^ /*].*\\bLocale.getDefault()" "import java.util.Locale;")
      (add-import-if-not-present "^ *[^ /*].*\\bnew ArrayList()" "import java.util.ArrayList;")
      (add-import-if-not-present "^ *[^ /*].*\\bnew AbstractMap\\." "import java.util.AbstractMap;")
      (add-import-if-not-present "^ *[^ /*].*\\bArrays.\\(deepEquals\\|asList\\)(" "import java.util.Arrays;")

      ))


;; Handle all the @Nullable annotations before calling this last.
(defun stub-decl-annos-to-source ()
  "Assume stub annotations are at beginning of file.
Transfer them to source that appears later in the file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\(^ *public \\(?:final \\)?boolean equals(\\)Object " "\\1@Nullable Object "))
  (while (re-search-forward "\\(@SideEffectFree\\|@Pure\\) \\(public [^(]*([^ )]*\\)" nil t)
    (let ((anno (match-string 1))
	  (signature (match-string 2)))
      (save-excursion
	(search-forward signature)
	(beginning-of-line)
	(open-line 1)
	(c-indent-line-or-region)
	(insert anno)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Log cleanup (Travis, Randoop, Gradle)
;;;


;; Previously named travis-cleanup-output.
(defun uncolorize ()
  "Remove color from a shell command output"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[34m\\[1m\\(.*?\\)\\[0m" "\\1")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[0K\\[33;1m\\(.*?\\)\\[0m" "\\1")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[3[0-4];1m\\(.*?\\)\\[[01]m" "\\1")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[[01];3[0-6]m\\(.*?\\)\\[\\(0;1\\)?m" "\\1")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[1m\\(.*?\\)\\[m" "\\1")
    (goto-char (point-min))
    (replace-string-noninteractive "\n" "\n")
    (goto-char (point-min))
    (replace-string-noninteractive "" "\n")
    (goto-char (point-min))
    (replace-string-noninteractive "[K\n" "\n")
    (goto-char (point-min))
    (replace-string-noninteractive "[0m\n" "\n")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\n\\[0K" "\n")
    ))


(defun azure-cleanup-output ()
  "Remove extraneous characters from an Azure Pipelines CI log."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-noninteractive "^20[0-9][0-9]-[01][0-9]-[0-3][0-9]T[012][0-9]:[0-5][0-9]:[0-9][0-9]\\.[0-9][0-9][0-9]\\([0-9][0-9][0-9][0-9]Z\\|-[01][0-9]00\\) " "")
    (goto-char (point-min))
    (replace-regexp-noninteractive "^20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]\\+[0-9][0-9][0-9][0-9] " "")
    (goto-char (point-min))
    (query-replace-regexp "at \\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) [0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] PM" "at DATE")
    (goto-char (point-min))
    (replace-regexp-noninteractive "^/__w/1/s/" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive "^/root/project/" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive ", Time elapsed: [0-9.]+ sec" "")
    (goto-char (point-min))
    (replace-string-noninteractive "[1m" "")
    (goto-char (point-min))
    (replace-string-noninteractive "[m" "")
    (goto-char (point-min))
    (replace-string-noninteractive "[36;1m" "")
    (goto-char (point-min))
    (delete-matching-lines "^##\\[debug\\]")
    )
  (uncolorize)
  )

(defun maven-cleanup-output ()
  "Clean up Maven output, to make diffing easier."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-noninteractive " ([0-9.]+ .?B at [0-9.]+ .?B/s)" "")
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\[INFO\\] Download\\(ed\\|ing\\) from central: .*\n\\)\\{2,\\}" nil t)
      (save-restriction
	(narrow-to-region (match-beginning 0) (match-end 0))
	(goto-char (point-min))
	(replace-regexp-noninteractive "^\\[INFO\\] Downloaded" "[INFO] ZDownloaded")
	(sort-lines nil (point-min) (point-max))
	(goto-char (point-min))
	(replace-regexp-noninteractive "^\\[INFO\\] ZDownloaded" "[INFO] Downloaded")
	(goto-char (point-max))))
    (goto-char (point-min))
    (while (re-search-forward (concat "\\[DEBUG\\] BND Instructions:\n"
				      "#-----------------------------------------------------------------------\n"
				      "#\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) "
				      "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Sep\\|Oct\\|Nov\\|Dec\\) "
				      ".*\n") nil t)
      (let ((env-start (match-end 0)))
	(if (search-forward "#-----------------------------------------------------------------------\n" nil t)
	    (sort-lines nil env-start (match-beginning 0)))))))




;; First convert from Mac 
(defun cleanup-randoop-log ()
  "Remove/normalize from Randoop output (or the randoop-log.txt file) lines
that differ from run to run, such as times and directory names."
  (interactive)
  (save-excursion

    (set-buffer-file-coding-system 'utf-8-unix)

    ;; Gradle and Travis control characters
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[[1-6]B\\[[1-6]A\\[2K\n" "")
    (goto-char (point-min))
    (replace-string-noninteractive "[34m[1m" "")
    (goto-char (point-min))
    (replace-string-noninteractive "[0m" "")
    (goto-char (point-min))
    (replace-string-noninteractive "[0K" "")
    (goto-char (point-min))
    (replace-string-noninteractive "\n[1B\n" "\n")
    (goto-char (point-min))
    (replace-string-noninteractive "[33;1m" "")
    (goto-char (point-min))
    (replace-string-noninteractive "[32;1m" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[2A.*\\[2A" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive "^Starting a Gradle Daemon.*\n" "")

    ;; Travis markers
    (goto-char (point-min))
    (delete-matching-lines "^travis_time:\\(start\\|end\\):")

    (goto-char (point-min))
    (replace-string-noninteractive "\n                                                                                                                                                                                                       \n\n" "\n")
    (goto-char (point-min))
    (replace-regexp-noninteractive "took [0-9.]+ secs" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive "^Total Time: [0-9]+\\.[0-9]+s.*" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive "^Average Time/Jar: [0-9]+\\.[0-9]+s.*" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive " Took [0-9]+ mins [0-9.]+ secs\\.$" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive " \\(UP-TO-DATE\\)$" "") ; leave NO-SOURCE in the file
    (goto-char (point-min))

    (replace-regexp-noninteractive "\\(BUILD SUCCESSFUL\\) in \\([0-9]+m \\)?[0-9]+s" "\\1")
    (goto-char (point-min))
    (replace-regexp-noninteractive "^\\(    Average method execution time.*?\\)[-+0-9.e]+$" "\\1")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\n    Progress update: .*      (\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) [0-9]+ [0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\(GMT\\|UTC\\|PDT\\|PST\\) 201[0-9])\n" "")
    (goto-char (point-min))
    (replace-regexp-noninteractive "      (\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) \\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) [0-9]+ [0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\(GMT\\|UTC\\|PDT\\|PST\\) 201[0-9])" "")

    (goto-char (point-min))
    (delete-matching-lines "^Creating new cache")
    (goto-char (point-min))
    (delete-matching-lines "^Starting [0-9]+\\(th\\) build in daemon")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\[\\(Task\\|Daemon\\) worker," "[*,")
    (goto-char (point-min))
    (replace-regexp-noninteractive "\\(Task\\|Daemon\\) worker Thread [0-9]+" "*")

    (goto-char (point-min))
    (replace-regexp-noninteractive "/homes/gws/" "/home/")
    (goto-char (point-min))
    (replace-regexp-noninteractive "/home\\(s/gws\\)?/mernst/research/testing/randoop\\(-REAL\\|-fork-jkotalik\\)?" "~/randoop")
    (goto-char (point-min))
    (replace-regexp-noninteractive "/java-[78]-\\(openjdk-amd64\\|oracle\\)/" "/java/")

    (goto-char (point-min))
    (delete-matching-lines "^\\(Receiving objects\\|Resolving deltas\\):")
    (goto-char (point-min))
    (delete-matching-lines "10kB [0-9.]+MB/s eta [0-9:]+")
    (goto-char (point-min))
    (delete-matching-lines "^[0-9a-f]+: \\(Download complete\\|Pull complete\\|Pulling fs layer\\|Verifying Checksum\\|Waiting\\|\\(Downloading\\|Extracting\\) +[0-9.]+ [kM]?B/[0-9.]+ [kM]?B\\) *$")

    (goto-char (point-min))
    ;; (remove-hashcodes)
    (canonicalize-hashcodes)

    (goto-char (point-min))
    (sort-gradle-tests-buffer)

    ))

(defun remove-hashcodes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-noninteractive "@[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]?[0-9a-f]?" "")
    ))

(defun canonicalize-hashcodes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((table (make-hash-table))
	  (last-canonical 0))
      (while (re-search-forward "\\(@\\)\\([0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]*\\)\\([^0-9a-f]\\)" nil t)
	(let* ((hashcode (substring-no-properties (match-string 2)))
	       (canonical (or (gethash hashcode table)
			      (puthash hashcode (format "ihc%d" (cl-incf last-canonical)) table))))
	  (replace-match canonical t t nil 2)))
      )))


(defun sort-gradle-tests-buffer ()
  "Call `sort-gradle-tests-region' on each section of the file
that is delimited by two lines starting with a colon."
  (interactive)
  (while (not (eobp))
    (beginning-of-line)
    (if (re-search-forward "^:" nil t)
	(progn
	  (forward-line)
	  (let ((region-start (point)))
	    (if (re-search-forward "^:" nil t)
		(progn
		  (beginning-of-line)
		  (let ((region-end (point)))
		    (sort-gradle-tests-region region-start region-end)))
	      ;; trigger "(eobp)" condition
	      (goto-char (point-max)))))
      (goto-char (point-max)))))

(defun sort-gradle-tests-region (beg end)
  "Sort gradle tests in the region."
  (goto-char beg)
  (let* ((paragraph-start "^randoop.* > .* \\(STARTED\\|STANDARD_OUT\\)")
	 (paragraph-separate (concat "$\n" paragraph-start)))
    (if (save-excursion (re-search-forward paragraph-start end t))
	(progn
	  (save-excursion
	    (save-restriction ; "end" won't be accurate because we change the size of the text
	      (narrow-to-region beg end)
	      (goto-char beg)
	      (replace-regexp-noninteractive
	       ;; "For historical compatibility reasons, ‘$’ can be used only at the
	       ;; end of the regular expression, or before ‘\)’ or ‘\|’."  [Yuck!]
	       "^\\(\\(randoop.* > .*\\) \\(?:STARTED\\|STANDARD_OUT\\)\\)\n\n\\(\\2 \\(?:PASSED\\|STANDARD_OUT\\)\\)"
	       "\\1{{{LINEBREAK}}}\\3")
	      (goto-char beg)
	      (sort-paragraphs nil beg (point-max))
	      (goto-char beg)
	      (replace-string-noninteractive "{{{LINEBREAK}}}" "\n\n")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fix voice-to-text
;;;

(defun voice-to-text-fix ()
  "Fix some typos in a buffer containing dictated text."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (save-excursion (replace-regexp-noninteractive "  +" " "))
    (save-excursion (replace-regexp-noninteractive "^ +" ""))
    (save-excursion (replace-string-noninteractive "“ " "“"))
    (save-excursion (replace-string-noninteractive " \"," "\","))
    (save-excursion (replace-string-noninteractive "( " "("))
    (save-excursion (replace-string-noninteractive " )" ")"))
    (save-excursion (replace-string-noninteractive "—" " -- "))
    (save-excursion (uneducate-quotes))
    (let ((case-fold-search nil))
      (save-excursion (while (re-search-forward "[.?!] +[a-z][a-z]" nil t)
                        (goto-char (match-beginning 0))
                        (capitalize-word 1))))
    (let ((case-fold-search nil))
      (save-excursion (while (re-search-forward "\n\n[a-z][^A-Z]" nil t)
                        (goto-char (match-beginning 0))
                        (capitalize-word 1))))
    (insert "[I should send only 10K characters at a time.]\nPlease improve the below text.  It was produced by automated voice-to-text and contains errors.  Please correct errors, but do not summarize or omit sentences.  Output only the corrected text.\n\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File transfer
;;;

(defun kindle-prepare-for-transfer ()
  (interactive)
  (find-file "~/tmp/")
  (find-file-other-window (substitute-in-file-name "/media/$USER/Kindle/documents/")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;
