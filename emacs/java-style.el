;;; -*- lexical-binding: t -*-

;; Improve the style of Java code

;; Parts of this are obviated by the use of automatic formatting tools,
;; such as google-java-format.

(defvar java-style-debug nil)

(require 'etags)
(require 'etags-mde)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java style
;;;

(defun java-replace-file-by-path ()
  "Replace uses of File by Path, in Java code.
File is obsolescent (but not yet deprecated); Path is better for new code.
It probably isn't worthwhile to replace *all* occurrences of File by Path in
existing code.
It's not even possible because many legacy APIs work with Files, such as
ProcessBuilder.directory(File).
So, it needs to be done carefully and slowly, probably file by file.
Oracle gives correspondences between File and Path:
http://docs.oracle.com/javase/tutorial/essential/io/legacy.html
These Emacs commands automate some of them."
  (interactive)
  ;; Todo: these first few should not trigger on `new File("string")
  (tags-query-replace-noerror "new File(\\([^,)]*\\), \\([^,)]*\\))" "\\1.resolve(\\2)")
  (tags-query-replace-noerror "new File(\\([^,)]*\\))" "Paths.get(\\1)")
  (tags-query-replace-noerror "\\(\\(^\\|,\\|(\\|/\\*@\\(NonNull\\|Nullable\\)\\*/\\) *\\)File\\b" "\\1Path")
  (tags-query-replace-noerror "<File>" "<Path>")
  ;; too many other "getName()" methods: (tags-query-replace-noerror "\\.getName()" ".toString()")
  (tags-query-replace-noerror "\\(file\\|path\\)\\.getName()" "\\1.getFileName().toString()")
  (tags-query-replace-noerror "\\.getParent()" ".getParent().toString()")
  (tags-query-replace-noerror "\\.getParentFile()" ".getParent()")
  (tags-query-replace-noerror "\\.getAbsolute\\(File\\|Path\\)()" ".toAbsolutePath().toString()")
  (tags-query-replace-noerror "\\b\\([A-Za-z0-9_]+\\)\\b\\.exists()" "Files.exists(\\1)")
  (tags-query-replace-noerror "\\b\\([A-Za-z0-9_]+\\)\\b\\.canRead()" "Files.isReadable(\\1)")
  (tags-query-replace-noerror "\\b\\([A-Za-z0-9_]+\\)\\b\\.canWrite()" "Files.isWritable(\\1)")
  (tags-query-replace-noerror "\\b\\([A-Za-z0-9_]+\\)\\b\\.canExecute()" "Files.isExecutable(\\1)")
  (tags-query-replace-noerror "\\b\\([A-Za-z0-9_]+\\)\\b\\.isDirectory()" "Files.isDirectory(\\1)")
  (tags-query-replace-noerror "\\b\\([A-Za-z0-9_]+\\)\\b\\.mkdirs()" "Files.createDirectory(\\1)")
  ;;
  (tags-search "deleteOnExit")
  )


(defun remove-redundant-null-test ()
  (interactive)
  (tags-query-replace-noerror "((\\([A-Za-z0-9_]+\\) != null) && \\((\\1 instanceof [A-Za-z0-9_]+)\\))" "\\2")
  (tags-query-replace-noerror "(\\([A-Za-z0-9_]+\\) != null && \\(\\1 instanceof [A-Za-z0-9_]+\\))" "(\\2)")
  (tags-query-replace-noerror "((\\([A-Za-z0-9_]+\\) == null) || \\(!(\\1 instanceof [A-Za-z0-9_]+)\\))" "(\\2)")
  (tags-query-replace-noerror "(\\([A-Za-z0-9_]+\\) != null || \\(!(\\1 instanceof [A-Za-z0-9_]+)\\))" "(\\2)")

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Delimiters
;;;

(defun tags-cleanup-parens ()
  "Remove parentheses."
  (interactive)

  (tags-replace "return (\\(-?[][A-Za-z0-9_.]+\\));$" "return \\1;")
  (tags-replace "return (\\(\\\"[^\\\"]*\\\"\\));$" "return \\1;")
  ;; This isn't fruitful; the two tags-replace forms above handle all cases.
  ;; (tags-query-replace-noerror "return (\\([^()\n]*\\));$" "return \\1;")
  )


;; TODO BUG: This fails if a file was not already read into a buffer,
;; perhaps because reading a file into a buffer changes the match-data.
;; So, before running this do: (tags-search "\\`\\(.\\|\n\\)")
;; PROBLEM: That tags-search is very painful for extremely large projects.
;; TODO: Handle adding curly braces when an if or for has a body consisting
;; of a single statement, on the same line as the if or for.
(defun add-curly-braces ()
  "Add curly braces around body of if/for statements whose body is a single
statement.  Does replacement in any file in a currently-visited tags table."
  (interactive)

  ;; Could also find if/for statements that don't end with an open curly or
  ;; a semicolon, which suggests that the body is a single statement that
  ;; is broken across lines.

  ;; avoid matching "else if"; should search for it separately.
  (tags-query-replace-noerror "} else \\([^\ni][^\n{]*;\\)$" "} else { \\1 }")

  ;; Find if/for statements that end with a close paren, which suggests the
  ;; body is on the next line.  Also else statements that end a line.
  (let ((tags-regex
         "^ *\\(?:}? else *\\)?\\(\\(if\\|for\\|while\\) (.*)\\|}? else\\( //.*\\)?\\)\\(.*;\\)?$"))
    (tags-search tags-regex)
    (if java-style-debug
        (message "match-data after tags-search: %s" (match-data)))
    (while t
      ;; The call to looking-back has an important side effect:  it sets (match-data).
      (if (not (looking-back tags-regex nil))
	  (error "This can't happen: not looking-back from %d: %s"
		 (point) (buffer-substring (max 0 (- (point) 45)) (point))))
      (if java-style-debug
	  (message "match-data at top of loop: %s" (match-data)))
      (if java-style-debug
	  (message "(point) at top of loop: %s" (point)))
      (let ((start-of-if (match-beginning 1)))
        ;; (message "Before and after point:\n %s\n %s"
        ;;          (buffer-substring (max (- (point) 45) (point-min))
        ;;                            (point))
        ;;          (buffer-substring (point)
        ;;                            (min (+ (point) 45) (point-max))))
        ;; Are tags-search and tags-loop-continue guaranteed to leave the
        ;; match-data set??  Do looking-at to re-set match-data.
        (beginning-of-line)
        ;; Match might not have started at beginning of line
        ;; (if (not (looking-at tags-regex))
        ;;          (error "This can't happen"))

        (if java-style-debug
	    (message "Looking at (2): %s"
		     (buffer-substring (point)
                                       (min (+ (point) 45) (point-max)))))
        ;; (message "1 %s" (match-data))
        (goto-char start-of-if)
        (if java-style-debug
	    (message "Looking at (3): %s"
		     (buffer-substring (point)
				       (min (+ (point) 45) (point-max)))))
        (let* ((line (buffer-substring (point) (save-excursion (end-of-line) (point))))
               (semicolon-terminated (equal ";" (substring line -1))))
          (if (and (= (how-many-in-string "(" line) (how-many-in-string ")" line))
                   (= 0 (how-many-in-string "{" line))
                   (= 0 (how-many-in-string "//" line))
                   (let ((leading-spaces (progn (string-match "^ *" line)
                                                (match-string 0 line))))
                     (if semicolon-terminated
                         (looking-at (concat leading-spaces "[^ \n].*\n+"
                                             leading-spaces "[^ ]"))
                       (and (looking-at (concat leading-spaces "[^ \n].*\n"
                                                leading-spaces "[ ]"))
                            (not (looking-at (concat leading-spaces "[^ \n].*\n"
                                                     leading-spaces "[ ]*\\(for\\|if\\|try\\)\\b")))))))
              ;; Parens are balanced on the if/for line, and either:
              ;;  * line ends with ";" and next line is equally indented, or
              ;;  * line does not end with ";" and next line is indented more.
              (progn
                (cond ((looking-at " *\\(if\\|for\\|while\\)")
                       (forward-sexp 2))
                      ((looking-at " *\\(}? else\\)")
                       ;; (message "2 %s" (match-data))
                       (goto-char (match-end 0)))
                      (t
                       (error "This can't happen.  start-of-if=%d (point)=%d.  Looking at: %s"
			      start-of-if
			      (point)
                              (buffer-substring (point)
                                                (min (+ (point) 45) (point-max))))))
                (insert " \{")
                (if semicolon-terminated
                    (newline-and-indent))
                (re-search-forward ";\\( *//.*\\)?$")
                (while (looking-back "^[^;\n]*//[^\n]*$" nil)
                  (re-search-forward ";\\( *//.*\\)?$"))
                (if (looking-at "\n *\\(else\\)")
                    (progn
                      ;; (message "3 %s" (match-data))
                      (goto-char (match-beginning 1))
                      (insert "} "))
                  (progn
                    (newline-and-indent)
                    (insert "}")
                    (c-indent-line-or-region))))
            (forward-line)))
        (if java-style-debug
	    (message "match-data before (fileloop-continue): %s" (match-data)))
        (fileloop-continue)
        (if java-style-debug
	    (message "match-data before (fileloop-continue): %s" (match-data)))
        )))

  ;; This may be subsumed by `examine-and-cleanup-curly-braces'.

  )


;; TODO: Split this into formatting (which is not relevant when using a formatter)
;; and adding braces.
;; Here are two searches to find missing braces:
;; (rg "if \\(.*\\) [^{=|!&][^{]*;$" "java" "~/research/types/checker-framework-fork-mernst-branch-return-braces/")
;; (rg "else [^{]*;$" "java" "~/research/types/checker-framework-fork-mernst-branch-return-braces/")

(defun examine-and-cleanup-curly-braces ()
  "Investigate Java code that does not use curly braces for compound statements.
Works over the currently-visited tags table."
  (interactive)

  ;; Clean up formatting of curly braces.
  ;; For example, don't put curly braces before if or else on their own line.
  ;; This might all be obviated by use of a formatter such as google-java-format.
  (tags-query-replace-noerror "^\\( *\\)}\n *else" "\\1} else")
  (tags-query-replace-noerror "\\([{;]\\) *}\n\\( *\\)else" "\\1\n\\2} else")
  (tags-query-replace-noerror "}\n *else" "} else")
  (tags-query-replace-noerror " else\n *{" " else {")
  (tags-query-replace-noerror "\\( *\\)\\(.*) \\|else \\)\\(continue;\\|return\\b[^;\n]*;\\)$" "\\1\\2{\n\\1  \\3\n\\1}")
  (tags-query-replace-noerror "\\(\\(else \\)?if (.*\\|else\\)\n *{" "\\1 {")

  (tags-query-replace-noerror
   (concat "^\\( *\\)"
	   (concat
	    "\\b\\(\\(?:if\\|for\\) "
	    "([^\n()]*\\(?:([^\n()]*)[^\n()]*\\)*) \\)"
	    )
	   "\\([^\n;{}]*;\\)")
   (concat "\\1\\2{\n\\1  \\3\n\\1}")
   )

  ;; I need to run these one at a time; they do not work when run within this method.

  (tags-search "^ *else [^i{/|&]")
  (tags-search "[^/] else { [^/\n]")


  (tags-search "[^/] else\\( *//.*\\)?\n")
  (tags-search "^ *\\(}? else *\\)?\\(\\(if\\|for\\) (.*)\\|}? else\\( //.*\\)?\\)\n *[^ \n&|/=!.]")

  (tags-search "^ *\\b\\(\\(else \\)?if\\|for\\) ([^\n()]*\\(([^\n()]*)[^\n()]*\\)?) [^\n;{}]*;")

  (tags-search "^ *\\b\\(\\(else \\)?if\\|for\\) (.*) .*[^{\n|&]$")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comments
;;;

(defun improve-java-comment-style ()
  "Add periods at end of sentences.  Requires examination of each match."
  (tags-query-replace-noerror "^\\( *// [A-Z].*[^.!?,;:]\\)$" "\\1.")
  (tags-query-replace-noerror "\\(^ *// [^\n]*[^.!?,;:\n]\\)\\(\n *[^/ \n]\\)" "\\1.\\2")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Javadoc
;;;

(defun improve-javadoc-style ()
  "Improve Javadoc style in Java files, in the current TAGS table."
  (interactive)

  ;; Code in Javadoc.
  ;; Need to run javadoc-fix-code-tags until it has no effect.
  (javadoc-fix-code-tags)
  (javadoc-fix-code-tags)
  (condition-case nil
      (improve-javadoc-code-style)
    (error nil))

  ;; Text in Javadoc.
  (improve-javadoc-description-style)
  (improve-javadoc-tag-style)
  (javadoc-whether-to-true-if)
  (improve-javadoc-initial-verb)
  (javadoc-add-summary))


;; TODO: More Javadoc fixup, for /** and */ not on their own line when they should be:
;; This is irrelevant if using a formatter such as google-java-format.
;; (tags-search "/\\*\\* [A-Z].*\n *\\*")
;; (tags-search "/\\*\\*\\(\n[ \t]*\\*.*\\)+.*\\*/")



;; Need to run this repeatedly until it has no effect.
;; After running, search for "<code>" and "<tt>" to clean up stragglers by hand.
(defun javadoc-fix-code-tags ()
  (interactive)
  (tags-query-replace-noerror (concat "^\\( *\\(?:/\\*\\)?\\*.*\\)"
                                      "<\\(?:code\\|tt\\)> *\\(?:\n *\\* *\\)?"
                                      "\\([^<>@&{}]+?\\)\\(?:\n *\\* *\\)?"
                                      " *</\\(?:code\\|tt\\)>")
                              "\\1{@code \\2}")
  ;; Wouldn't this be caught by the above?
  (tags-query-replace-noerror "<\\(?:code\\|tt\\)>\\(false\\|null\\|true\\)</\\(?:code\\|tt\\)>" "{@code \\1}")
  (tags-query-replace-noerror "<\\(?:code\\|tt\\)>\\([A-Za-z_0-9\\.()\\[\\]]+\\)</\\(?:code\\|tt\\)>" "{@code \\1}")
  (tags-query-replace-noerror "<\\(?:code\\|tt\\)>\\([^<>{}]+\\)</\\(?:code\\|tt\\)>" "{@code \\1}")
  )
;; Same thing, but permitting @ in the body:
;;  (tags-query-replace-noerror "^\\( *\\(?:/\\*\\)?\\*.*\\)<\\(?:code\\|tt\\)>\\(?:\n *\\* *\\)?\\([^<>&]+?\\)\\(?:\n *\\* *\\)?</\\(?:code\\|tt\\)>" "\\1{@code \\2}")
;; Also:
;;   (tags-query-replace-noerror "<code>\n" "{@code\n")
;;   (tags-query-replace-noerror "<code>" "{@code ")
;;   (tags-query-replace-noerror "</code>" "}")


(defun improve-javadoc-code-style ()
  "Improve style for inline code in Javadoc comments,
for files in the current TAGS table."
  (interactive)

  ;; TODO: These should not occur within <code>...</code>

  (tags-query-replace-noerror "&lt;--?&gt;" "&harr;")
  (tags-query-replace-noerror "&lt;--?" "&larr;")
  (tags-query-replace-noerror "--?&gt;" "&rarr;")
  (tags-query-replace-noerror "&lt;==?&gt;" "&hArr;")
  (tags-query-replace-noerror "&lt;==" "&hArr;")
  (tags-query-replace-noerror "==?&gt;" "&rArr;")

  (tags-query-replace-noerror "\\({@code[^}]?*\\)&lt;" "\\1<")
  (tags-query-replace-noerror "\\({@code[^}]?*\\)&gt;" "\\1>")

  ;; Too many false positives, though good to do in general.
  ;; (tags-query-replace-noerror "&lt;" "<")
  ;; (tags-query-replace-noerror "&gt;" ">")
  )


(defun downcase-previous-character ()
  "Downcase the character before point."
  (let* ((prev-char (char-before (point)))
         (replacement (downcase prev-char)))
    (if (not (equal prev-char replacement))
        (progn
          (delete-char -1)
          (insert (downcase prev-char))))))

;; The official Javadoc style guide
;; (https://www.oracle.com/technical-resources/articles/java/javadoc-tool.html)
;; states that:
;;  * The doc comment should start with a sentence that ends with a period
;;  * block tags such as @param and @throws that consist of a single phrase should not end with a period.
;;  * block tags start with a lowercase letter unless they are a complete sentence.

;; Be sure to check the changes; occasionally, the first word of a Javadoc
;; comment is a proper noun and should not be downcased, or the 
(defun improve-javadoc-tag-style ()
  "Improve style for Javadoc @param and @return,
for files in the current TAGS tables."
  (interactive)

  ;; Remove trailing period.  That is, end the phrase with a period
  ;; only if more text follows it.
  (let ((regexp (concat "\\("
			"\\* @\\(?:\\(?:param\\|throws\\)[ \t\n*]+[A-Za-z0-9_]+\\|return\\) +"
			"[^@./]*\\(?:\\(?:{@\\(?:link\\|code\\) [^}]*}\\|\\.[^ \n]\\)[^@./]*\\)*"
			"\\)"
			"\\."
			"\\(\n *\\*/\\|\n *\\* *@\\)"))
	(replacement "\\1\\2"))
    ;; Do it twice because matches may overlap.
    (condition-case nil
	(tags-replace regexp replacement)
      (error nil))
    (condition-case nil
	(tags-replace regexp replacement)
      (error nil))
    )
  
  (condition-case nil
      (tags-replace "\\(@\\(?:param[ \t\n*]+[A-Za-z0-9_]+\\|return\\)\\) +- +" "\\1 ")
    (error nil))

  ;; PROBLEM: Execution does not get to here.
  (message "Execution got to here.")

  ;; Start descriptive text with lowercase letter.
  (condition-case nil
      (let ((tags-case-fold-search nil))
        ;; Emacs can convert case when doing {query-}replace-regexp, but it doesn't
        ;; seem to work with tags-query-replace, so call downcase-previous-character.
        ;; We only do so if the capital letter is at the beginning of a word
        ;; whose other characters are lowercase.
        (tags-search "\\(?:@\\(?:param[ \t\n*]+<?[A-Za-z0-9_]+>?\\|return\\)[ \t\n*]+\\(?:\n +\* +\\)?\\)\\([A-Z]\\)[a-z]*\\b")
        (goto-char (match-end 1))
	(if (not (save-excursion (backward-char 1) (looking-at "Java ")))
	    (downcase-previous-character))
        (while t
          (fileloop-continue)
          (goto-char (match-end 1))
	  (if (not (save-excursion (backward-char 1) (looking-at "Java ")))
	      (downcase-previous-character))))
    (user-error nil))

  ;; To detect incorrect end-of-clause punctuation (always a period,
  ;; never "!" or "?") for @param, @return, @throws, @exception:
  ;; (Run until it finds no more issues; would twice be enough?)
  (let ((regexp "\\(^ *\\* @\\(?:param\\|return\\|throws\\|exception\\)[^@./]*\\)\\.\\([ \n]*\\([* \n]* @\\|[* \n]*\\*/\\)\\)")
	(replacement "\\1\\2"))
    (condition-case nil
	(tags-replace regexp replacement)
      (error nil))
    (condition-case nil
	(tags-replace regexp replacement)
      (error nil))
    (condition-case nil
	(tags-replace regexp replacement)
      (error nil))
    )

  (condition-case nil
      (tags-replace " \\*\\*/" " */")
    (error nil))

  ;; Missing period at the end of the main part of the Javadoc:
  (condition-case nil
      (tags-replace "\\(/\\*\\*[^@./]*\\(?:\\(?:{@\\(?:link\\|code\\) [^}]*}\\|[.!?][)]? \\)[^@./]*\\)+[^.!? \n]\\)\\([ \n]*\\*/\\)"
		    "\\1.\\2")
    (error nil))

  ;; Non-capitalized first letter in the main part of the Javadoc:
  (let ((case-fold-search nil))
    nil ;; placeholder
    ;; TODO: implement this
    ;; (tags-replace-capitalize-2 "\\(/\\*\\*[[:space:]]*\\)\\([a-z]\\)"))
    )

  )

(defun improve-javadoc-description-style ()
  "Improve style for Javadoc descriptions, for files in the current TAGS tables.
The description is everything but the block tags (such as @param and @return)."
  (interactive)

  ;; End the first phrase with a period.
  (tags-query-replace-noerror
   (concat "\\(^ */\\*\\*\\(?: *\n *\\*\\)? *[A-Z][^.\n]*\\(?: *\n *\\* *[^.\n]*\\)?\\(?:[a-z0-9]\\)\\)"
	   "\\( *\\(\\*/$\\|\n *\\* *\n\\)\\)")
   "\\1.\\2")
  )

(defun javadoc-whether-to-true-if ()
  "Replace \"whether\" by \"true if\" in Javadoc."
  (tags-query-replace-noerror "\\(@return\\|@param [a-zA-Z0-9_]+\\)\\(?: indicates\\)? [wW]hether to \\(.*\\) or not$" "\\1 if true, \\2")
  (tags-query-replace-noerror "\\(@return\\|@param [a-zA-Z0-9_]+\\)\\(?: indicates\\)? [wW]hether\\(?: or not\\)? to " "\\1 if true, ")
  (tags-query-replace-noerror "\\(@return\\|@param [a-zA-Z0-9_]+\\)\\(?: indicates\\)? [wW]hether \\(.*\\) or not$" "\\1 true if \\2")
  (tags-query-replace-noerror "\\(@return\\|@param [a-zA-Z0-9_]+\\)\\(?: indicates\\)? [wW]hether\\(?: or not\\)? " "\\1 true if ")
  (tags-query-replace-noerror "\\* \\(?:Return\\|Check\\|Determine\\|Get\\|Indicate\\|Test\\)s?\\(?: to see\\)? whether \\(.*\\) or not\\.$" "* Returns true if \\1.")
  (tags-query-replace-noerror "\\* \\(?:Return\\|Check\\|Determine\\|Get\\|Indicate\\|Test\\)s?\\(?: to see\\)? whether\\(?: or not\\)? " "* Returns true if ")
  (tags-query-replace-noerror "\\* Whether\\( or not\\)? to " "* If true, ")
  (tags-query-replace-noerror "\\* Whether or not " "* True if ")
  (tags-query-replace-noerror "\\* Whether \\(.*\\) or not\\.$" "* True if \\1.")
  (tags-query-replace-noerror "\\* Whether to \\(.*\\) or not\\.$" "* If true, \\1.")
  (tags-query-replace-noerror "\\* Whether " "* True if ")
  )


(defun improve-javadoc-initial-verb ()
  "Make the initial verb in a Javadoc comment be present tense, not a command."
  (interactive)

  ;; Remove trailing period.  That is, end the phrase with a period
  ;; only if more text follows it.
  (let ((regexp "\\(/\\*\\*\n *\\* \\)\\(Get\\) ")
	(replacement "\\1Returns "))
    (condition-case nil
	(tags-replace regexp replacement)
      (error nil)))
  (let ((regexp "\\(/\\*\\*\n *\\* \\)\\(Return\\|Increment\\) ")
	(replacement "\\1\\2s "))
    (condition-case nil
	(tags-replace regexp replacement)
      (error nil)))
  )





(defun javadoc-add-summary ()
  (interactive)
  (tags-query-replace-noerror (concat "/\\*\\*\\(?:\n *\\*\\)? \\(\\(?:@param\\(?:[^@\n]+\n\\)* *\\* \\)*\\)"
			              "@return \\(.*\\(?:\n *\\* [^@\n].+\\)\\{0,1\\}[^\n. ]\\)\\.?"
			              "\\(\n? *\\*/\\|\n *\\* @\\)")
		              "/** Returns \\2.\n\\1@return \\2 \\3")
  (tags-query-replace-noerror "/\\*\\* @see \\(.*\\) \\*/" "/**\n   * See {@link \\1}.\n   * @see \\1\n   */")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java generics
;;;



;; Finds nothing actionable in Daikon as of 1/2/2006.
(defun generify-java ()
  "Find Java code that should be made generic."
  (interactive)
  (let ((case-fold-search nil))
    (tags-search "^[^\n*/]*[^_.\"]\\b\\(Iterator\\|Collection\\|Set\\|List\\|Enumeration\\) [^=\"+:]"))
  ;; Look for casts after calling "next() or "getNext()":
  (tags-search "(.+) *[a-zA-Z0-9_]+\\.\\(getNext\\|next\\|get\\) ?("))


;; To find more patterns to add:
;; Search for:
;; <.*> .* = new .*<.+>
;; search for:
;; new [A-Za-s0-9_]+<.+>
(defun use-java-diamond-operator ()
  "Change code to use Java's diamond operator."
  (interactive)
  (tags-query-replace-noerror "\\(\\(?:List\\|Queue\\)<\\([^>]*\\)> [A-Za-z0-9_]+ = new \\(Array\\|Linked\\)?List\\)<\\2>" "\\1<>")
  (tags-query-replace-noerror "\\(\\(?:Hash\\)?Map<\\(.*\\)> [A-Za-z0-9_]+ = new \\(\\(Linked\\)?Hash\\|Tree\\)Map\\)<\\2>" "\\1<>")
  (tags-query-replace-noerror "\\(Set<\\([^>]*\\)> [A-Za-z0-9_]+ = new \\(\\(Linked\\)?Hash\\|Tree\\)Set\\)<\\2>" "\\1<>")
  (tags-query-replace-noerror "\\(Vector<\\([^>]*\\)> [A-Za-z0-9_]+ = new Vector\\)<\\2>" "\\1<>")
  (tags-query-replace-noerror "\\b\\(\\([A-Za-z0-9_, ]+\\)\\(<.*>\\) [A-Za-z0-9_]+ = new \\2\\)\\3" "\\1<>")
  (tags-query-replace-noerror "\\b\\(new [A-Za-s0-9_, ]+\\)<.+>(" "\\1<>(")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For loops
;;;


(defun fix-java-for-loops ()
  "Convert uses of iterators into new-style for loops."
  (interactive)
  ;; Body copied from tags-query-replace.
  (setq tags-loop-scan
	`(find-candidate-old-for-loop)
	tags-loop-operate `(progn
			     (message "performing at %s" (point))
			     (perform-replace old-for-loop-regexp
					      for-loop-replacement
					      t t nil)
			     t))
  (fileloop-continue t))

(defvar old-for-loop-regexp
  "\\bfor (Iterator<\\(.*\\)> +\\(\\w+\\) *= *\\(.*\\)\\.iterator() *; *\n?.*; *)[ \n]*\\(\\){\n *\\(\\1 .*[^ ]\\) *= *\\2\\.next();")
(defvar for-loop-replacement
  "for (\\5 : \\3) {")

;; These are for debugging.
;; (re-search-forward old-for-loop-regexp)
(defvar old-for-loop-regexp-part1
  "\\bfor (Iterator<\\(.*\\)> +\\(\\w+\\) = \\(.*\\)\\.iterator() *;")
;; (re-search-forward old-for-loop-regexp-part1)
(defvar old-for-loop-regexp-part2
  "\\bfor (Iterator<\\(.*\\)> +\\(\\w+\\) = \\(.*\\)\\.iterator() *; *\n?.*; *)[ \n]*\\(\\){")
;; (re-search-forward old-for-loop-regexp-part2)


;; Need to also look for (but it doesn't happen as often):
;;             Iterator<Class> class_it = classes.iterator();
;;             while (class_it.hasNext()) {
;;               Class c = class_it.next();



(defun find-candidate-old-for-loop ()
  "Return t if candidate found."
  (interactive)
  (message "considering %s" (buffer-file-name))
  (catch 'fcofl
    (while (re-search-forward old-for-loop-regexp nil t)
      (let* ((iterator-var (match-string 2))
	     (match-begin (match-beginning 0))
	     (match-end (match-end 0))
	     (body-begin (match-beginning 4))
	     (body-end (save-excursion
			 (goto-char body-begin)
			 (forward-sexp 1)
			 (point))))
	(goto-char match-end)
	(if (not (re-search-forward (concat "\\b" iterator-var "\\b") body-end t))
	    (progn
	      (message "no iterator-var %s; going back to %s" iterator-var match-begin)
	      (goto-char match-begin)
	      (if (not (re-search-forward old-for-loop-regexp match-end t))
		  (error "didn't find match"))
	      (cl-assert (= match-begin (match-beginning 0)))
	      (message "going back to %s for %s" match-begin iterator-var)
	      (goto-char match-begin)
	      (throw 'fcofl t)))))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checker Framework support
;;;


;;;
;;; Making Java "stub libraries" or "skeleton files" for the Checker
;;; Framework (JSR 308).  Run these after reading the comments and adding
;;; annotations such as "@Nullable" where appropriate.
;;;

(defun delete-java-comments ()
  "Delete all Java comments.  Use with caution."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "/*" nil t)
    (let ((beg (- (point) 2))
          (end (progn (search-forward "*/") (point))))
      (delete-region beg end))))

;; The throw statement is necessary for skeleton files.  For stub
;; libraries, replacing the body by a semicolon is more appropriate.
(defun make-java-bodies-empty ()
  "Replace the body of all Java methods by a body that throws an exception."
  (interactive)
  ;; (goto-char (point-min))
  (while (re-search-forward "^[ \t].*{\n[ \t]*" nil t)
    (let ((block-start (point))
	  (block-end (save-excursion (goto-char (match-beginning 0))
				     (forward-sexp)
				     (backward-char)
				     (skip-chars-backward " \t")
				     (if (looking-back "\n" (- (point) 1))
					 (backward-char))
				     (point))))
      (if (save-excursion
            (goto-char block-start)
            (beginning-of-line)
            (looking-at ".*\\bclass\\b"))
	  ;; Don't empty class bodies
	  (forward-line 1)
	(progn
	  (delete-region block-start block-end)
	  (goto-char block-start)
	  (insert "throw new RuntimeException(\"skeleton method\");"))))))


;; This is for creating test cases for the Checker Framework.
;; Call on a .out filename, to insert "// ::" comments in a .java file.
(defun integrate-out ()
  (interactive)
  (let* ((out-file (buffer-substring (point) (save-excursion (end-of-line) (point))))
	 (java-file (concat (substring out-file 0 (- (length out-file) 4)) ".java")))
    (integrate-out-from-files java-file out-file)))

(defun integrate-out-from-files (java-file out-file)
  (let ((java-buffer (find-file-noselect java-file)))
    (with-current-buffer (find-file-noselect out-file)
      (goto-char (point-max))
      (while (re-search-backward (concat "^\\(?:" java-file "\\)?:\\([0-9]+\\): \\(.*\\)$") nil t)
	(let ((line (string-to-number (match-string 1)))
	      (msg (match-string 2)))
	  (with-current-buffer java-buffer
	    (goto-char (point-min)) (forward-line (1- line)) ; instead of: (goto-line line)
	    (open-line 1)
	    (c-indent-line-or-region)
	    (insert "// :: " msg)))))))
;; (integrate-out



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving from jsr308_imports environment variable to explicit import statements
;;;

(defun add-imports (regex package)
  (tags-search regex)
  (while t
    (if (search-backward "\nimport" nil t)
	(progn
	  (search-forward "\n\n")
	  (insert "/*>>>\nimport " package ";\n*/\n\n")))
    (goto-char (point-max))
    (mde-tags-loop-continue)))

(defun add-all-imports-to-directory (directory)

  (visit-tags-table (substitute-in-file-name directory))

  ;; These are in alphabetical order by package
  (add-imports "/\*@\\(Format\\|ReturnsFormat\\)"
	       "org.checkerframework.checker.formatter.qual.*")
  (add-imports "/\*@\\(Initialized\\|NotOnlyInitialized\\|UnderInitialization\\|UnknownInitialization\\)"
	       "org.checkerframework.checker.initialization.qual.*")
  (add-imports "/\*@\\(Interned\\|PolyInterned\\|UsesQbjectEquals\\)"
	       "org.checkerframework.checker.interning.qual.*")
  (add-imports "/\*@\\(AssertNonNull\\|EnsuresNonNull\\|MonotonicNonNull\\|NonNull\\|NonRaw\\|Nullable\\|PolyNull\\|Raw\\|RequiresNonNull\\)"
	       "org.checkerframework.checker.nullness.qual.*")
  (add-imports "/\*@\\(Regex\\|PartialRegex\\|PolyRegex\\)"
	       "org.checkerframework.checker.regex.qual.*")
  (add-imports "/\*@\\(BinaryName\\|ClassGetName\\|FieldDescriptor\\|FullyQualifiedName\\|MethodDescriptor\\|PolySignature\\|SourceName\\)"
	       "org.checkerframework.checker.signature.qual.*")
  (add-imports "/\*@\\(Deterministic\\|LockingFree\\|Pure\\|SideEffectFree\\|TerminatesExecution\\)"
	       "org.checkerframework.dataflow.qual.*")
  (add-imports "/\*@\\(Dependent\\|Unused\\)"
	       "org.checkerframework.framework.qual.*")
  ;; Project-specific qualifiers
  (add-imports "/\*@\\(NonPrototype\\|Prototype\\|VIndexUnqualified\\|ValueIndex\\|VarIndex\\)"
	       "typequals.*")

  (tags-replace "\\(import org.checkerframework..*\\)\n\\*/\n\n/\\*>>>\n\\(import org.checkerframework..*\\)" "\\1\n\\2")
  (tags-replace "\\(import org.checkerframework..*\\)\n\\*/\n\n/\\*>>>\n\\(import org.checkerframework..*\\)" "\\1\n\\2")
  )
;; (add-all-imports-to-directory "$d/java")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving @Nullable annotations from declaration position to type position
;;;

;; If @Nullable annotations have been written in declaration position,
;; this Emacs code moves them to type annotation position.
;; I have not tried it within the function, enly evaluating each form at the top level.

(defun move-nullable-to-type-annotation-position ()
  (interactive)

  (defvar modifers-plus-space-regex)
  (setq modifers-plus-space-regex "\\(\\(?:abstract \\|final \\|private \\|protected \\|public \\|static \\|transient \\|volatile \\|<[^>]*> \\)+\\)")

  ;; This handles @Nullable on its own line
  (tags-query-replace
   (concat "\\(
   *\\)@Nullable\\(\\(?:
   *@\\(?:Override\\|CanIgnoreReturnValue\\|SuppressWarnings([^)]*)\\)\\)*\\)\\(
   *\\)" modifers-plus-space-regex)
   "\\2\\3\\4@Nullable ")


  ;; This handles @Nullable on the same line as other code
  (tags-query-replace
   (concat "@Nullable " modifers-plus-space-regex)
   "\\1@Nullable ")

  ;; Swap order of two declarations, when the @Nullable will stay on its own line
  ;; due to limitations of google-java-format
  (tags-query-replace
   "\\(
   *@Nullable\\)\\(
   *@Override\\)"
   "\\2\\1")

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO
;;;

;; Utilize maplist.
;; C-u M-x rg RET for \(.* : .*\n(.*\n)?.*\.add\( RET  (add --multiline)


;; Utilize getOrDefault and computeIfAbsent.
;; Search for:
;;  * \.get.*\n.*== null
;;  * \.contains\(
;; 
;; Here are searches for the two:
;; C-u M-x rg RET \.get\(.*\n.*== null RET  (add --multiline)
;; (rg "\\.get\\(.*\\n.*== null" "java" default-directory)
;; (tags-search "\\b\\([A-Za-z]+\\)\\.contains(.*\n.*\\1")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JUnit style
;;;

(defun junit-assert-style ()
  "Convert uses of JUnit's assertTrue(x.equals(y)) to assertEquals(x, y)."
  (interactive)
  (tags-query-replace "^\\([ \t]*\\)assertTrue(\\([^;]*\\)[ \n\t]*\\.equals(\\([^;]*\\)));$"
		      "\\1assertEquals(\\3, \\2);")
  )

;;; To change Java assert statements into JUnit method calls (desirable in tests):

;;; Equality
;; (tags-query-replace "assert \\(.*\\).equals(\\(.*\\));" "assertEquals(\\2, \\1);")
;; (tags-query-replace "assert \\(.*\\).equals(\\(.*\\)) : \\1;" "assertEquals(\\2, \\1);")
;; (tags-query-replace "assert \\([^;&|]+\\) == true;" "assertTrue(\\1);")
;; (tags-query-replace "assert \\([^;&|]+\\) == false;" "assertFalse(\\1);")
;; (tags-query-replace "assert \\([^;&|]+\\) == null;" "assertNull(\\1);")
;; (tags-query-replace "assert \\([^;&|]+\\) != null;" "assertNotNull(\\1);")
;; ;; Change ==:  for primitive types, use assertEquals; for reference types, use assertSame.
;; (tags-query-replace "assert \\(.+\\) == \\([-+0-9.]+\\);" "assertEquals(\\2, \\1);")
;; (tags-query-replace "assert \\([-+0-9.]+\\) == \\(.+\\);" "assertEquals(\\1, \\2);")
;; (tags-query-replace "assert \\(.*\\) == \\(.*\\);" "assertEquals(\\2, \\1);")
;; (tags-query-replace "assert \\(.*\\) == \\(.*\\);" "assertSame(\\2, \\1);")
;; (tags-query-replace "assertTrue(\\(.*\\) == \\(.*\\));" "assertEquals(\\2, \\1);")
;; (tags-query-replace "assertTrue(\\(.*\\) == \\(.*\\));" "assertSame(\\2, \\1);")

;; (tags-query-replace "assert \\(.*[<>].*[^\"]\\);" "assertTrue(\\1);")

;; (tags-query-replace "assert !\\(.*\\);" "assertFalse(\\1);")

;;; Catch-all
;; (tags-query-replace "assert \\([^:;]*\\);" "assertTrue(\\1);")

;; (tags-query-replace "assertTrue(\\(?:\"[^,]*\", \\)\\([^\"].*\\)?.equals(\\(.*\\)));" "assertEquals(\\2, \\1);")
;; (tags-query-replace "assertTrue(\\([^\"].*\\).equals(\\(.*\\)));" "assertEquals(\\2, \\1);")

;; (tags-query-replace "\\(assertEquals(\\)\\([ \n]*\\)\\(\"[^\"]*\",[ \n]*\\)" "\\1\\2")
;; (tags-query-replace "\\(assertTrue(\\)\\([ \n]*\\)\\(\"[^\"]*\",[ \n]*\\)" "\\1\\2")
;; (tags-query-replace "\\(assertFalse(\\)\\([ \n]*\\)\\(\"[^\"]*\",[ \n]*\\)" "\\1\\2")
;; (tags-query-replace "assertTrue(!" "assertFalse(")


;; (tags-query-replace "assert \\([^=:\n]+\\) == \\([^=:\n]+\\);" "assertEquals(\\2, \\1);")

;; (tags-query-replace "assert \\(.+\\) != null;" "assertNotNull(\\1);")

;; (tags-query-replace "assert !\\([^=\n]+\\);" "assertFalse(\\1);")



;;; Here are Emacs commands for replacing JUnit assertions with Hamcrest
;;; versions that produce more readable output.  The Hamcrest code can be
;;; harder to read, but the error messages can be clearer.  Depending on
;;; the use case, this may be a worthwhile tradeoff.

;; (tags-query-replace "assertFalse(\\(\".*\",\\)
;; \\([ 	]*\\) \\(.*\\)\\.equals(\\(.*\\)));
;; " "assertThat(\\1
;; \\2\\3, not(\\4));
;; ")
;; (tags-query-replace "assertTrue(!\\(.*\\)\\.equals(\\(.*\\)));" "assertThat(\\1, not(\\2));")
;; (tags-query-replace "assertFalse(\\([^\"].*\\)\\.equals(\\(.*\\)));" "assertThat(\\1, not(\\2));")
;; (tags-query-replace "assertTrue(\\([^\"].*\\)\\.equals(\\(.*\\)));" "assertEquals(\\1, \\2);")
;; (tags-query-replace "assertTrue(\\([^\"].*\\) == \\(.*\\));" "assertEquals(\\1, \\2);")
;; (tags-query-replace "assertTrue(\\(\".*\"\\), !\\(.*\\)\\.equals(\\(.*\\)));" "assertThat(\\1, \\2, not(\\3));")
;; (tags-query-replace "assertTrue\\( ?\\)(\\(\".*\"\\),\\( \\|
;;  *\\)!\\(.*\\)\\.equals ?(\\(.*\\)));" "assertThat\\1(\\2,\\3\\4, not(\\5));")
;; (tags-query-replace "assertTrue\\( ?\\)(\\(\".*\"\\),\\( \\|
;;  *\\)\\(.*\\)\\.equals ?(\\(.*\\)));" "assertEquals\\1(\\2,\\3\\4, \\5);")
;; (tags-query-replace "assertTrue\\( ?\\)(\\(\".*\"\\),\\( \\|
;;  *\\)\\(.*\\) == \\(.*\\));" "assertEquals\\1(\\2,\\3\\4, \\5);")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formatting -- not needed when using a formatter such as google-java-format
;;;

(defun tags-cleanup-java-whitespace ()
  "Make whitespace in Java code uniform (and good style).
It is irrelevant if using a code formatter such as google-java-format."
  (interactive)
  ;; avoid matching urls (http://...) and strings ("//")
  (tags-query-replace-noerror "\\(\\(?:\\`\\|[^:\"/]\\)//\\)\\([^ /\n\t]\\)" "\\1 \\2")
  ;; omit "switch" from this regexp
  (tags-query-replace-noerror "\\([^_]\\)\\b\\(catch\\|for\\|if\\|while\\|return\\)(" "\\1\\2 (")
  (tags-query-replace-noerror "){" ") {")
  (tags-query-replace-noerror "\\(}\\)\\(catch\\|else\\|finally\\)\\b" "\\1 \\2")
  (tags-query-replace-noerror "\\()\\)\\({[^0-9]\\)" "\\1 \\2")
  (tags-query-replace-noerror "\\b\\(else\\|finally\\|try\\)\\({\\)" "\\1 \\2")
  ;; TODO: This should not be run on lines that contain "{@code".  For now, don't replace in Javadoc.
  ;; TODO: This should not be run on lines that contain "\code{" (ie, not on LaTeX files).
  (tags-query-replace-noerror "^\\([ \t]*[^* \t].*\\);}" "\\1; }")
  ;;; These are subsumed by the above.
  ;; (tags-query-replace-noerror "}else{" "} else {")
  ;; (tags-query-replace-noerror "}else" "} else")
  ;; (tags-query-replace-noerror "else{" "else {")

  (tags-query-replace-noerror "\\b\\(for (.*;\\)\\([^[:space:]\n].*;\\)\\([^[:space:]\n]\\)" "\\1 \\2 \\3")
  (tags-query-replace-noerror "\\b\\(for (.*[^;];\\)\\([^ \t\n;]\\)" "\\1 \\2")
  (tags-query-replace-noerror "\\b\\(throws.*[a-z]\\){" "\\1 {")
  )

;; Not needed any more -- just run google-java-format.
(defun declaration-annotations-to-their-own-line ()
  "Move commented declaration annotations to their own line,
for files in the current TAGS tables.
This is not necessary when using script `run-google-java-format'."
  (interactive)
  (tags-query-replace-noerror "^\\( *\\)/\\*\\(@SideEffectFree\\|@Pure\\|@Deterministic\\)\\*/ \\(public\\|private\\|protected\\|boolean\\|int\\|static\\)" "\\1/*\\2*/\n\\1\\3")
  )

