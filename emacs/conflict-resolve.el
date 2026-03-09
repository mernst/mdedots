        ;;; -*- lexical-binding: t -*-

;; This file contains functions that resolve merge conflicts.
;; Also see file diff-clean.el, which is for diffs (not conflicts).

;; Typical workflow for resolving conflicts:
;; Run at the top level: etags $(rg --files-with-matches '<<<<<<')
;; Visit that tags table.
;; (require 'conflict-resolve)
;; ;; TODO: What is the purpose of this?
;; ;; (read-conflict-files-from-tags-table)
;; Now run as many of the following as desired.
;; (conflict-resolve)
;; (tags-conflict-resolve)
;; (conflict-resolve-annotation-lines)
;; (tags-conflict-resolve-annotation-lines)
;; (resolve-annotatedfor-conflicts)
;; (move-cf-imports-to-beginning)
;; (resolve-import-conflicts)
;; (resolve-method-signature)
;; (conflict-resolve-empty)
;; (resolve-equals-method-conflict)


;; Most useful for pulling remote into annotated code, such as the
;; Checker Framework annotated JDK:
;; (tags-conflict-resolve-annotation-lines-in-head)
;; (tags-conflict-resolve-annotation-lines-in-other ()


;; Run vc-resolve-conflicts for each file that this matches.
;; (tags-search less-than-hunk-start)


(eval-when-compile
  (require 'etags)
  (require 'util-mde))

(autoload 'replace-all-occurrrences-iteratively "util-mde")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defun grouped (regex)
  "Return a regex that places the given regex in a capturing group."
  (concat "\\(" regex "\\)"))

(defconst less-than-hunk-start-re
  "^<<<<<<<\\(?: HEAD\\)\n")
(defconst less-than-hunk-start-grouped
  (grouped less-than-hunk-start-re))
(defconst vertical-bar-separator-re
  "|||||||\\(?: [0-9a-f]\\{7,11\\}\\| merged common ancestors\\)?\n")
(defconst equal-sign-separator-re
  "=======\n")
(defconst greater-than-hunk-end-re
  ">>>>>>>\\(?: [0-9a-f]\\{40\\}\\)?\n")

(defun lines-without-at-start-re (char)
  "Matches a sequence of lines that do not start with the given character."
  (concat "\\(:?\n\\|[^" char "].*\n\\)*"))

(defconst left-lines-re
  (lines-without-at-start-re "|"))
(defconst left-lines-grouped-re
  (grouped left-lines-re))
(defconst right-lines-re
  (lines-without-at-start-re ">"))
(defconst right-lines-grouped-re
  (grouped right-lines-re))
(defconst base-lines-re
  (lines-without-at-start-re "="))
(defconst base-lines-grouped-re
  (grouped base-lines-re))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in @AnnotatedFor annotations
;;;

(defun resolve-annotatedfor-conflicts ()
  "Resolve conflicts that invove only @AnnotatedFor lines.
A caveat:
The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; (read-conflict-files-from-tags-table)
  (tags-search
   (concat "^" less-than-hunk-start-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?"
           vertical-bar-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?"
           equal-sign-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?"
           greater-than-hunk-end-re)
   )
  (while t
    ;; (message "#1 %s" (match-string 1))
    ;; (message "#2 %s" (match-string 1))
    ;; (message "#3 %s" (match-string 3))
    (replace-match (merged-annotated-for (remove-text-properties-string (match-string 1)) (remove-text-properties-string (match-string 3))))
    (fileloop-continue))
  ;; TODO: does not get run because previous loop throws an exception
  )

(defun merged-annotated-for (annotatedfor-arg-1 annotatedfor-arg-2)
  "Merge two @AnnotatedFor annotation arguments into one @AnnotatedFor annotation."
  (save-match-data
    (let* ((args1 (parse-annotatedfor-argument annotatedfor-arg-1))
	   (args2 (parse-annotatedfor-argument annotatedfor-arg-2))
	   (args (sort (delete-dups (append args1 args2)))))
      (concat "@AnnotatedFor({\""
	      (mapconcat #'identity args "\", \"")
	      "\"})\n"))))
;; (merged-annotated-for "{\"signature\", \"nullness\" ,\"interning\"}" "{\"propkey\", \"signature\"")

(defun parse-annotatedfor-argument (arg)
  "Given an argument to @AnnotatedFor, return a list of the string arguments."
  (split-string arg "\" *, *\"" 'omit-separators "[ {}\"]*"))
;; (cl-assert (equal '("signature") (parse-annotatedfor-argument "{\"signature\"}")))
;; (cl-assert (equal '("signature") (parse-annotatedfor-argument " \"signature\"  "))
;; (cl-assert (equal '("signature" "nullness" "interning") (parse-annotatedfor-argument "{\"signature\", \"nullness\" ,\"interning\"}")


(defun resolve-annotatedfor-usesobjectequals-conflicts ()
  "Resolve conflicts that invove only @AnnotatedFor and @UsesObjectEquals.
A caveat:
The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; (read-conflict-files-from-tags-table)
  (tags-search
   (concat less-than-hunk-start-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\(.*\\)class \\(.*\\)\n"
           vertical-bar-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\2class \\3\n"
           equal-sign-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\(\\2\\(?:@UsesObjectEquals \\|@Interned \\)*class \\3\n\\)"
           greater-than-hunk-end-re
           ))
  (while t
    ;; (message "#1 %s" (match-string 1))
    ;; (message "#2 %s" (match-string 1))
    ;; (message "#3 %s" (match-string 3))
    (message "#6 %s" (match-string 6))
    (replace-match
     (concat (merged-annotated-for (remove-text-properties-string (match-string 1)) (remove-text-properties-string (match-string 5)))
	     (match-string 6)))
    (fileloop-continue))
  ;; TODO: does not get run because previous loop throws an exception
  )




;; (tags-query-replace (concat less-than-hunk-start-re
;; "@AnnotatedFor({?\"interning\"}?)
;; ||||||| merged common ancestors
;; =======
;; @AnnotatedFor({\"lock\"})
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; ")
;; "@AnnotatedFor({\"interning\", \"lock\"})
;; "))



(defun resolve-one-annotatedfor-conflict (annotatedfor-arg-1 annotatedfor-arg-2 annotatedfor-arg-combined)
  (tags-query-replace
   (format (concat less-than-hunk-start-re
                   "@AnnotatedFor({?%s}?)\n"
                   "public\\(.*\\) @UsesObjectEquals class \\(.*\\)\n"
                   vertical-bar-separator-re
                   "public\\1 class \\2\n"
                   equal-sign-separator-re
                   "@AnnotatedFor({?%s}?)
public\\1 class \\2
>>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
") annotatedfor-arg-1 annotatedfor-arg-2)
   (format
    "@AnnotatedFor({%s})
public\\1 @UsesObjectEquals class \\2
" annotatedfor-arg-combined)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in imports
;;;


;; This is appropriate only for the Checker Framework annotated JDK.
(defun move-cf-imports-to-beginning ()
  ;; Move Checker Framework imports before the hunk.
  (tags-query-replace
   (concat less-than-hunk-start-grouped
           "\\(\\(import org.checkerframework..*;\n\\)+\n\\)")
   "\\2\\1")
  )


;; This is superseded by merge-java-imports-driver.sh .
(defun tags-conflict-resolve-import-conflicts ()
  "Resolve conflicts that invove only import lines, by accepting all the lines.
Two caveats:
1. You may have to adjust whitespace at the beginning and end manually.
2. The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; This is necessary because the mode-hook might blow away the match-data,
  ;; causing the value of (e.g.) `(match-string 1)` to be incorrect.
  ;; Pre-reading the conflict files ensures that their mode-hook has already run
  ;; before this function runs.
  (read-conflict-files-from-tags-table)
  (tags-search
   (concat less-than-hunk-start-re
           "\\(\\(?:\\(?:import .*;\\)?\n\\)*\\)"
           vertical-bar-separator-re
           "\\(\\(?:\\(?:import .*;\\)?\n\\)*\\)"
           equal-sign-separator-re
           "\\(\\(?:\\(?:import .*;\\)?\n\\)*\\)"
           greater-than-hunk-end-re)
   )
  (while t
    ;; (message "#1 %s" (match-string 1))
    ;; (message "#3 %s" (match-string 3))
    (replace-match (sorted-non-duplicate-lines (match-string 1) (match-string 3)))
    (fileloop-continue))
  ;; TODO: does not get run because previous loop throws an exception
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in annotations
;;;

(defvar annotation-line-regex nil
  "A regular expression that matches a declaration annotation (which should be
written on its own line).  The regexp is not anchored by \"^\" or \"$\".")
(setq annotation-line-regex
      (concat
       " *@\\(?:"
       (string-join
        '(
          ;; "SuppressWarnings(.*)" intentionally omitted; it should be be the
          ;; last annotation textually and should be resolved by hand.
          "CallerSensitive"
          "Deprecated.*"
          "ForceInline"
          "Override"

          ;; CF annotations
          "AnnotatedFor(.*)"
          "CFComment(.*)"
          "Covariant({[0-9]})"
          "CreatesMustCallFor"
          "Deterministic"
          "Ensures.*"
          "EqualsMethod"
          "ForName"
          "FormatMethod"
          "GetClass"
          "GetConstructor"
          "GetMethod"
          "I18nMakeFormat"
          "InheritableMustCall(.*)"
          "Invoke"
          "MayReleaseLocks"
          "MustCall(.*)"
          "NewInstance"
          "NotOwning"
          "OptionalCreator"
          "OptionalEliminator"
          "OptionalPropagator"
          "PolyUIEffect"
          "PolyUIType"
          "Pure"
          "ReleasesNoLocks"
          "Requires.*"
          "SafeEffect"
          "SideEffectFree"
          "SideEffectsOnly(.*)"
          "StaticallyExecutable"
          "TerminatesExecution"
          "UIEffect"
          "UIPackage"
          "UIType"
          "UsesObjectEquals"
          )
        "\\|")
       "\\)"))



(defun tags-conflict-resolve-annotation-lines ()
  "When two annotation groups are the same, resolve those lines."
  (interactive)

  ;; HEAD and OTHER are the same.
  (tags-query-replace-noerror
   (concat
    less-than-hunk-start-grouped
    (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            left-lines-re
            vertical-bar-separator-re "\\)")
    (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            base-lines-re
            equal-sign-separator-re "\\)")
    "\\2" ;; (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            right-lines-re
            greater-than-hunk-end-re "\\)")
    )
   "\\2\\1\\3\\5\\6")

  ;; HEAD and base are the same.
  (tags-query-replace-noerror
   (concat
    less-than-hunk-start-grouped
    (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            left-lines-re
            vertical-bar-separator-re "\\)")
    "\\2" ;; (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            base-lines-re
            equal-sign-separator-re "\\)")
    (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            right-lines-re
            greater-than-hunk-end-re "\\)")
    )
   "\\5\\1\\3\\4\\6")
  
  ;; OTHER and base are the same.
  (tags-query-replace-noerror
   (concat
    less-than-hunk-start-grouped
    (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            left-lines-re
            vertical-bar-separator-re "\\)")
    (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            base-lines-re
            equal-sign-separator-re "\\)")
    "\\4" ;; (concat "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            right-lines-re
            greater-than-hunk-end-re "\\)")
    )
   "\\4\\1\\3\\5\\6")
  )

(defun tags-conflict-resolve-annotation-lines-in-head ()
  "Move annotations only on the HEAD method before the hunk.
  This assumes there is no corresponding annotation in base or OTHER."
  (interactive)
  (tags-query-replace
   (concat less-than-hunk-start-grouped "\\(\\(" annotation-line-regex "\n\\)+\\)")
   "\\2\\1")
  )

(defun tags-conflict-resolve-annotation-lines-in-other ()
  "Move annotations only on the OTHER method before the hunk.
  This assumes there is no corresponding annotation in base or HEAD."
  (interactive)
  (tags-query-replace
   (concat "^\\("
           less-than-hunk-start-re
           left-lines-re
           vertical-bar-separator-re
           base-lines-re equal-sign-separator-re "\\)"
           "\\(\\(?:" annotation-line-regex "\n\\)+\\)")
   "\\2\\1")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in method signatures
;;;


(defun resolve-method-signature ()

  ;; Resolve the first line of a diff, when HEAD has been edited.
  ;; This version requires "public" at start of \2 and \4.
  (tags-query-replace
   (concat
    less-than-hunk-start-grouped
    "\\( *public .*\n\\)"
    "\\(" "" left-lines-re "" vertical-bar-separator-re "\\)"
    "\\( *public .*\n\\)"
    "\\(" "" base-lines-re "" equal-sign-separator-re "\\)"
    "\\4")
   "\\2\\1\\3\\5")
  ;; The more general version, which I don't seem to need.
  (if nil
      (tags-query-replace
       (concat
        less-than-hunk-start-grouped
        "\\(.*\n\\)"
        "" left-lines-re "" vertical-bar-separator-re "\\)"
        "\\(.*\n\\)"
        "" right-lines "" equal-sign-separator-re "\\)"
        "\\4")
       "\\2\\1\\3\\5"))

  ;; Resolve the first line of a diff, when OTHER has been edited.
  ;; This version requires "public" at start of \2 and \4.
  (tags-query-replace
   (concat
    less-than-hunk-start-grouped
    "\\( *public .*\n\\)"
    "\\(" left-lines-re "" vertical-bar-separator-re "\\)"
    "\\2"
    "\\(" base-lines-re "" equal-sign-separator-re "\\)"
    "\\( *public .*\n\\)")
   "\\5\\1\\3\\4")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts (general case)
;;;


(defun tags-conflict-resolve ()
  "Resolve diffs in the current tags table."
  (interactive)
  (tags-conflict-resolve-empty)
  (tags-conflict-resolve-with-two-same))


(defun conflict-resolve ()
  "Resolve diffs in the current buffer."
  (interactive)
  (conflict-resolve-empty)
  (conflict-resolve-with-two-same))


;; Completely empty diff.
(defvar empty-diff-regexes
  (list
   (concat
    less-than-hunk-start-re
    vertical-bar-separator-re
    equal-sign-separator-re
    greater-than-hunk-end-re)
   ""))

;; Diffs where one of the ancestors is empty.
(defconst left-base-empty-regexes
  (list
   (concat
    less-than-hunk-start-re
    vertical-bar-separator-re
    equal-sign-separator-re
    right-lines-grouped-re
    greater-than-hunk-end-re)
   "\\1"))
(defconst base-right-empty-regexes
  (list  (concat
          less-than-hunk-start-re
          left-lines-grouped-re
          vertical-bar-separator-re
          equal-sign-separator-re
          greater-than-hunk-end-re)
         "\\1"))
(defconst left-right-empty-regexes
  (list
   (concat
    less-than-hunk-start-re
    vertical-bar-separator-re
    base-lines-grouped-re
    equal-sign-separator-re
    greater-than-hunk-end-re)
   ""))

(defun tags-conflict-resolve-empty ()
  (interactive)
  (apply #'tags-query-replace-noerror empty-diff-regexes)
  (apply #'tags-query-replace-noerror left-base-empty-regexes)
  (apply #'tags-query-replace-noerror base-right-empty-regexes)
  (apply #'tags-query-replace-noerror left-right-empty-regexes)
  )

(defun conflict-resolve-empty ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp empty-diff-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp left-base-empty-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp base-right-empty-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp left-right-empty-regexes)
    ))

(defconst same-left-and-base-regexes
  (list
   (concat
    less-than-hunk-start-re
    left-lines-grouped-re
    vertical-bar-separator-re
    "\\1"
    equal-sign-separator-re
    right-lines-re
    greater-than-hunk-end-re)
   "\\2"))

(defconst same-base-and-right-regexes
  (list
   (concat
    less-than-hunk-start-re
    left-lines-re
    vertical-bar-separator-re
    base-lines-grouped-re
    equal-sign-separator-re
    "\\1"
    greater-than-hunk-end-re)
   "\\1"))

(defconst same-left-and-right-regexes
  (list
   (concat
    less-than-hunk-start-re
    left-lines-grouped-re
    vertical-bar-separator-re
    base-lines-re
    equal-sign-separator-re
    "\\1"
    greater-than-hunk-end-re)
   "\\1"))

(defun tags-conflict-resolve-with-two-same ()
  "Resolve diffs in which two of the versions of the text are the same."
  (interactive)
  (apply #'tags-query-replace-noerror same-left-and-base-regexes)
  (apply #'tags-query-replace-noerror same-base-and-right-regexes)
  (apply #'tags-query-replace-noerror same-left-and-right-regexes)
  )

(defun conflict-resolve-with-two-same ()
  "Resolve diffs in which two of the versions of the text are the same."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp same-left-and-base-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp same-base-and-right-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp same-left-and-right-regexes)
    ))

(defvar left-prefix-regexes
  (list
   (concat
    less-than-hunk-start-re
    left-lines-grouped-re
    vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" base-lines-re "\\)")
    (concat "\\(\\)")
    equal-sign-separator-re
    (concat "\\(" "\\1" left-or-right-regexp "\\1" "\\)")
    greater-than-hunk-end-re)
   "\\3"))

(defvar left-suffix-regexes
  (list
   (concat
    less-than-hunk-start-re
    (concat "\\(" left-or-right-regexp "\\)")
    vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" base-lines-re "\\)")
    (concat "\\(\\)")
    equal-sign-separator-re
    (concat "\\(" left-or-right-regexp "\\1" "\\1" "\\)")
    greater-than-hunk-end-re)
   "\\3"))

;; This may be very inefficient.
(defvar right-prefix-regexes
  (list
   (concat
    less-than-hunk-start-re
    (concat "\\(" "\\(" left-or-right-regexp "\\)" "\\(" left-or-right-regexp "\\)" "\\)")
    vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" base-lines-re "\\)")
    (concat "\\(\\)")
    equal-sign-separator-re
    (concat "\\(" "\\2" "\\)")
    greater-than-hunk-end-re)
   "\\1"))

;; This may be very inefficient.
(defvar right-suffix-regexes
  (list
   (concat
    less-than-hunk-start-re
    (concat "\\(" "\\(" left-or-right-regexp "\\)" "\\(" left-or-right-regexp "\\)" "\\)")
    vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" base-lines-re "\\)")
    (concat "\\(\\)")
    equal-sign-separator-re
    (concat "\\(" "\\3" "\\)")
    greater-than-hunk-end-re)
   "\\1"))


(defun conflict-resolve-left-subsequence ()
  "Resolve diffs when left is a subsequence of right."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp left-prefix-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp left-suffix-regexes)
    ))



(defun conflict-resolve-right-subsequence ()
  "Resolve diffs when right is a subsequence of left."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp right-prefix-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp right-suffix-regexes)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remove common prefix from within version control conflicts
;;;

(defun tags-reduce-conflicts ()
  "Reduce conflicts in the current tags table."
  (interactive)
  (tags-reduce-conflicts-same-prefix))

(defun reduce-conflicts ()
  "Reduce conflicts in the current buffer."
  (interactive)
  (reduce-conflicts-same-prefix))

;; TODO: Conflicts with the same suffix.

(defvar same-prefix-regexes
  (list
   (concat
    less-than-hunk-start-grouped
    "\\(\\(?:.*\n\\)+\\)"
    "\\(" left-lines-re "" vertical-bar-separator-re "\\)"
    "\\2"
    "\\(" base-lines-re equal-sign-separator-re "\\)"
    "\\2")
   "\\2\\1\\3\\4"))

(defun tags-reduce-conflicts-same-prefix ()
  "Reduce conflicts that have the same prefix."
  (interactive)
  (apply #'tags-query-replace-noerror same-prefix-regexes)
  )

(defun reduce-conflicts-same-prefix ()
  "Reduce conflicts that have the same prefix."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp same-prefix-regexes)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remove common prefix from within version control conflicts
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special cases
;;;

;; (tags-query-replace (concat less-than-hunk-start-re
;; @AnnotatedFor({?\"interning\"}?)
;; ||||||| merged common ancestors
;; =======
;; @AnnotatedFor({\"lock\"})
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; ") "@AnnotatedFor({\"interning\", \"lock\"})
;; ")
;; 
;; \\(\\(\\(?:import .*;\\)?
;; \\)*\\)"
;;
;; (tags-query-replace (concat less-than-hunk-start-re
;; @AnnotatedFor({\"interning\"})
;; public\\(.*\\) @UsesObjectEquals class \\(.*\\)
;; ||||||| merged common ancestors
;; public\\1 class \\2
;; =======
;; @AnnotatedFor({\"lock\"})
;; public\\1 class \\2
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; ") "@AnnotatedFor({\"interning\", \"lock\"})
;; public\\1 @UsesObjectEquals class \\2
;; ")
;; 
;; (tags-query-replace (concat less-than-hunk-start-re
;; @AnnotatedFor({\"formatter\", \"i18n\"})
;; public\\(.*\\) @UsesObjectEquals class \\(.*\\)
;; ||||||| merged common ancestors
;; public\\1 class \\2
;; =======
;; @AnnotatedFor({\"lock\"})
;; public\\1 class \\2
;; >>>>>>> e7e1e93d462edbc8326a066d532bae9848222596
;; ") "@AnnotatedFor({\"formatter\", \"i18n\", \"lock\"})
;; public\\1 @UsesObjectEquals class \\2
;; ")

;; Help for merge conflicts:
;; 
;; (tags-query-replace (concat less-than-hunk-start-re
;; \\([^|]*\\)||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; \\([^|]*\\)||||||||| merged common ancestors
;; =========
;; =======
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "\\1\\2")
;; 
;; (tags-query-replace (concat less-than-hunk-start-re
;; \\([^|]*\\)||||||| merged common ancestors
;; >>>>>>>>> Temporary merge branch 2
;; =======
;; 
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "\\1")
;; 
;; (tags-query-replace (concat less-than-hunk-start-re
;; ||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; =======
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "")
;; 
;; (tags-query-replace (concat less-than-hunk-start-re
;; \\([^|]*\\)||||||| merged common ancestors
;; >>>>>>>>> Temporary merge branch [0-9]
;; =======
;; \\([^>]*\\)>>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "\\1\\2")
;; 
;; (tags-query-replace (concat less-than-hunk-start-re
;; \\([^|]*\\)||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; \\([^|]*\\)||||||||| merged common ancestors
;; \\([^>]*\\)=========
;; =======
;; \\([^>]*\\)>>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "\\1\\2\\3\4")
;; 
;; 
;; 

(defun resolve-equals-method-conflict ()
  ;; Special case for the `equals()` method.
  (tags-query-replace
   (concat
    less-than-hunk-start-re
    "    public boolean equals(Object obj) {\n"
    vertical-bar-separator-re
    "    public boolean equals(Object obj) {\n"
    equal-sign-separator-re
    "    public boolean equals(@Nullable Object obj) {\n"
    greater-than-hunk-end-re)
   "    public boolean equals(@Nullable Object obj) {\n")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

(defun remove-text-properties-string (s)
  (set-text-properties 0 (length s) nil s)
  s)

(put 'with-temp-buffer 'lisp-indent-function 1)

(defun sorted-non-duplicate-lines (lines1 lines2)
  "Return a string consisting of the unique lines in the two input strings.
In the result, the lines are sorted."
  (save-match-data
    (with-temp-buffer "*sorted-non-duplicate-lines*"
      (insert lines1)
      (insert lines2)
      (delete-duplicate-lines (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (buffer-string))))
;; (sorted-non-duplicate-lines "a\nc\nd\n" "d\ne\nb\nd\n")


(defun tags-query-replace-noerror (from to &optional delimited)
  "Like `tags-query-replace', but does not throw user-error when done."
  (condition-case nil
      (tags-query-replace from to delimited)
    (user-error nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TAGS tables
;;;

(defun read-conflict-files-from-tags-table ()
  "Reald all the files in the tags table into their own buffers."
  (interactive)
  (tags-search less-than-hunk-start-re)
  (while t
    (fileloop-continue)))


;;; TODO: What is the purpose of this?  That is, why would I want to run it?
(defun read-all-files-from-tags-table ()
  "Read all the files in the tags table into their own buffers.
This takes up a ridiculous amount of Emacs memory, for large TAGS tables."
  (interactive)
  (tags-search "\\`[^z]")
  (while t
    (fileloop-continue)))



(provide 'conflict-resolve)
