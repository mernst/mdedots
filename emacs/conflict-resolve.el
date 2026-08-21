;;; -*- lexical-binding: t -*-

;; This file contains functions that resolve merge conflicts.
;; Also see file diff-clean.el, which is for diffs (not conflicts).

;; Typical workflow for resolving conflicts, using a tags table:
;; Run at the top level: etags $(rg --files-with-matches '^<<<<<<')
;; Visit that tags table.
;; (require 'conflict-resolve)
;; ;; TODO: What is the purpose of this?
;; ;; (read-conflict-files-from-tags-table)
;; Now run as many of the following as desired.
;; (tags-conflict-resolve)
;; (tags-conflict-resolve-annotation-lines)
;; (tags-conflict-resolve-method-signature)
;; (tags-conflict-resolve-equals-method-conflict)

;; When not using a tags table:
;; (conflict-resolve)
;; (conflict-resolve-annotation-lines)
;; (resolve-annotatedfor-conflicts)
;; (move-cf-imports-to-beginning)
;; (resolve-import-conflicts)
;; (conflict-resolve-empty)


;; Most useful for pulling remote into annotated code, such as the
;; Checker Framework annotated JDK:
;; (tags-conflict-resolve-annotation-lines-in-head)
;; (tags-conflict-resolve-annotation-lines-in-other)
;; (tags-conflict-resolve-import-conflicts)


;; Run vc-resolve-conflicts for each file that this matches.
;; (tags-search cr-less-than-hunk-start-re)


(eval-when-compile
  (require 'etags)
  (require 'util-mde))

(autoload 'tags-query-replace-noerror "etags-mde")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defun cr-grouped (regex)
  "Return a regex that places the given regex in a capturing group."
  (concat "\\(" regex "\\)"))
(defun cr-alternatives (&rest regexes)
  "Return a regex that permits any of the given regexes."
  (concat "\\(?:"
          (string-join regexes "\\|")
          "\\)"))


;; A conflict marker is 7 repetitions of a punctuation character, optionally
;; followed by a space and a label.  The label varies with the git command
;; and configuration: "HEAD", "ours", "theirs", a branch name, a full or
;; abbreviated commit SHA, "merged common ancestors", or any of those
;; followed by ":" and a file name.  Therefore, accept an arbitrary label.
;;
;; Only the first of these regexps is anchored with "^".  In an Emacs regexp,
;; "^" is special only at the beginning of the regexp or just after "\\(",
;; "\\(?:", or "\\|"; elsewhere it matches a literal "^" character.  These
;; regexps are concatenated after text that ends with a newline, so anchoring
;; the later ones is both unnecessary and (in most uses) wrong.
(defconst cr-less-than-hunk-start-re
  "^<<<<<<<\\(?: .*\\)?\n")
(defconst cr-less-than-hunk-start-grouped
  (cr-grouped cr-less-than-hunk-start-re))
(defconst cr-vertical-bar-separator-re
  "|||||||\\(?: .*\\)?\n")
(defconst cr-equal-sign-separator-re
  "=======\n")
(defconst cr-greater-than-hunk-end-re
  ">>>>>>>\\(?: .*\\)?\n")

(defun cr-lines-without-at-start-re (char)
  "Matches a sequence of lines that do not start with the given character."
  (concat "\\(?:[^" char "\n].*\n\\|\n\\)*"))

(defconst cr-left-lines-re
  (cr-lines-without-at-start-re "|"))
(defconst cr-left-lines-grouped-re
  (cr-grouped cr-left-lines-re))
(defconst cr-right-lines-re
  (cr-lines-without-at-start-re ">"))
(defconst cr-right-lines-grouped-re
  (cr-grouped cr-right-lines-re))
(defconst cr-base-lines-re
  (cr-lines-without-at-start-re "="))
(defconst cr-base-lines-grouped-re
  (cr-grouped cr-base-lines-re))
;; Acceptable on either side of the "=======" separator, so it excludes the
;; terminators of both sides: "|||||||" for the left and ">>>>>>>" for the right.
(defconst cr-left-or-right-lines-re
  (cr-lines-without-at-start-re "|>"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in @AnnotatedFor annotations
;;;

(defun resolve-annotatedfor-conflicts ()
  "Resolve conflicts that involve only @AnnotatedFor lines.
A caveat:
The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; (read-conflict-files-from-tags-table)
  (tags-search
   (concat cr-less-than-hunk-start-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?"
           cr-vertical-bar-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?"
           cr-equal-sign-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?"
           cr-greater-than-hunk-end-re)
   )
  (ignore-errors
    (while t
      ;; (message "#1 %s" (match-string 1))
      ;; (message "#2 %s" (match-string 1))
      ;; (message "#3 %s" (match-string 3))
      (replace-match (cr-merged-annotated-for (match-string-no-properties 1) (match-string-no-properties 3)))
      (fileloop-continue))
    )
  )

(defun cr-merged-annotated-for (annotatedfor-arg-1 annotatedfor-arg-2)
  "Merge two @AnnotatedFor annotation arguments into one @AnnotatedFor annotation.
Either argument may be nil, meaning that side has no @AnnotatedFor annotation.
If neither side has one, the result is the empty string."
  (save-match-data
    (let* ((args1 (cr-parse-annotatedfor-argument annotatedfor-arg-1))
	   (args2 (cr-parse-annotatedfor-argument annotatedfor-arg-2))
	   (args (sort (delete-dups (append args1 args2)))))
      (if (null args)
	  ""
	(concat "@AnnotatedFor({\""
		(mapconcat #'identity args "\", \"")
		"\"})\n")))))
;; (cr-merged-annotated-for "{\"signature\", \"nullness\" ,\"interning\"}" "{\"propkey\", \"signature\"")

(defun cr-parse-annotatedfor-argument (arg)
  "Given an argument to @AnnotatedFor, return a list of the string arguments.
If ARG is nil, meaning there is no @AnnotatedFor annotation, return nil."
  (and arg
       (split-string arg "\" *, *\"" 'omit-separators "[ {}\"]*")))
;; (cl-assert (equal '("signature") (cr-parse-annotatedfor-argument "{\"signature\"}")))
;; (cl-assert (equal '("signature") (cr-parse-annotatedfor-argument " \"signature\"  "))
;; (cl-assert (equal '("signature" "nullness" "interning") (cr-parse-annotatedfor-argument "{\"signature\", \"nullness\" ,\"interning\"}")


(defun resolve-annotatedfor-usesobjectequals-conflicts ()
  "Resolve conflicts that involve only @AnnotatedFor and @UsesObjectEquals.
A caveat:
The mode-hook might blow away the match-data, in which case first run
   `M-x read-conflict-files-from-tags-table`."
  (interactive)
  ;; (read-conflict-files-from-tags-table)
  (tags-search
   (concat cr-less-than-hunk-start-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\(.*\\)class \\(.*\\)\n"
           cr-vertical-bar-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\2class \\3\n"
           cr-equal-sign-separator-re
           "\\(?:@AnnotatedFor(\\(.*\\))\n\\)?\\(\\2\\(?:@UsesObjectEquals \\|@Interned \\)*class \\3\n\\)"
           cr-greater-than-hunk-end-re
           ))
  (ignore-errors
    (while t
      ;; (message "#1 %s" (match-string 1))
      ;; (message "#2 %s" (match-string 1))
      ;; (message "#3 %s" (match-string 3))
      (message "#6 %s" (match-string 6))
      (replace-match
       (concat (cr-merged-annotated-for (match-string-no-properties 1) (match-string-no-properties 5))
	       (match-string 6)))
      (fileloop-continue))
    )
  )




;; (tags-query-replace (concat cr-less-than-hunk-start-re
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
   (format (concat cr-less-than-hunk-start-re
                   "@AnnotatedFor({?%s}?)\n"
                   "public\\(.*\\) @UsesObjectEquals class \\(.*\\)\n"
                   cr-vertical-bar-separator-re
                   "public\\1 class \\2\n"
                   cr-equal-sign-separator-re
                   "@AnnotatedFor({?%s}?)\n"
                   "public\\1 class \\2\n"
                   ">>>>>>> e7e1e93d462edbc8326a066d532bae9848222596\n"
                   )
           annotatedfor-arg-1 annotatedfor-arg-2)
   (format
    (concat
     "@AnnotatedFor({%s})\n"
     "public\\1 @UsesObjectEquals class \\2\n")
    annotatedfor-arg-combined)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in imports
;;;


;; This is appropriate only for the Checker Framework annotated JDK.
(defun move-cf-imports-from-head-to-before ()
  "Move Checker Framework imports from HEAD to before the hunk."
  (interactive)
  (tags-query-replace-noerror
   (concat cr-less-than-hunk-start-grouped
           "\\(\\(import org.checkerframework..*;\n\\)+\n\\)")
   "\\2\\1")
  )
;; This is appropriate only for the Checker Framework annotated JDK.
(defun move-cf-imports-from-other-to-before ()
  "Move Checker Framework imports from the other side to before the hunk.
The other side is the one after the `=======' separator; see
`move-cf-imports-from-head-to-before' for the HEAD side."
  (interactive)
  (tags-query-replace-noerror
   (concat (cr-grouped
            (concat cr-less-than-hunk-start-re
                    "\\(?:\n\\|import .*;\n\\)*" ; cr-left-lines-re
                    cr-vertical-bar-separator-re
                    "\\(?:\n\\|import .*;\n\\)*" ; cr-base-lines-re
                    cr-equal-sign-separator-re))
           "\\(\\(?:import org.checkerframework..*;\n\\)+\n\\)")
   "\\2\\1")
  )




;; This is superseded by merge-java-imports-driver.sh .
(defun tags-conflict-resolve-import-conflicts ()
  "Resolve conflicts that involve only import lines, by accepting all the lines.
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
   (concat cr-less-than-hunk-start-re
           "\\(\\(?:\\(?:\\(?:// \\)?import .*;\\)?\n\\)*\\)"
           cr-vertical-bar-separator-re
           "\\(\\(?:\\(?:\\(?:// \\)?import .*;\\)?\n\\)*\\)"
           cr-equal-sign-separator-re
           "\\(\\(?:\\(?:\\(?:// \\)?import .*;\\)?\n\\)*\\)"
           cr-greater-than-hunk-end-re)
   )
  (ignore-errors
    (while t
      (message "#1 %s" (match-string 1))
      (message "#3 %s" (match-string 3))
      (replace-match (cr-sorted-non-duplicate-lines (match-string 1) (match-string 3)))
      (fileloop-continue)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in annotations
;;;

(defconst cr-annotation-names
  (list
   ;; "SuppressWarnings(.*)" intentionally omitted; it should be resolved by hand.
   "CallerSensitive"
   "Deprecated.*"
   "Deserializer(.*)"
   "ForceInline"
   "IntrinsicCandidate"
   "Override"

   ;; CF annotations
   "AnnotatedFor(.*)"
   "CFComment(.*)"
   "Covariant({[0-9]})"
   "CreatesMustCallFor"
   "Deterministic"
   "DoesNotUnrefineReceiver(.*)"
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
   ))
(defconst cr-annotation-including-suppresswarnings-names
  (cons "SuppressWarnings(.*)"
        cr-annotation-names))

(defvar cr-annotation-line-regex nil
  "A regular expression that matches a declaration annotation.
The annotation should be written on its own line.  The regexp is
not anchored by \"^\" or \"$\".")
(defvar cr-annotation-including-suppresswarnings-line-regex nil
  "A regular expression that matches a declaration annotation.
The annotation should be written on its own line.  The regexp is
not anchored by \"^\" or \"$\".")
(setq cr-annotation-line-regex
      (concat
       " *@"
       (apply #'cr-alternatives cr-annotation-names)
       )
      cr-annotation-including-suppresswarnings-line-regex
      (concat
       " *@"
       (apply #'cr-alternatives cr-annotation-including-suppresswarnings-names)
       ))



(defun tags-conflict-resolve-annotation-lines ()
  "When two annotation groups are the same, resolve those lines.
Leaves the rest of the conflict as is."
  (interactive)

  ;; HEAD and OTHER are the same.
  (tags-query-replace-noerror
   (concat
    cr-less-than-hunk-start-grouped
    (concat "\\(\\(?:" cr-annotation-including-suppresswarnings-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-left-lines-re
            cr-vertical-bar-separator-re "\\)")
    (concat "\\(\\(?:" cr-annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-base-lines-re
            cr-equal-sign-separator-re "\\)")
    "\\2" ;; (concat "\\(\\(?:" cr-annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-right-lines-re
            cr-greater-than-hunk-end-re "\\)")
    )
   "\\2\\1\\3\\5\\6")

  ;; HEAD and base are the same.
  (tags-query-replace-noerror
   (concat
    cr-less-than-hunk-start-grouped
    (concat "\\(\\(?:" cr-annotation-including-suppresswarnings-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-left-lines-re
            cr-vertical-bar-separator-re "\\)")
    "\\2" ;; (concat "\\(\\(?:" cr-annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-base-lines-re
            cr-equal-sign-separator-re "\\)")
    (concat "\\(\\(?:" cr-annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-right-lines-re
            cr-greater-than-hunk-end-re "\\)")
    )
   "\\5\\1\\3\\4\\6")

  ;; OTHER and base are the same.
  (tags-query-replace-noerror
   (concat
    cr-less-than-hunk-start-grouped
    (concat "\\(\\(?:" cr-annotation-line-regex "\n\\)*\\)")
    (concat "\\("
            cr-left-lines-re
            cr-vertical-bar-separator-re "\\)")
    (concat "\\(\\(?:" cr-annotation-including-suppresswarnings-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-base-lines-re
            cr-equal-sign-separator-re "\\)")
    "\\4" ;; (concat "\\(\\(?:" cr-annotation-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-right-lines-re
            cr-greater-than-hunk-end-re "\\)")
    )
   "\\2\\1\\3\\5\\6")

  ;; Annotations only in HEAD; no annotations in base or OTHER.
  (tags-query-replace-noerror
   (concat
    cr-less-than-hunk-start-grouped
    (concat "\\(\\(?:" cr-annotation-including-suppresswarnings-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-left-lines-re
            cr-vertical-bar-separator-re "\\)")
    (concat "\\("
            " *[^ @\n]" cr-base-lines-re
            cr-equal-sign-separator-re "\\)")
    (concat "\\("
            " *[^ @\n]" cr-right-lines-re
            cr-greater-than-hunk-end-re "\\)")
    )
   "\\2\\1\\3\\4\\5\\6")

  ;; Annotations only in OTHER; no annotations in base or HEAD.
  (tags-query-replace-noerror
   (concat
    cr-less-than-hunk-start-grouped
    (concat "\\("
            " *[^ @\n]"            cr-left-lines-re
            cr-vertical-bar-separator-re "\\)")
    (concat "\\("
            " *[^ @\n]" cr-base-lines-re
            cr-equal-sign-separator-re "\\)")
    (concat "\\(\\(?:" cr-annotation-including-suppresswarnings-line-regex "\n\\)+\\)")
    (concat "\\("
            cr-right-lines-re
            cr-greater-than-hunk-end-re "\\)")
    )
   "\\4\\1\\2\\3\\5\\6")

  )

(defun tags-conflict-resolve-empty-head ()
  "If head is empty and both base and other are not, accept empty head.
Use this with care."
  (interactive)
  (tags-query-replace-noerror
   (concat cr-less-than-hunk-start-re
           ;; no left lines
           cr-vertical-bar-separator-re
           "\\(\n\\|[^=\n].*\n\\)" ;; at least one line
           cr-base-lines-re
           cr-equal-sign-separator-re
           "\\(\n\\|[^>\n].*\n\\)" ;; at least one line
           cr-right-lines-re
           cr-greater-than-hunk-end-re)
   "")
  )

(defun tags-conflict-resolve-annotation-lines-in-head ()
  "Move annotations only on the HEAD method before the hunk.
  This assumes there is no corresponding annotation in base or OTHER."
  (interactive)
  (tags-query-replace
   (concat cr-less-than-hunk-start-grouped "\\(\\(" cr-annotation-line-regex "\n\\)+\\)")
   "\\2\\1")
  )

(defun tags-conflict-resolve-annotation-lines-in-other ()
  "Move annotations only on the OTHER method before the hunk.
  This assumes there is no corresponding annotation in base or HEAD."
  (interactive)
  (tags-query-replace
   (concat "^\\("
           cr-less-than-hunk-start-re
           cr-left-lines-re
           cr-vertical-bar-separator-re
           cr-base-lines-re cr-equal-sign-separator-re "\\)"
           "\\(\\(?:" cr-annotation-line-regex "\n\\)+\\)")
   "\\2\\1")
  )

(defun tags-conflict-resolve-reverse (left right)
  "If base is empty, and the left and right are as given, swap their order."
  (interactive "sLeft regexp: \nsRight regexp: ")
  (tags-query-replace
   (concat "^<<<<<<<.*\n"
           "\\(" left "\\)\n"
           "|||||||.*\n"
           "=======\n"
           "\\(" right "\\)\n"
           ">>>>>>>.*\n")
   (concat "\\2\n\\1\n")))

;; (tags-conflict-resolve-reverse " *@Override" " *@Pure")
;; (tags-conflict-resolve-reverse " *@IntrinsicCandidate" " *@StaticallyExecutable")
;; (tags-conflict-resolve-reverse " *@Override" " *@SideEffectsOnly(.*)")
;; (tags-conflict-resolve-reverse " *@SuppressWarnings(.*)" " *@\\(SideEffectFree\\|SideEffectsOnly(.*)\\)")



(if nil
    (tags-query-replace
     (concat
      (concat
       "\\("
       "<<<<<<<.*\n"
       ".*"
       ;; "    @SuppressWarnings(\"this-escape\")\n"
       "|||||||.*\n"
       "=======\n"
       "\\)")
      "\\( *@\\(?:SideEffectFree\\|Pure\\)\n\\)"
      (concat
       "\\("
       ".*"
       ">>>>>>>.*\n"
       "\\)"))
     (concat
      "\\2"
      "\\1"
      "\\3"
      ))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Resolve version control conflicts in method signatures
;;;

(defun tags-conflict-resolve-method-signature ()
  "Resolve conflicts in which the versions differ in a method signature line.
Operates on the files in the current tags table.
Run this after `tags-conflict-resolve-annotation-lines'.
Each replacement is proposed interactively, because a proposal may be wrong:
the signatures might differ in more than their annotations."
  (interactive)

  ;; Resolve the first line of a diff, when HEAD has been edited.
  ;; Requires an access modifier at start of \2 and \4.
  (tags-query-replace-noerror
   ;; PROBLEM: This matches any signature that differs, not just ones that differ in annotations.
   ;; So be careful when accepting the replacement.
   (concat
    cr-less-than-hunk-start-grouped
    "\\( *\\(?:public\\|private\\|protected\\).*\\( [a-zA-Z0-9_]+\\(?:<[^>\n]+>\\)?\\(?:\\[\\]\\)* [a-zA-Z0-9_]+(\\).*\n\\)"
    "\\(" "" cr-left-lines-re "" cr-vertical-bar-separator-re "\\)"
    "\\( *\\(?:public\\|private\\|protected\\).*\\3.*\n\\)"
    "\\(" "" cr-base-lines-re "" cr-equal-sign-separator-re "\\)"
    "\\5")
   "\\2\\1\\4\\6")
  ;; The more general version, which I don't seem to need.
  (if nil
      ;; TODO! This does not work.  Debug later.
      (tags-query-replace-noerror
       (concat
        cr-less-than-hunk-start-grouped
        "\\(.*\n\\)"
        "\\(" cr-left-lines-re "" cr-vertical-bar-separator-re "\\)"
        "\\(.*\n\\)"
        "\\(" cr-right-lines-re "" cr-equal-sign-separator-re "\\)"
        "\\4")
       "\\2\\1\\3\\5"))

  ;; Resolve the first line of a diff, when OTHER has been edited.
  ;; Requires an access modifier at start of \2 and \4.

  ;; The first version requires "@" in the OTHER line, and is good for added annotations.
  (tags-query-replace-noerror
   ;; PROBLEM: This matches any signature that differs, not just ones that differ in annotations.
   ;; So be careful when accepting the replacement.
   (concat
    cr-less-than-hunk-start-grouped
    "\\( *\\(?:public\\|private\\|protected\\).*\\( [a-zA-Z0-9_]+\\(?:<[^>\n]+>\\)?\\(?:\\[\\]\\)* [a-zA-Z0-9_]+(\\).*\n\\)"
    "\\(" cr-left-lines-re "" cr-vertical-bar-separator-re "\\)"
    "\\2"
    "\\(" cr-base-lines-re "" cr-equal-sign-separator-re "\\)"
    "\\( *\\(?:public\\|private\\|protected\\).*\\3.*\n\\)"
    )
   "\\6\\1\\4\\5")
  (if nil
      (tags-query-replace-noerror
       ;; PROBLEM: This matches any signature that differs, not just ones that differ in annotations.
       ;; So be careful when accepting the replacement.
       (concat
        cr-less-than-hunk-start-grouped
        "\\( *\\(?:public\\|private\\|protected\\) .*\n\\)"
        "\\(" cr-left-lines-re "" cr-vertical-bar-separator-re "\\)"
        "\\2"
        "\\(" cr-base-lines-re "" cr-equal-sign-separator-re "\\)"
        "\\( *\\(?:public\\|private\\|protected\\) .*\n\\)")
       "\\5\\1\\3\\4")
    )
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
(defvar cr-empty-diff-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    cr-vertical-bar-separator-re
    cr-equal-sign-separator-re
    cr-greater-than-hunk-end-re)
   ""))

;; Diffs where one of the ancestors is empty.
(defconst cr-left-base-empty-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    cr-vertical-bar-separator-re
    cr-equal-sign-separator-re
    cr-right-lines-grouped-re
    cr-greater-than-hunk-end-re)
   "\\1"))
(defconst cr-base-right-empty-regexes
  (list  (concat
          cr-less-than-hunk-start-re
          cr-left-lines-grouped-re
          cr-vertical-bar-separator-re
          cr-equal-sign-separator-re
          cr-greater-than-hunk-end-re)
         "\\1"))
(defconst cr-left-right-empty-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    cr-vertical-bar-separator-re
    cr-base-lines-grouped-re
    cr-equal-sign-separator-re
    cr-greater-than-hunk-end-re)
   ""))

(defun tags-conflict-resolve-empty ()
  "Resolve diffs in which at least one of the versions of the text is empty.
Operates on the files in the current tags table."
  (interactive)
  (apply #'tags-query-replace-noerror cr-empty-diff-regexes)
  (apply #'tags-query-replace-noerror cr-left-base-empty-regexes)
  (apply #'tags-query-replace-noerror cr-base-right-empty-regexes)
  (apply #'tags-query-replace-noerror cr-left-right-empty-regexes)
  )

(defun conflict-resolve-empty ()
  "Resolve diffs in which at least one of the versions of the text is empty.
Operates on the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-empty-diff-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-left-base-empty-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-base-right-empty-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-left-right-empty-regexes)
    ))

(defconst cr-same-left-and-base-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    cr-left-lines-grouped-re
    cr-vertical-bar-separator-re
    "\\1"
    cr-equal-sign-separator-re
    cr-right-lines-grouped-re
    cr-greater-than-hunk-end-re)
   "\\2"))

(defconst cr-same-base-and-right-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    cr-left-lines-grouped-re
    cr-vertical-bar-separator-re
    cr-base-lines-grouped-re
    cr-equal-sign-separator-re
    "\\2"
    cr-greater-than-hunk-end-re)
   "\\1"))

(defconst cr-same-left-and-right-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    cr-left-lines-grouped-re
    cr-vertical-bar-separator-re
    cr-base-lines-grouped-re
    cr-equal-sign-separator-re
    "\\1"
    cr-greater-than-hunk-end-re)
   "\\1"))

(defun tags-conflict-resolve-with-two-same ()
  "Resolve diffs in which two of the versions of the text are the same."
  (interactive)
  (apply #'tags-query-replace-noerror cr-same-left-and-base-regexes)
  (apply #'tags-query-replace-noerror cr-same-base-and-right-regexes)
  (apply #'tags-query-replace-noerror cr-same-left-and-right-regexes)
  )

(defun conflict-resolve-with-two-same ()
  "Resolve diffs in which two of the versions of the text are the same."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-same-left-and-base-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-same-base-and-right-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-same-left-and-right-regexes)
    ))

(defvar cr-left-prefix-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    cr-left-lines-grouped-re
    cr-vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" cr-base-lines-re "\\)")
    (concat "\\(\\)")
    cr-equal-sign-separator-re
    (concat "\\(" "\\1" cr-left-or-right-lines-re "\\)")
    cr-greater-than-hunk-end-re)
   "\\3"))

(defvar cr-left-suffix-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    (concat "\\(" cr-left-or-right-lines-re "\\)")
    cr-vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" cr-base-lines-re "\\)")
    (concat "\\(\\)")
    cr-equal-sign-separator-re
    (concat "\\(" cr-left-or-right-lines-re "\\1" "\\)")
    cr-greater-than-hunk-end-re)
   "\\3"))

;; This may be very inefficient.
(defvar cr-right-prefix-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    (concat "\\(" "\\(" cr-left-or-right-lines-re "\\)" "\\(" cr-left-or-right-lines-re "\\)" "\\)")
    cr-vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" cr-base-lines-re "\\)")
    (concat "\\(\\)")
    cr-equal-sign-separator-re
    (concat "\\(" "\\2" "\\)")
    cr-greater-than-hunk-end-re)
   "\\1"))

;; This may be very inefficient.
(defvar cr-right-suffix-regexes
  (list
   (concat
    cr-less-than-hunk-start-re
    (concat "\\(" "\\(" cr-left-or-right-lines-re "\\)" "\\(" cr-left-or-right-lines-re "\\)" "\\)")
    cr-vertical-bar-separator-re
    ;; For now, permit no ancestor text, but do capture group #2.
    ;; (concat "\\(" up-to-5-lines "\\|" cr-base-lines-re "\\)")
    (concat "\\(\\)")
    cr-equal-sign-separator-re
    (concat "\\(" "\\3" "\\)")
    cr-greater-than-hunk-end-re)
   "\\1"))


(defun conflict-resolve-left-subsequence ()
  "Resolve diffs when left is a subsequence of right."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-left-prefix-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-left-suffix-regexes)
    ))



(defun conflict-resolve-right-subsequence ()
  "Resolve diffs when right is a subsequence of left."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-right-prefix-regexes)
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-right-suffix-regexes)
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

(defvar cr-same-prefix-regexes
  (list
   (concat
    cr-less-than-hunk-start-grouped
    "\\(\\(?:.*\n\\)+\\)"
    "\\(" cr-left-lines-re "" cr-vertical-bar-separator-re "\\)"
    "\\2"
    "\\(" cr-base-lines-re cr-equal-sign-separator-re "\\)"
    "\\2")
   "\\2\\1\\3\\4"))

(defun tags-reduce-conflicts-same-prefix ()
  "Reduce conflicts that have the same prefix."
  (interactive)
  (apply #'tags-query-replace-noerror cr-same-prefix-regexes)
  )

(defun reduce-conflicts-same-prefix ()
  "Reduce conflicts that have the same prefix."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (apply #'query-replace-regexp cr-same-prefix-regexes)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remove common prefix from within version control conflicts
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special cases
;;;

(if nil
    (tags-query-replace-noerror
     (concat cr-less-than-hunk-start-grouped
             (cr-grouped " *@Pure\n *@EnsuresNonNullIf(.*)\n")
             cr-left-lines-grouped-re
             (cr-grouped cr-vertical-bar-separator-re)
             cr-base-lines-grouped-re
             (cr-grouped cr-equal-sign-separator-re)
             "\\( *@Override\n\\)"
             cr-right-lines-grouped-re
             (cr-grouped cr-greater-than-hunk-end-re))
     "\\2\\7\\1\\3\\4\\5\\6\\8\\9")
  )


;; (tags-query-replace (concat cr-less-than-hunk-start-re
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
;; (tags-query-replace (concat cr-less-than-hunk-start-re
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
;; (tags-query-replace (concat cr-less-than-hunk-start-re
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
;; (tags-query-replace (concat cr-less-than-hunk-start-re
;; \\([^|]*\\)||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; \\([^|]*\\)||||||||| merged common ancestors
;; =========
;; =======
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "\\1\\2")
;;
;; (tags-query-replace (concat cr-less-than-hunk-start-re
;; \\([^|]*\\)||||||| merged common ancestors
;; >>>>>>>>> Temporary merge branch 2
;; =======
;;
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "\\1")
;;
;; (tags-query-replace (concat cr-less-than-hunk-start-re
;; ||||||| merged common ancestors
;; <<<<<<<<< Temporary merge branch [0-9]
;; =======
;; >>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "")
;;
;; (tags-query-replace (concat cr-less-than-hunk-start-re
;; \\([^|]*\\)||||||| merged common ancestors
;; >>>>>>>>> Temporary merge branch [0-9]
;; =======
;; \\([^>]*\\)>>>>>>> 188671d75f03ca6ac40460c17fd6f35bf91e88f3
;; ") "\\1\\2")
;;
;; (tags-query-replace (concat cr-less-than-hunk-start-re
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

(defun tags-add-nullable-to-equals ()
  "Add @Nullable to the signature of equals."
  (interactive)
  (tags-query-replace-noerror
   (concat (cr-grouped
            (concat (cr-alternatives cr-less-than-hunk-start-re
                                     cr-vertical-bar-separator-re
                                     cr-equal-sign-separator-re)
                    "\\(?: *@Override\n\\)?"))
           "\\( *\\(?:public \\)?boolean equals(\\)\\(Object [a-zA-Z_]+)\\(?:;\\| {\\)\n\\)")
   "\\1\\2@Nullable \\3")
  )

;; tags-add-nullable-to-equals eliminates the need for this, probably.
(defun tags-conflict-resolve-equals-method-conflict ()
  "Special case for the `equals()` method."
  (interactive)
  (tags-query-replace
   (concat
    cr-less-than-hunk-start-re
    "    public boolean equals(Object obj) {\n"
    cr-vertical-bar-separator-re
    "    public boolean equals(Object obj) {\n"
    cr-equal-sign-separator-re
    "    public boolean equals(@Nullable Object obj) {\n"
    cr-greater-than-hunk-end-re)
   "    public boolean equals(@Nullable Object obj) {\n")
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

(defun cr-sorted-non-duplicate-lines (lines1 lines2)
  "Return a string consisting of the unique lines in the two input strings.
In the result, the lines are sorted."
  (save-match-data
    (with-temp-buffer
      (insert lines1)
      (insert lines2)
      (delete-duplicate-lines (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (buffer-string))))
;; (cr-sorted-non-duplicate-lines "a\nc\nd\n" "d\ne\nb\nd\n")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TAGS tables
;;;

(defun read-conflict-files-from-tags-table ()
  "Read all the conflicting files in the tags table into their own buffers."
  (interactive)
  (tags-search cr-less-than-hunk-start-re)
  (ignore-errors
    (while t
      (fileloop-continue))))


;;; TODO: What is the purpose of this?  That is, why would I want to run it?
(defun read-all-files-from-tags-table ()
  "Read all the files in the tags table into their own buffers.
This takes up a ridiculous amount of Emacs memory, for large TAGS tables."
  (interactive)
  (tags-search "\\`[^z]")
  (ignore-errors
    (while t
      (fileloop-continue))))



(provide 'conflict-resolve)
