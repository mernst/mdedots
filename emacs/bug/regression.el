(require 'cl-lib)			; for `cl-assert`

(cl-assert (string-equal "~" (abbreviate-file-name "~")))
(cl-assert (string-equal "~" (abbreviate-file-name (file-truename "~"))))

;; for `compile-abbreviate-directory'
(require 'compile)
(cl-assert (string-equal "~/research/invariants/java/"
		      (compile-abbreviate-directory
		       "~/research/invariants/java/"
		       "~/research/invariants/java/daikon/"
		       "~/research/invariants/daikon.ver3/"
		       "~/research/invariants/")))
