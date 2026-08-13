;;; -*- lexical-binding: t -*-

(require 'browse-url)

(defvar browsed-urls '())
(defun browse-url-once (url)
  "Browse a URL, at most once per session.
Maybe this should be enhanced to be once per hour."
  (interactive (list (read-string "URL: ")))
  (setq url (substring-no-properties url))
  (if (not (member url browsed-urls))
      (progn
	(setq browsed-urls (cons url browsed-urls))
	(browse-url url)
	;; Keep the URLs in the correct order when multiple ones are specified,
	;; and try to avoid being mistaken for a robot.  (.3 was too little
	;; to avoid "Are you a robot" challenges from Stack Overflow, but 1
	;; was enough.)
	(sleep-for 1))))

(defun browse-url-regex-args (prompt)
  "Read a regexp and a match group number, for use in an `interactive' spec.
Prompt with PROMPT.  The match group number is the prefix argument, if any."
  (list (read-regexp prompt)
	(and current-prefix-arg (prefix-numeric-value current-prefix-arg))))

(defun browse-url-once-if-matched (url-regex &optional match-group-number)
  "Visit every URL that matches URL-REGEX.
MATCH-GROUP-NUMBER is the group of URL-REGEX that is the URL; it defaults to 0."
  (interactive (browse-url-regex-args "URL regexp"))
  (save-excursion
    (while (re-search-forward url-regex nil t)
      (browse-url-once (match-string (or match-group-number 0))))))

(defun browse-url-if-matched (url-regex &optional match-group-number)
  "Visit every URL that matches URL-REGEX.
MATCH-GROUP-NUMBER is the group of URL-REGEX that is the URL; it defaults to 0."
  (interactive (browse-url-regex-args "URL regexp"))
  (save-excursion
    (while (re-search-forward url-regex nil t)
      (browse-url (match-string (or match-group-number 0))))))

(defun browse-url-once-via-text-properties (anchor-regex &optional match-group-number)
  "Visit every link with anchor text that matches ANCHOR-REGEX.
MATCH-GROUP-NUMBER is the group of ANCHOR-REGEX that is the anchor text;
it defaults to 0.  Matches that are not links are skipped."
  (interactive (browse-url-regex-args "Anchor text regexp"))
  (save-excursion
    (while (re-search-forward anchor-regex nil t)
      (let ((url (url-via-text-properties match-group-number)))
	(if url
	    (browse-url-once url)
	  (message "No URL at anchor text %s" (match-string-no-properties 0)))))))

(defun browse-url-via-text-properties (anchor-regex &optional match-group-number)
  "Visit every link with anchor text that matches ANCHOR-REGEX.
MATCH-GROUP-NUMBER is the group of ANCHOR-REGEX that is the anchor text;
it defaults to 0.  Matches that are not links are skipped."
  (interactive (browse-url-regex-args "Anchor text regexp"))
  (save-excursion
    (while (re-search-forward anchor-regex nil t)
      (let ((url (url-via-text-properties match-group-number)))
	(if url
	    (browse-url url)
	  (message "No URL at anchor text %s" (match-string-no-properties 0)))))))

(defun url-via-text-properties (&optional match-group-number)
  "While point is within anchor text, get the linked-to URL.
Return nil if the anchor text is not a link."
  (let ((tproperties (text-properties-at (match-beginning (or match-group-number 0)))))
    (or (plist-get tproperties 'w3m-href-anchor)
	(plist-get tproperties 'shr-url))))

(provide 'browse-url-once)
