;;; -*- lexical-binding: t -*-

(defvar browsed-urls '())
(defun browse-url-once (url)
  "Browse a URL, at most once per session.
Maybe this should be enhanced to be once per hour."
  (set-text-properties 0 (length url) nil url)
  (if (not (member url browsed-urls))
      (progn
	(setq browsed-urls (cons url browsed-urls))
	(browse-url url)
	;; Keep the URLs in the correct order when multiple ones are specified,
	;; and try to avoid being mistaken for a robot.  (.3 was too little
	;; to avoid "Are you a robot" challenges from Stack Overflow, but 1
	;; was enough.)
	(sleep-for 1))))

(defun browse-url-once-if-matched (url-regex &optional match-group-number)
  "Visit every URL that matches URL-REGEX."
  (save-excursion
    (while (re-search-forward url-regex nil t)
      (browse-url-once (match-string (or match-group-number 0))))))

(defun browse-url-if-matched (url-regex &optional match-group-number)
  "Visit every URL that matches URL-REGEX."
  (save-excursion
    (while (re-search-forward url-regex nil t)
      (browse-url (match-string (or match-group-number 0))))))

(defun browse-url-once-via-text-properties (anchor-regex &optional match-group-number)
  "Visit every link with anchor text that matches ANCHOR-REGEX."
  (save-excursion
    (while (re-search-forward anchor-regex nil t)
      (browse-url-once
       (url-via-text-properties match-group-number)))))

(defun browse-url-via-text-properties (anchor-regex &optional match-group-number)
  "Visit every link with anchor text that matches ANCHOR-REGEX."
  (save-excursion
    (while (re-search-forward anchor-regex nil t)
      (browse-url
       (url-via-text-properties match-group-number)))))

(defun url-via-text-properties (&optional match-group-number)
  "While point is within anchor text, get the linked-to URL."
  (let ((tproperties (text-properties-at (match-beginning (or match-group-number 0)))))
    (or (plist-get tproperties 'w3m-href-anchor)
	(plist-get tproperties 'shr-url))))

(provide 'browse-url-once)
