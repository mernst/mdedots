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

(defun browse-url-once-if-matched (regex &optional match-group-number)
  (save-excursion
    (while (re-search-forward regex nil t)
      (browse-url-once (match-string (or match-group-number 0))))))

(defun browse-url-if-matched (regex &optional match-group-number)
  (save-excursion
    (while (re-search-forward regex nil t)
      (browse-url (match-string (or match-group-number 0))))))

(defun browse-url-once-via-text-properties (regex &optional match-group-number)
  (save-excursion
    (while (re-search-forward regex nil t)
      (browse-url-once
       (url-via-text-properties match-group-number)))))

(defun browse-url-via-text-properties (regex &optional match-group-number)
  (save-excursion
    (while (re-search-forward regex nil t)
      (browse-url
       (url-via-text-properties match-group-number)))))

(defun url-via-text-properties (&optional match-group-number)
  (let ((tproperties (text-properties-at (match-beginning (or match-group-number 0)))))
    (or (plist-get tproperties 'w3m-href-anchor)
	(plist-get tproperties 'shr-url))))

(provide 'browse-url-once)
