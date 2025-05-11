;; This code makes it clear that during both pre-command-hook and
;; post-command-hook, last-command is set to the previous command while
;; this-command is set to the command about to be executed (or just
;; executed).

(message-buffer)

(defun mde-pre-command-hook ()
  (if (string= "command-hooks.el" (buffer-name (current-buffer)))
      (mde-print-command-vars "mde-pre-command-hook")))

(defun mde-post-command-hook ()
  (if (string= "command-hooks.el" (buffer-name (current-buffer)))
      (mde-print-command-vars "mde-post-command-hook")))

(defun mde-print-command-vars (string)
  (interactive "s")
  (message "%s :  last-command %s this-command %s"
	   string last-command this-command))


(add-hook 'pre-command-hook 'mde-pre-command-hook)
(add-hook 'post-command-hook 'mde-post-command-hook)



(message "%s" (string= "command-hooks.el" (buffer-name (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Another person remarks:

;I can confirm the symptoms you mention.  Here is another way to
;demonstrate the bug:

; If a process filter calls switch-to-buffer/-other-window/-frame then
; the next input event is interpreted as though the previous buffer
; was still current, ie. the previous buffer's local keymap is used
; and self-inserting characters are inserted in the previous buffer.
; Subsequent input events are interpreted in the new buffer as
; expected.

; (local-set-key events 'self-insert-command)

; (setq events [?q])
(setq events [S-mouse-1])

(defun status (&optional string)
  (interactive (list "Key   "))
  (let ((mess (format "%s  Win %s  Buf %s  Binding %s"
		   string
		   (selected-window)
		   (current-buffer)
		   (local-key-binding events))))
    (message mess)
    (insert mess "\n")))

(local-set-key events 'status)

(defun pre () ""
  (status "Before")
  (setq pre-command-hook))

(defun post () ""
  (status "After ")
  (setq post-command-hook))

(defun filter (proc s)
  (message s)(sit-for 1)
  (switch-to-buffer (process-buffer proc))
  (status "Filter")
  (add-hook 'pre-command-hook 'pre)
  (add-hook 'post-command-hook 'post))

(set-process-filter (start-process "Boo" "Boo" "sh" "-c" "sleep 1;echo Hi") 'filter)
