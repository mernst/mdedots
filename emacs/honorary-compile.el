;;; -*- lexical-binding: t -*-

;;; honorary-compile.el --- treat some shell buffers as compilation buffers

(defvar honorary-compilation-minor-mode nil
  "Non-nil if the current buffer should be considered a compilation buffer,
but without having the keymap installed.")
(make-variable-buffer-local 'honorary-compilation-minor-mode)

;; This should probably be able to toggle.
;; Maybe don't bother permitting buffer specification.
(defun honorary-compilation-buffer (&optional buffer)
  "Make the specified or current buffer be considered a compilation buffer."
  (interactive)
  ;; Equally efficient to do save-excursion and set-buffer unconditionally?
  (if (or (not buffer) (eq buffer (current-buffer)))
      (setq honorary-compilation-minor-mode t)
    (with-current-buffer buffer
      (setq honorary-compilation-minor-mode t))))

;; ordinarily, only checks compilation-mode and compilation-minor-mode
(defun compilation-buffer-p--honorary-compilation-buffer (orig-fun buffer)
  (or (funcall orig-fun buffer)
      (with-current-buffer buffer
        honorary-compilation-minor-mode)))
(advice-add 'compilation-buffer-p :around #'compilation-buffer-p--honorary-compilation-buffer)

;; In particular, if Vortex is running in a shell buffer, put point in that
;; buffer and run next-error.
(defun compilation-find-buffer--use-current-buffer (orig-fun &optional avoid-current)
  "If in shell mode, no compilation started, and errors found, use current buffer."
  (condition-case err
      (funcall orig-fun avoid-current)
    (error
     (if (and (eq (car err) 'error)
              (equal (car (cdr err)) "No compilation started!")
              (eq major-mode 'shell-mode)
              ;; (save-excursion
              ;;   (goto-char (point-min))
              ;;   (re-search-forward error-file-line-re nil t))
              )
         (progn
           ;; Prevent compilation-setup from modifying mode-line-process
           (let ((old-mode-line-process mode-line-process))
             ;; Don't do this, because it installs compilation-minor-mode-map
             ;; (compilation-minor-mode 1)      ; arg 1 = turn on
             (compilation-setup)
             (setq mode-line-process old-mode-line-process))
           (setq honorary-compilation-minor-mode t)
           (current-buffer))
       (signal (car err) (cdr err))))))
(advice-add 'compilation-find-buffer :around #'compilation-find-buffer--use-current-buffer)

(provide 'honorary-compile)

;;; honorary-compile.el ends here
