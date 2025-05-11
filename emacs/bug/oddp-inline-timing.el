;; Difference in time is about 16 vs 11 seconds.

(defsubst my-oddp (x)
  "T if INTEGER is odd."
  (eq (logand x 1) 1))

(defun use-oddp (a b c)
  (and (oddp a) (oddp b) (oddp c)))

(defun use-my-oddp (a b c)
  (and (my-oddp a) (my-oddp b) (my-oddp c)))

(defun test-use-oddp ()
  (interactive)
  (let ((begin-time (current-time))
	(var 100000))
    (while (> var -100000)
      (use-oddp var var var)
      (setq var (- var 1)))
    (let ((end-time (current-time)))
      (message "oddp: %d" (+ (* (- (car end-time) (car begin-time)) (expt 2 16))
			     (- (cadr end-time) (cadr begin-time)))))))

(defun test-use-my-oddp ()
  (interactive)
  (let ((begin-time (current-time))
	(var 100000))
    (while (> var -100000)
      (use-my-oddp var var var)
      (setq var (- var 1)))
    (let ((end-time (current-time)))
      (message "my-oddp: %d" (+ (* (- (car end-time) (car begin-time)) (expt 2 16))
				(- (cadr end-time) (cadr begin-time)))))))
