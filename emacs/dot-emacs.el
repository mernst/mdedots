;;; -*- lexical-binding: t -*-

;;; dot-emacs.el --- Michael D. Ernst's Emacs customizations (.emacs file)

;;; Commentary:

;; Much customization appears in files loaded from this one,
;; such as startup-functions-mde.el.

;;; Code:

;; Temporary, for debugging.
(add-variable-watcher 'after-save-hook (lambda (&rest x) (message "Variable after-save-hook changed: %S" x)))

;; Put these first to put packages before built-ins, on the load-path.
(setq load-path (cons package-user-dir load-path))
;; (package-initialize)                    ; seems necessary for compiling via makefile

(setq load-prefer-newer t)

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/mew/elisp")
(add-to-list 'load-path "~/java/plume-lib/javadoc-lookup/src/main/emacs")
(let ((default-directory "~/.emacs.d/elpa/"))
  (if (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path)))
(add-to-list 'load-path "~/java/google-java-format/core/src/main/scripts/")


(eval-when-compile
  (require 'ispell)
  (require 'vc-annotate)
  (require 'smerge-mode)
  (require 'rg-result nil t)
  (require 'file-comparison)
  (require 'dbus))

;; To use the ELPA package manager, call  M-x list-packages
;; To install a package:  M-x package-install RET magit RET , or:
;;   * move to a line, and press Enter.
;;   * tab to the "Install" button and press Enter.
;; To update packages, press: U x
;; New packages are installed at ~/.emacs.d/elpa/.
(use-package package
  :ensure nil
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  ;; Putting melpa-stable before built-in because magit needs the latest transient.
  (package-archives '(
                      ("melpa-stable" . "http://stable.melpa.org/packages/")
                      ("melpa"        . "https://melpa.org/packages/")
                      ("gnu"          . "http://elpa.gnu.org/packages/")
                      ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                      )))
;; To fix bug with magit and transient.
(setq package-install-upgrade-built-in t)

;; (require 'benchmark-init nil 'no-error)
;; ;; To view, do one of these:
;; ;;  (benchmark-init/show-durations-tabulated)
;; ;;  (benchmark-init/show-durations-tree)
;; ;; Disable collection of benchmark data after init is done.
;; (if (fboundp 'benchmark-init/deactivate)
;;     (add-hook 'after-init-hook 'benchmark-init/deactivate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Site-specific

(defvar system-site
  (cond ((or (equal "pag-me" (system-name))
             (equal "mernst-laptop" (system-name))
             (equal "mernst-ubuntu" (system-name))
             (equal "mdelap" (system-name))
             (equal "mdet430s" (system-name))
             (equal "MDEX300" (system-name))
             (equal "mdex300" (system-name))
             (equal "mdex61" (system-name))
             (equal "mernst-vmu" (system-name))
             (equal "localhost.localdomain" (system-name))
             (equal "x1-6-00-d0-59-b7-4e-5f" (system-name))
             (equal "UWCSE-TP0IB0O91" (system-name))
             (string-match "24-6-[0-9]+" (system-name))
             (equal "Vigor31" (system-name))
             (equal "JHR" (system-name))
             (string-match "\\.dagstuhl\\.de$" (system-name))
             (equal "ubuntu" (system-name))
             (equal "yoga-ubuntu" (system-name))
             (equal "mdet1700" (system-name))
             (equal "op7010" (system-name))
             (equal "xps8940" (system-name))
             ;; Actually WSL Ubuntu
             (equal "sb2" (system-name))
             (equal "sb2.localdomain" (system-name))
             (equal "sl7" (system-name))
             (equal "sl7.localdomain" (system-name))
             ;; Generally for machines not connected to a university-managed
             ;; file system.
             (equal "warfa.cs.washington.edu" (system-name))
             )
         ;; might also be my home machine; in any event, not a CSE-supported machine
         'laptop)
        ((string-match "\\(\\.\\|^\\)cs\\.washington\\.edu$" (system-name))
         'cse)
        ((equal "uwplse.org" (system-name))
         'uwplse)
        ((equal "u51c281961efd55.ant.amazon.com" (system-name))
         'amazon)
        ((or (equal "VMWXP" (system-name))
             (equal "MDE-X60S" (system-name)))
         'windows)
        ((equal 'darwin system-type)
         'mac)
        ((equal "software.imdea.org" (system-name))
         'imdea)
        ((string-match "^dhcp-.*\\.imdea$" (system-name))
         'mac)
        ((string-match "\\.\\(csail\\|lcs\\)\\.mit\\.edu$" (system-name))
         'csail)
        )
  "Symbol representing the site at which Emacs is running.")
(if (not system-site)
    (error "Where am I?  system-site=nil  (system-name)=%s" (system-name)))

(defmacro cse (&rest body)
  "Execute BODY if running at UW Department of Computer Science & Engineering."
  (declare (indent 0))
  `(if (eq system-site 'cse)
       (progn ,@body)))
(put 'cse 'lisp-indent-function 0)
(defmacro uwplse (&rest body)
  "Execute BODY if running on uwplse.org."
  (declare (indent 0))
  `(if (eq system-site 'uwplse)
       (progn ,@body)))
(put 'uwplse 'lisp-indent-function 0)
(defmacro windows (&rest body)
  "Execute BODY if running on Windows PC."
  (declare (indent 0))
  `(if (eq system-site 'windows)
       (progn ,@body)))
(put 'windows 'lisp-indent-function 0)
(defmacro mac (&rest body)
  "Execute BODY if running on Mac OSX."
  (declare (indent 0))
  `(if (eq system-site 'mac)
       (progn ,@body)))
(put 'mac 'lisp-indent-function 0)
(defmacro csail (&rest body)
  "Execute BODY if running at MIT Computer Science & Artificial Intelligence Lab."
  (declare (indent 0))
  `(if (eq system-site 'csail)
       (progn ,@body)))
(put 'csail 'lisp-indent-function 0)


(with-eval-after-load "edebug"
  (def-edebug-spec cse (body))
  (def-edebug-spec uwplse (body))
  (def-edebug-spec windows (body))
  (def-edebug-spec mac (body))
  (def-edebug-spec csail (body))
  )
(if (or (eq system-site 'laptop)
        (equal (getenv "DISPLAY") ":0")
        (equal (getenv "DISPLAY") "localhost:10.0")
        (equal (getenv "DISPLAY") "localhost:11.0"))
    (setq visible-bell t))


(defun windows-convert-homedir (string)
  (if (and string
           (string-match "^\\(\$HOME\\|\$(HOME)\\|\${HOME}\\|~\\|~mernst\\)\\($\\|/\\)"
                         string))
      (concat "e:/home/" (substring string (match-end 0)))
    string))
;; (defun wcd-test (arg goal)
;;   (let ((result (windows-convert-homedir arg)))
;;     (if (not (equal result goal))
;;      (error "(wcd \"%s\") => \"%s\"   should be: \"%s\"" arg result goal))))
;; (wcd-test "/foo/bar" "/foo/bar")
;; (wcd-test "/foo/bar/" "/foo/bar/")
;; (wcd-test "~" "e:/home/")
;; (wcd-test "~/" "e:/home/")
;; (wcd-test "~/foo" "e:/home/foo")
;; (wcd-test "~mernst" "e:/home/")
;; (wcd-test "~mernst/" "e:/home/")
;; (wcd-test "~mernst/foo" "e:/home/foo")
;; (wcd-test "~foo/foo" "~foo/foo")
;; (wcd-test "$HOME" "e:/home/")
;; (wcd-test "$HOME/" "e:/home/")
;; (wcd-test "$HOME/foo" "e:/home/foo")
;; (wcd-test "$(HOME)" "e:/home/")
;; (wcd-test "$(HOME)/" "e:/home/")
;; (wcd-test "$(HOME)/foo" "e:/home/foo")
;; (wcd-test "${HOME}" "e:/home/")
;; (wcd-test "${HOME}/" "e:/home/")
;; (wcd-test "${HOME}/foo" "e:/home/foo")

(declare-function expand-file-name--windows-homedir "dot-emacs")
(declare-function substitute-in-file-name--windows-homedir "dot-emacs")
(windows
  (defun expand-file-name--windows-homedir (orig-fun name &optional directory)
    (funcall orig-fun
             (windows-convert-homedir name)
             (windows-convert-homedir directory)))
  (advice-add 'expand-file-name :around #'expand-file-name--windows-homedir)
  (defun substitute-in-file-name--windows-homedir (orig-fun filename)
    (funcall orig-fun (windows-convert-homedir filename)))
  (advice-add 'substitute-in-file-name :around #'substitute-in-file-name--windows-homedir)
  ;; Testing:
  ;; (expand-file-name "foo" "~/")
  ;; (substitute-in-file-name "${HOME}/foo")
  )

;; UW CSE-specific
(defun update-conf-mode-hook ()
  "Run the createcal program after its input files have been edited."
  ;; Documentation for createcal: https://courses.cs.washington.edu/tools/createcal/doc/
  (let ((filename (file-truename buffer-file-name)))
    (if (and (string-match "/calendar/\\(inputFiles\\|htmlTemplates\\)/" filename)
             (not (string-match "/503/17sp/" filename)))
        (add-hook 'after-save-hook 'run-createcal nil 'local))))
;; TODO: need to apply this hook to files such as hwlist.template as well as .ini files
(add-hook 'conf-mode-hook 'update-conf-mode-hook)

(defun run-createcal ()
  "Run external program createcal in the parent directory."
  (interactive)
  (shell-command "cd `realpath ..` && createcal")
  ;; Show output if there is any (it will all be error output)
  (if (bufferp "*Shell Command Output*")
      (pop-to-buffer "*Shell Command Output*")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path

;; (defun expand-file-name-maybe (file-name)
;;   "If FILE-NAME exists, expand it with `expand-file-name'; otherwise return nil."
;;   (if file-name
;;       (let ((expanded (expand-file-name file-name)))
;;         (and (file-exists-p expanded)
;;              expanded))))

;; ;; This prevents things from being added to load-path, but doesn't remove
;; ;; them from load-path.
;; (setq load-path-prune-regexps
;;       (mapcar
;;        (function (lambda (str)
;;                    "Make trailing slash optional if at end of string."
;;                    (if (string-match "/$" str)
;;                        (concat
;;                         (substring str 0 (match-beginning 0))
;;                         "\\(/\\|$\\)")
;;                      str)))
;;        (delq nil
;;              (list
;;               ;; Code I don't think I use any more
;;               "/mernst/emacs/old/"
;;               ;; My link to the Emacs distribution; already in load-path
;;               "/mernst/emacs/etc/"
;;               ;; My links to site-lisp; keep only one,
;;               ;; named "/mernst/emacs/site-lisp/".
;;               ;; I do want to add one, which is much of the point
;;               ;; of config-and-add-path anyway.
;;               "/mernst/emacs/uns/"
;; 
;;               ;; prefer ~mernst/emacs/mew/
;;               "/mernst/emacs/mew-"
;; 
;;               ;; Does not contain Emacs Lisp code
;;               "/mernst/emacs/bug/"
;;               ;;; This doesn't seem to work; how do I eliminate this?
;;               ;; Older versions
;;               ;; SEMI succeeds TM; I had no luck with either
;;               ;;            "/tm$"
;;               ;; Tramp directories
;;               "/tramp-2.0.29/\\(contrib\\|info\\|test\\|texi\\|tramp2\\)/"
;;               ))))
;; (setq load-path-prune-noisy t)
;; 
;; (setq load-path-assoc-list-file (expand-file-name "~/emacs/.path.el"))
;; (setq load-path-extra-load-dirs '("/usr/share/emacs/site-lisp"
;;                                   "/usr/share/emacs/21.4/site-lisp"))
;; (load (expand-file-name "~mernst/bin/src/plume-lib/emacs/config-and-add-path"))
;; 
;; ;; This comes after the load, for interactive execution -- and thus, it
;; ;; cannot have the desired effect of suppressing compilation warnings.
;; (eval-when-compile (require 'config-and-add-path))
;; 
;; (setq load-path
;;       (append
;;        ;; Move certain files to the beginning of load-path
;;        (delq nil
;;              (mapcar (function expand-file-name-maybe)
;;                      (list
;;                       ;; fixes
;;                       "~mernst/emacs/fixes"
;; 
;;                       "~mernst/emacs"
;;                       "~mernst/emacs/plume-lib"
;;                       "~mernst/emacs/magit/lisp"
;;                       "~mernst/emacs/mew"
;;                       "~mernst/emacs/latexinfo"
;;                       "~mernst/bin/install/scala/misc/scala-tool-support/emacs"
;;                       )))
;;        load-path
;;        (delq nil
;;              (mapcar (function expand-file-name-maybe)
;;                      (list
;;                       ;; Site-lisp stuff at end because newer versions of Emacs may
;;                       ;; already have what's in site-lisp.
;; 
;;                       ;; (cse "/uns/share/emacs/site-lisp")
;;                       ;; (cse "/uns/share/emacs/site-lisp/elib") ; for pcl-cvs
;;                       )))))
;; 
;; ;; (message load-path)
;; 
;; ;; Is this necessary any more?
;; (cse
;;   (setq load-path
;;      ;; Used to be delete-if, but that requires "(require 'cl-lib)".
;;      (delete nil
;;              (mapcar
;;               ;; This function returns nil if I want to remove the dir,
;;               ;; otherwise returns the dir itself.
;;               (function (lambda (str)
;;                           (and (not (load-path-prune str))
;;                                str)))
;;               load-path))))



(defvar office-program "libreoffice" "A program such as Star Office, Open Office, or Libre Office")

;; This appears early so that even if there's an error elsewhere in
;; this init file, the autoloaded functions still get defined.
(load "autoloads-mde.el" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fonts, colors, etc.
;;;

;; Try to eliminate garbled display.  (This doesn't solve the problem!)
;; (modify-all-frames-parameters '((inhibit-double-buffering . t)))

;; Remove some flashy GUI features.
(menu-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
;; Leave the margins ("fringe"), which are used for line-continuation symbols.
(set-face-background 'fringe "gray85")  ; default is almost unnoticeable pale blue

(defun set-frame-title (string)
  "Set the title of the current frame."
  (interactive "sFrame title: ")
  (modify-frame-parameters (car (frame-list)) `((title . ,string))))
;; (set-frame-title "emacs@manioc")

(defun maximize-frame-height ()
  "Resize window to fill the screen vertically."
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullheight))

;; Anything that sets a specific font should be in .Xresources .

;; Can also set font via shift-leftclick; and just above definition of
;; `mouse-set-font' is a list of fonts.

(defun font-exists-p (font)
  (and (window-system)
       (x-list-fonts font)))
;; (font-exists-p "Inconsolata")

;; (defun inconsolata (size)
;;   "Set font to Inconsolata at the given font size.
;; Use Vera Sans if Inconsolata is not available."
;;   (interactive "nSize for Inconsolata font: ")
;;   (if (font-exists-p "Inconsolata")
;;       (set-frame-font (format "Inconsolata %d" size)))
;;   (maximize-frame-height))

(when (window-system)
  (cond
   ;; Hack is narrower, so I like it better.
   ;; Inconsolata 16 is nearly the same as Hack 14.  Hack has taller half-height, and Hack is bolder.
   ((font-exists-p "Hack") (set-frame-font "Hack 13" nil t))
   ((font-exists-p "-SRC-Hack-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1") (set-frame-font "Hack 13" nil t))

   ((font-exists-p "Inconsolata") (set-frame-font "Inconsolata 14" nil t))

   ((font-exists-p "Courier Prime") (set-frame-font "Courier Prime:spacing=100:size=18" nil t))
   ((font-exists-p "Courier New") (set-frame-font "Courier New:spacing=100:size=18" nil t))

   ;; Monaspace is a very wide font; it does not fit very much text horizontally.
   ;; This one has some serifs
   ;; ((font-exists-p "Monaspace Neon") (set-frame-font "Monaspace Neon 14" nil t))
   ((font-exists-p "Monaspace Argon") (set-frame-font "Monaspace Argon 14" nil t))
   ;; This one has serifs.
   ;; ((font-exists-p "Monaspace Xenon") (set-frame-font "Monaspace Xenon 14" nil t))
   ;; This one is italic.
   ;; ((font-exists-p "Monaspace Radon") (set-frame-font "Monaspace Radon 14" nil t))
   ))

(with-eval-after-load "faces"
  (cond ((font-exists-p "Inconsolata")
         (set-face-attribute 'fixed-pitch nil :family "Inconsolata"))
        ))

(defun hack-font (size)
  "Set the font to Hack, in the given size."
  (interactive "nFont size for Hack: ")
  (set-frame-font (concat "Hack " (int-to-string size))))

;; This is ugly
(defun sans-mono-font (size)
  (format "DejaVu Sans Mono %d" size))

(defun 81-column-frame ()
  "Set the selected frame to 81 columns and full screen height."
  (interactive)
  (set-frame-parameter (selected-frame) 'width 81)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullheight))

(defun 101-column-frame ()
  "Set the selected frame to 101 columns and full screen height."
  (interactive)
  (set-frame-parameter (selected-frame) 'width 101)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullheight))
(global-set-key "\C-x~" #'101-column-frame)

(defun 121-column-frame ()
  "Set the selected frame to 121 columns and full screen height."
  (interactive)
  (set-frame-parameter (selected-frame) 'width 121)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullheight))


;;;
;;; Colors
;;;

(require 'darken-lighten-face)

(with-eval-after-load "font-lock"
  (darken-face-foreground 'font-lock-comment-face)
  (darken-face-foreground 'font-lock-string-face)
  (darken-face-foreground 'font-lock-keyword-face)
  (darken-face-foreground 'font-lock-type-face)
  (darken-face-foreground 'font-lock-constant-face)
  (darken-face-foreground 'font-lock-variable-name-face)
  ;; This darkens it too much; I would like more subtle darkening.
  ;; (darken-face-foreground 'font-lock-function-name-face)
  ;; Leave font-lock-warning-face as is; I want it bright.
  )
(fset 'turn-on-font-lock-if-enabled 'turn-on-font-lock-if-desired)

;; This darkens it too much; I would like more subtle darkening for shadow.
;; (darken-face-foreground 'shadow)


(with-eval-after-load "compile"
  (darken-face-foreground 'compilation-warning))

;; This doesn't work because all of these are inherited, not directly defined.
(with-eval-after-load "dired"
  (darken-face-foreground 'dired-header)
  (darken-face-foreground 'dired-mark)
  (darken-face-foreground 'dired-marked)
  (darken-face-foreground 'dired-flagged)
  (darken-face-foreground 'dired-warning)
  (darken-face-foreground 'dired-directory)
  (darken-face-foreground 'dired-symlink)
  (darken-face-foreground 'dired-ignored))

(with-eval-after-load "font-latex"
  (darken-face-foreground 'font-latex-bold-face)
  (darken-face-foreground 'font-latex-italic-face)
  (darken-face-foreground 'font-latex-math-face)
  (darken-face-foreground 'font-latex-sedate-face)
  (darken-face-foreground 'font-latex-string-face)
  (darken-face-foreground 'font-latex-warning-face)
  (darken-face-foreground 'font-latex-verbatim-face)
  (darken-face-foreground 'font-latex-superscript-face)
  (darken-face-foreground 'font-latex-subscript-face)
  ;; (darken-face-foreground 'font-latex-slide-title-face)
  (darken-face-foreground 'font-latex-doctex-preprocessor-face)
  (darken-face-foreground 'font-latex-doctex-documentation-face))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files that are always loaded or autoloaded
;;;

;; Inserting these files' contents would make this file really big.

(require 'dbus)

(load "mode-hooks-mde" nil t)

(load "prog-modes-mde" nil t)           ; programming language mode hooks

(load "startup-functions-mde" nil t)



(autoload 'timelog "timelog" "Edit the timelog." t)

(defun mew-messages ()
  "Return the number of Mew messages."
  (save-window-excursion
    (mew)
    (count-lines (point-min) (point-max))))

(defun timelog-summarize--add-to-dos (_beg _end)
  (let (;; (messages-summary (timelog-mew-messages-summary))
	(messages-summary (timelog-inbox-threads-summary))
        (to-dos-summary (timelog-to-dos-summary)))
    (insert to-dos-summary
	    messages-summary
	    "\nMy backlog is\n" to-dos-summary
	    messages-summary
	    )))
(advice-add 'timelog-summarize :after #'timelog-summarize--add-to-dos)

(defun timelog-inbox-threads-summary ()
  (let* ((previous-messages (save-excursion
                              (re-search-backward "^\\([0-9]+\\) threads in my inbox" nil t)
                              (match-string 1)))
         (message-summary (concat 
                           " threads in my inbox (previously "
                           previous-messages
                           ")\n")))
    message-summary))

(defun timelog-mew-messages-summary ()
  (let* ((previous-messages (save-excursion
                              (re-search-backward "^\\([0-9]+\\) messages in my inbox" nil t)
                              (match-string 1)))
         (message-summary (concat (int-to-string (mew-messages))
                                  " messages in my inbox (previously "
                                  previous-messages
                                  ")\n")))
    message-summary))


(defun timelog-to-dos-summary ()
  (let* ((previous-to-dos (save-excursion
                            (re-search-backward "^\\([0-9]+\\) to-do items" nil t)
                            (match-string 1)))
         (to-dos (save-window-excursion
                   (edit-to-do)
                   (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       ;; not: (count-matches to-do-priority-regex)
                       ;; Count all paragraphs, because even paragraphs
                       ;; without priority numbers are important in
                       ;; dtermining how well I am keeping up with my work.
                       (count-matches "\n\n")
                       ))))
         (to-dos-summary (concat (int-to-string to-dos)
                                 " to-do items (previously "
                                 previous-to-dos
                                 ")\n")))
    to-dos-summary))

;; (defadvice type-break (after type-break-do-timelog activate)
;;   "Record time spent, just after a typing break."
;;   (timelog))
(global-set-key "\C-cl" 'timelog)


;; ;; Afterward, use:  M-x R RET
;; (if (locate-library "ess-site")
;;     (require 'ess-site))

;; Unset this function because it interferes with completion for M-x javadoc-lookup, which
;; I use more often.  I could also just make this non-interactive, or arrange
;; that ess-julia is autoloaded but not loaded when Emacs starts up.
(fset 'julia-manual-lookup-function nil)


;; From http://stackoverflow.com/questions/6895155/multiple-asynchronous-shell-commands-in-emacs-dired
(defun shell-command--in-new-buffer (_command &optional _output-buffer _error-buffer)
  "Permit multiple asynchronous sub-processes.
Eliminate the question, \"A command is running - kill it?\""
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(advice-add 'shell-command :before #'shell-command--in-new-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages I can't autoload or that I always use

;; (setq large-file-warning-threshold 30000000) ; default 10000000
;; (setq undo-ask-before-discard nil)      ; it's annoying to be constantly asked
;; (setq undo-outer-limit 15000000)        ; default 12000000


;; Mail hackery

;; Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(setq mew-rc-file "~/emacs/mew-mde")
;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

;; Rmail and VM
(defvar mde-vm nil
  "Non-nil if I want to use the VM mail reader.
If this is nil, I should never load or see VM.
This variable must get defined before file \"rmail-mde\" is
loaded, or it's as if the value is nil.")
(setq auto-mode-alist
      (cons '("\\.e?mail\\(-[0-9]*\\)?$" . vm-mode) auto-mode-alist))
(setq auto-mode-alist                   ; e.g., "INBOX-20080902-to-do"
      (cons '("\\(^\\|/\\)INBOX-[0-9]*\\(-to-do\\)?$" . vm-mode) auto-mode-alist))
(with-eval-after-load "rmail" (require 'rmail-mde))
(with-eval-after-load "vm" (require 'rmail-mde))
(with-eval-after-load "vm-startup" (require 'rmail-mde)) ; necessary?
;; This doesn't seem to work
(setq vm-init-file (locate-library "rmail-mde"))
;; sendmail-mde is not loaded by Mew when sending mail, it seems
(with-eval-after-load "sendmail" (load "sendmail-mde" nil t))
(autoload 'mde-mail-alias "sendmail-mde") ; is this the right thing to do?
(autoload 'get-spam-mail "rmail-mde"
  "Move my spam (junk) mail from IMAP to local file spam.mail." t)
(autoload 'get-spam-mail-mde "rmail-mde"
  "Move my personalized spam (junk) mail from IMAP to local file spam.mail." t)
(autoload 'get-bulk-mail "rmail-mde"
  "Move my bulk mail from IMAP to local file .maildrop-bulk.
After running this, run from the shell:  print-mail bulk." t)


;;; This doesn't seem necessary any more.
;; use a better convention than buffer<2> for making buffer names unique.
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward)
;; (setq uniquify-after-kill-buffer-p t)   ; slightly dangerous, but convenient


;; Prevent point from entering the prompt.
(setq minibuffer-prompt-properties
      (nconc minibuffer-prompt-properties
             '(point-entered minibuffer-avoid-prompt)))

;; Small packages

;; define access for some ftp and web sites
(load "ftp-mde" nil t)

;; Enhancement of C-x = (what-cursor-position).
(load "count" nil t)

;; Let shell buffers act like compilation buffers
(require 'honorary-compile)


;;; Instead of editing this, I often want to edit the definition of
;;; `buffer-menu-replacement-alist', which is later in this file.
;; pseudo environment variables.  The cdr should NOT end with a slash.
;; For the buffer menu, see variable `buffer-menu-replacement-alist'.
(load "ffap-mde" nil t)
;;; Experimentally commented out.
;; (setq ffap-semi-env-vars
;;       (append ffap-semi-env-vars
;;               '(
;;                 ("18au" . "$sdi")
;;                 ("13wi" . "$dp")
;;                 ("19wi" . "$se")
;;                 ("19sp" . "$gse")
;;                 ("annotation-file-utilities" . "$afu")
;;                 ("annotation-tools" . "$at")
;;                 ("annotations" . "$anno")
;;                 ("ch" . "$ch")
;;                 ("checker" . "$ch")
;;                 ("checker-framework" . "$cf")
;;                 ("checker-framework-demos" . "$t/checker-framework-demos")
;;                 ("checker-framework-dff" . "$cfd")
;;                 ("checker-framework-inference" . "$cfi")
;;                 ("checker-framework-nondff" . "$cfnd")
;;                 ("daikon" . "$d")
;;                 ("daikon/inv" . "$d/java/daikon/inv")
;;                 ("framework" . "$cf/framework")
;;                 ("handouts" . "$sdi/handouts")
;;                 ("hws" . "$sdi/hws")
;;                 ("java/daikon" . "$d/java/daikon")
;;                 ("javarifier" . "$t/javarifier")
;;                 ("jsr308-langtools" . "$t/jsr308-langtools")
;;                 ("nonnull-interned-demo" . "$t/checker-framework-demos/nonnull-interned-demo")
;;                 ("randoop" . "$HOME/research/testing/randoop")
;;                 ("scene-lib" . "$at/scene-lib")
;;                 ("sparta-meetings" . "$HOME/research/security/sparta-meetings")
;;                 ("tests/daikon-tests" . "$d/java/tests/daikon-tests")
;;                 )))

(setq ffap-url-regexp nil)           ; disable URL features in ffap

;; Don't do this; it messes up format=flowed.
;; ;; nuke-trailing-whitespace is not distributed with Emacs, so use whitespace.el.
;; (autoload 'whitespace-cleanup "whitespace" nil t)
;; ;; This is too much -- should depend on major mode or the like.
;; ;; (add-hook 'mail-send-hook 'whitespace-cleanup)
;; ;; (add-hook 'write-file-functions 'whitespace-cleanup)
;; (defadvice mew-draft-send-message (before whitespace-cleanup-before-send activate)
;;   "Call `whitespace-cleanup' before sending message."
;;   (condition-case nil
;;       (whitespace-cleanup)
;;     ((text-read-only)
;;      ;; This happens for MIME buffers.  Just ignore the error.
;;      nil)))

;;; Server-start should be run only once.
;;; I have the emacs started up in .xinitrc run it.
;;; Subsequent uses of server-start kill previous ones, even after the
;;; subsequent emacses are killed.  I don't see an easy way of determining
;;; whether there's already an emacs extant which will grab the a server
;;; request.  (The .emacs_server file remains even after the emacs creating
;;; it dies.)
;; ;; Start emacs server.  Now do emacsclient foo instead of emacs foo, and
;; ;; the emacs with the server running will edit the file.  Use C-x # to kill
;; ;; the client edit.  Set $EDITOR-like variables to emacsclient, not emacs.
(if (not noninteractive)
    (server-start))

;;; Todo: re-enable?
;; ;; For Google Chrome "Edit with Emacs" extension
;; (require 'edit-server nil t)
;; (if (featurep 'edit-server)
;;     (progn
;;       (setq edit-server-new-frame nil)
;;       (edit-server-start)))
;; (define-key ctl-x-map "\C-c" 'save-buffers-kill-emacs)


;; from https://github.com/progfolio/.emacs.d#vterm
(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window)
  :general
  (+general-global-application
   "t" '(:ignore t :which-key "terminal")
   "tt" 'vterm-other-window
   "t." 'vterm)
  :config
  (evil-set-initial-state 'vterm-mode 'emacs))
;;; This block is used for alpaca or straight, not standard use-package.
;;   :ensure (vterm :post-build
;;                  (progn
;;                    (setq vterm-always-compile-module t)
;;                    (require 'vterm)
;;                    ;;print compilation info for elpaca
;;                    (with-current-buffer (get-buffer-create vterm-install-buffer-name)
;;                      (goto-char (point-min))
;;                      (while (not (eobp))
;;                        (message "%S"
;;                                 (buffer-substring (line-beginning-position)
;;                                                   (line-end-position)))
;;                        (forward-line)))
;;                    (when-let* ((so (expand-file-name "./vterm-module.so"))
;;                                ((file-exists-p so)))
;;                      (make-symbolic-link
;;                       so (expand-file-name (file-name-nondirectory so)
;;                                            "../../builds/vterm")
;;                       'ok-if-already-exists))))

(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el" :branch "master"))
(setq whisper-model "medium.en")
(global-set-key "\C-c\C-w" 'whisper-run)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings

;;; Now the default behavior in Emacs 23, I think.
;; ;; global-set-key binds the key in all modes (not just text)
;; (global-set-key "\C-l" 'smart-recenter)      ; was recenter

;; (global-set-key [S-delete] 'backward-delete-char-untabify)
(global-set-key [(shift delete)] 'backward-delete-char-untabify)

(global-set-key [(home)] 'beginning-of-buffer) ; was beginning-of-line
(global-set-key [(end)] 'end-of-buffer)        ; was end-of-line

;; Which of these two will I prefer?
(global-set-key "\C-c\C-s" 'shell)    ; was undefined; note, conflicts with some packages
(global-set-key "\C-cs" 'shell)    ; was undefined; note, conflicts with rg.el
(global-set-key "\C-cS" 'shell)    ; was undefined

(global-set-key "\C-hn" nil)            ; was view-emacs-news
(global-set-key "\C-\\" nil)            ; was toggle-input-method

(defun edit-to-do ()
  "Edit ~/private/to-do."
  (interactive)
  (find-file (expand-file-name "~/private/to-do")))
;; Sequences consisting of C-c and a letter are reserved for users.
(global-set-key "\C-ct" 'edit-to-do)

(defvar temporary-to-do-file nil
  "The temporary to-do file visited by `edit-temporary-to-do'.")
(defun edit-temporary-to-do (&optional arg)
  "Visit a temporary to-do file.  With prefix arg, set the temporary to-do file."
  (interactive "P")
  (if arg
      (setq temporary-to-do-file (buffer-file-name))
    (if temporary-to-do-file
	(find-file (expand-file-name temporary-to-do-file))
      (error "temporary-to-do-file is not yet set"))))
(global-set-key "\C-c\C-t" 'edit-temporary-to-do)


(defvar to-do-priority-regex "\n\n\\([0-9]\\)[\n ]"
  "Regex that matches a priority for an item in the to-do file.")

(defun rebalance-to-do-priorities ()
  "Change priorities in current to-do document to range smoothly from 1 to 9."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (replace-string-noninteractive "\n\n\n" "\n\n")
      (goto-char (point-min))
      (let ((num-matches (count-matches to-do-priority-regex)))
        (let ((match-num 0))
          (while (re-search-forward to-do-priority-regex nil t)
            (goto-char (match-beginning 1))
            (delete-region (match-beginning 1) (match-end 1))
            (insert (int-to-string (1+ (/ (* match-num 9) num-matches))))
            (setq match-num (1+ match-num))))))))

(defun edit-deadlines ()
  "Edit ~/prof/deadlines."
  (interactive)
  (find-file (expand-file-name "~/private/deadlines")))
;; Sequences consisting of C-c and a letter are reserved for users.
(global-set-key "\C-cd" 'edit-deadlines)

(define-key ctl-x-map "h" 'help-command) ; was mark-whole-buffer
;; (define-key ctl-x-map "r" 'revert-buffer) ; is register prefix; preserve that
(define-key ctl-x-map "\C-r" 'revert-buffer) ; was ffap-read-only
(define-key ctl-x-map "-" 'kill-buffer-and-window) ; was inverse-add-global-abbrev
(define-key ctl-x-map "!" 'shell)       ; was undefined
;; (define-key ctl-x-map "\e" 'repeat-complex-command)
(define-key ctl-x-map [(escape)] 'repeat-complex-command)
;; this is different than the above under Emacs 20.2.96
(define-key ctl-x-map "\C-[" 'repeat-complex-command)
(define-key ctl-x-map "\C-d" 'dired)    ; was list-directory
(define-key ctl-x-map "\C-j" 'dired-jump) ; was undefined
(define-key ctl-x-map "\C-l" 'goto-line)      ; was downcase-region; or, C-c g
(define-key global-map "\C-x4l" 'find-library-other-window)

(defvar ctl-x-ctl-p-map (make-sparse-keymap)
  "Keymap for subcommands of C-x C-p, which are for page handling.")
(fset 'ctl-x-ctl-p-prefix ctl-x-ctl-p-map) ; what is this name good for?
(global-unset-key "\C-x\C-p")           ; was mark-page
(define-key ctl-x-map "\C-p" 'ctl-x-ctl-p-prefix)
(define-key ctl-x-ctl-p-map "\C-d" 'pages-directory)
(define-key ctl-x-ctl-p-map "\C-h" 'headers)
(setq pages-directory-buffer-narrowing-p nil)

;; (define-key esc-map "s" 'center-line)        ; was undefined
(define-key esc-map "?" 'describe-key)  ; was undefined
(define-key esc-map ":" 'tags-search)   ; was undefined
(define-key esc-map "\e" 'eval-expression) ; was keymap
;; I hit M-C-w too often when I mean to hit M-w
(define-key esc-map "\C-w" 'kill-ring-save) ; was append-next-kill
;; Another way to set M-C-space is (global-set-key [C-M-space] ...)
;; "\C- " as an argument to define-key DOES NOT WORK in Emacs 19.(early)
;; This (define-key) worked at Rice on Emacs 19.27, but not Emacs 19.29.
(define-key esc-map [?\C-\ ] 'jump-to-mark-and-pop) ; was mark-sexp
(define-key esc-map "\C-u" 'bury-or-raise-buffer)       ; was backward-up-list MNEMONIC
;; This (define-key) worked at Rice on Emacs 19.27, but not Emacs 19.29.
;; (define-key esc-map [?\C-\S-v] 'scroll-other-window-down)
;; (global-set-key [?\M-\C-\S-v] 'scroll-other-window-down) ; was mark-sexp
(global-set-key [(meta control V)] 'scroll-other-window-down) ; was mark-sexp


(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))
(define-key esc-map "\C-o" 'switch-to-other-buffer) ; was split-line

(define-key isearch-mode-map "\r" 'isearch-RET)
(define-key isearch-mode-map [return] 'isearch-RET)
(defun isearch-RET ()
  "Add RET to the search string and search."
  (interactive)
  (isearch-process-search-char ?\C-j))
(setq isearch 'region) ; change highlighting from unreadable magenta to yellow
(setq isearch-overlay nil)    ; force it to be recreated, lest it be reused


;; I press these keys too often, and I rarely use the functions.
(global-set-key "\C-xj" 'undefined)     ; was skk-auto-fill-mode
(global-set-key "\C-xn" 'undefined)     ; was narrow-to-region
(global-set-key "\C-xp" 'undefined)     ; was narrow-to-page
(global-set-key "\C-x\C-n" 'undefined)  ; was set-goal-column
(global-set-key "\C-x\C-u" 'undefined)  ; was upcase-region
(global-set-key "\ez" 'undefined)       ; was zap-to-char
(global-set-key "\eg" 'undefined)       ; was fill-region
(global-set-key [insert] 'undefined)    ; was overwrite-mode

;; Change ^h to delete
;;(aset global-map 8 'delete-backward-char) ; make ^H work like delete
;;(aset esc-map 63 'help-command) ; make M-? work like ^H used to

;; Commonly visited files; use C-x r j CHAR to find them.
;; This is useful for files that you need to visit frequently,
;; but that you don't want to keep in buffers all the time.
;; (set-register ?a '(file . "~/private/addresses.tex"))
;; (set-register ?e '(file . "~/emacs"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

;; custom-add-option adds additional options to the item; it does not set it.
;; (require 'warnings)
(if (not (boundp 'warning-suppress-types))
    (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)

(cse
  (setq user-mail-address "mernst@cs.washington.edu"))
(csail
  (setq user-mail-address "mernst@csail.mit.edu"))
(setq add-log-mailing-address "mernst@alum.mit.edu")
;; Remove "foo." from "mernst@foo.cs.rice.edu" (and similar)
(if (and user-mail-address
         (string-match "@[a-z0-9]+\\.\\(cs\\.rice\\.edu\\|research\\.microsoft\\.com\\|csail\\.mit\\.edu\\|lcs\\.mit\\.edu\\)$" user-mail-address))
    (setq user-mail-address (concat
                             (substring user-mail-address 0 (match-beginning 0))
                             "@"
                             (match-string 1 user-mail-address))))

;; Properties
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)   ; no longer on a key
(put 'narrow-to-page 'disabled nil)
(put 'erase-buffer 'disabled nil)

(setq completion-ignored-extensions
      (append '(".otl")
              '(".KILL")                 ; newsreader kill files
              '(".bak")                  ; backup files
              '(".bci" ".bif" ".com" ".so" ".ext") ; scheme object files
              '(".gfasl42" ".sfasl42" ".sparcf") ; Common Lisp object files
              ;; '(".pdf" ".PDF")                ; Adobe Portable Document Format files
              completion-ignored-extensions))
(setq completion-ignored-extensions
      (delete ".pdf" completion-ignored-extensions))

(setq minibuffer-max-depth nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "mernst")
(setq initial-scratch-message nil)

(setq message-log-max 500)              ; default 50

(setq kill-read-only-ok t)              ; Permit killing in read-only buffers.
(setq kill-ring-max 100)                ; default 60
(setq kill-whole-line t)

(setq fill-column 79)
(setq truncate-partial-width-windows nil)

;; Don't do this at MSR, lest analysts files can't be read by others.
;; (set-default-file-modes 493)         ; 755 octal

;; ;; Emacs 22 automatically loads ~/.abbrev_defs.
;; (cond ((file-exists-p "~/emacs/.abbrev_defs")
;;        (quietly-read-abbrev-file "~/emacs/.abbrev_defs"))
;;       ((locate-library ".abbrev_defs")
;;        (quietly-read-abbrev-file (locate-library ".abbrev_defs"))))
(abbrev-mode 1)
(setq save-abbrevs nil)                 ; I usually edit the file directly

(line-number-mode 0)

;; I wish there was a way to set the delay to less than one second.
(setq suggest-key-bindings nil)

(setq colon-double-space t)

(setq blink-matching-paren-distance 5000)

(setq shadows-compare-text-p t)

;; (This can't go in files loaded later, I think; it needs to come early.)
;; Don't ask whether to set local variables as specified at end of file.
;; This is dangerous (especially `enable-local-evalf)' but convenient.
(setq enable-local-variables t
      enable-local-eval t)
;; Some of these should already be done automatically by Emacs
(mapc (function (lambda (sym) (put sym 'safe-local-variable 'stringp)))
      '(inleft-string tex-main-file
                      time-stamp-start time-stamp-end time-stamp-format
                      Coding c-indentation-style))
(mapc (function (lambda (sym) (put sym 'safe-local-variable 'integerp)))
      '(time-stamp-count time-stamp-line-limit))
;; Watch out; forms automatically added in ~/.emacs could override this.
(setq safe-local-variable-values
      (append safe-local-variable-values
              '((auto-fill-function . nil))))

(setq version-control t)                ; turn on numeric backups
(setq kept-new-versions 3)              ; default 2
(setq kept-old-versions 0)              ; keep no older versions (default 2)
(setq delete-old-versions t)
(setq backup-by-copying-when-linked t)  ; per everyone at MSR

;; List of auto save file names; commented out to use Emacs's default.
;; (setq auto-save-list-file-name (expand-file-name "~/.emacs-autosaved"))

;; (defun auto-save-locally ()
;;   "Auto-save locally, to avoid delays due to slow NFS connections.
;; The problem with this is that Emacses on a different machine won't find
;; auto-save files.
;; For some reason, `buffer-auto-save-file-name' is nil in RMAIL buffers."
;;   (interactive)
;;   ;; auto-save creates the directory if it doesn't exist; also creates
;;   ;; auto-save-directory-fallback, which I don't like at top-level.
;;   (setq auto-save-directory (concat "/tmp/" (user-login-name) "/autosave/"))
;;   (setq auto-save-directory-fallback (concat "~" (user-login-name) "/tmp/autosave/"))
;;   (require 'auto-save))
;; ;; As of 11/7/96, local autosaving became painfully slow again, though it
;; ;; was being done locally.  Why?  Disable it.
;; ;; (cse (auto-save-locally))

(setq require-final-newline t)          ; files must end with a newline
;; (setq next-line-add-newlines t)

(setq buffer-menu-replacement-alist
      '(
        ;; Replacement is performed for each element in turn.
        ;; Regexps are anchored to beginning of filename.

        ;; Remote directories
        ("/mernst@theory.csail.mit.edu:/u/mernst/" . "th:~/")
        ("/mernst@theory.csail.mit.edu" . "th")
        ("/mernst@theory.csail.mit.edu:/u/mernst/" . "th:~/")
        ("/mernst@theory.csail.mit.edu" . "th")

        ;; Filesystem alternate names
        ("/projects/null/" . "/projects/")
        ("/SDG/" . "/")
        ("/PAG/" . "/")
        ("/var/mnt/uns.share/" . "/uns/share/")

        ;; Home directories
        ;; PAG CSAIL
        ("/g[1-6]/users/\\([a-z0-9_]+\\)/" . "~\\1/")
        ("/afs/csail.mit.edu/u/[a-z]/\\([a-z0-9_]+\\)/" . "~\\1/")
        ;; UW
        ("/homes/gws/mernst/" . "~/")
        ("/homes/gws/\\([a-z]+\\)/" . "~\\1/")

        ("~mernst\\b" . "~")

        ;; Teaching
        ("~/class/331/18au/" . "$sdi/")

        ;; Research
        ("~/research/macros/" . "$macros/")
        ("~/research/fcut/" . "$fcut/")
        ("~/research/invariants/daikon/" . "$d/")
        ("/scratch/mernst/clones/invariants/daikon/" . "$d/")
        ;; ("/projects/se/people/mernst/rothermel/" . "$inv/rothermel/")
        ("~/research/invariants/" . "$inv/")
        ("~/research/nlp/" . "$nlp/")
        ("/scratch/mernst/clones/nlp/" . "$nlp/")
        ("~/research/types/checker-framework/checker/" . "$ch/")
        ("/scratch/mernst/clones/types/checker-framework/checker/" . "$ch/")
        ("~/research/types/checker-framework/" . "$cf/")
        ("/scratch/mernst/clones/types/checker-framework/" . "$cf/")
        ("~/research/types/checker-framework-inference/" . "$cfi/")
        ("/scratch/mernst/clones/types/checker-framework-inference/" . "$cfi/")
        ("~/research/types/notes/" . "$qn/")
        ("/scratch/mernst/clones/types/notes/" . "$qn/")
        ("~/research/testing/" . "$testing/")
        ("/scratch/mernst/clones/testing/" . "$testing/")
        ("~/research/testing/randoop/" . "$randoop/")
        ("/scratch/mernst/clones/testing/randoop/" . "$randoop/")
        ("~/research/types/" . "$t/")
        ("/scratch/mernst/clones/types/" . "$t/")
        ("~/research/notes/version-control/" . "$vc/notes/")
        ("~/research/version-control/" . "$vc/")
        ("~/java/plume-lib/" . "$pl/")
        ("/scratch/mernst/clones/plume-lib/" . "$pl/")

        ("/scratch/mernst/clones/version-control/" . "$vc/")
        ("/afs/csail\\(.mit.edu\\)?/group/pag/" . "$pag/")

        ;; AI research
        ("~/class/573/project/" . "$medic/")

        ))
;; Testing
;; (Buffer-menu-abbreviate-file-name "~/research/invariants/java")
;; (Buffer-menu-abbreviate-file-name "~mernst/research/invariants/java")


;; (setq abbreviated-home-dir "^/u/mernst\\(/\\|$\\)")

;; I can't just modify abbreviated-home-dir because abbreviate-file-name
;; depends on the structure of the regexp.
;; (defadvice abbreviate-file-nameFOO (after abbreviate-home-directory activate)
;;   "Abbreviate home directories, except for the user's own.
;; Abbreviating the user's own home directory interacts poorly with
;; the recursive call to `abbreviate-file-name' for replacing the user's
;; home directory by ~.  The problem is that the advice has already acted
;; on the recursive call, so the regular expression match fails."
;;   (cond
;;    ;; Don't do this; it confuses Emacs.
;;    ;; ;; CSAIL
;;    ;; ((string-match "^/SDG/g2/users/mernst\\b" ad-return-value)
;;    ;;  (setq ad-return-value (concat "~" (substring filename (match-end 0)))))
;;    ;; UW
;;    ((and (string-match "^/homes/gws/[a-z]" ad-return-value)
;;          (not (string-match (concat "^" (regexp-quote (expand-file-name "~"))
;;                                     "\\(/\\|\\'\\)")
;;                             ad-return-value)))
;;     (setq ad-return-value (concat "~" (substring filename (1- (match-end 0))))))
;;    ;; Rice
;;    ((string-match "^/\\(homes/gws\\|/a/santa/comet\\)/mernst\\b"
;;                   ad-return-value)
;;     (setq ad-return-value (concat "~" (substring filename (match-end 0)))))))
;; Testing
;; (abbreviate-file-name "/home/mernst/emacs/two-window.el")
;; (abbreviate-file-name "/a/santa/comet/mernst/emacs/two-window.el")
;; (abbreviate-file-name "/homes/gws/gjb/bin/share/")
;; (abbreviate-file-name "/homes/gws/mernst/bin/share/")

(setq find-file-existing-other-name t)
(setq find-file-suppress-same-file-warnings t)

(defvar path-to-ctags "ctags"
  "Path to ctags program.")
(defun create-tags (dir-name)
  "Create TAGS file for all files under current directory."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

(setq grep-command "grep -n -i ")       ; add case-insensitivity


(defun dont-indent-after-signature (inserted-char)
  (if (and (= ?\n inserted-char)
           (looking-back "\n                    -Mike\n"
                         (1- (save-excursion (beginning-of-line) (point)))))
      'no-indent))
(add-hook 'electric-indent-functions 'dont-indent-after-signature)


;; Ispell

(setq-default ispell-program-name "hunspell")

(setq ispell-silently-savep t)       ; save ispell dictionary w/o confirmation
(setq ispell-query-replace-choices t)   ; make fixes throughout buffer

;; To permit quotes as word characters requires a change to the .aff file.
;; This dictionary's .aff file differs only in its WORCHARS entry.
(with-eval-after-load "ispell"
  (setq ispell-dictionary "en_US_apostrophe")
  (push `("en_US_apostrophe" ,(substitute-in-file-name "$HOME/dots/en_US_apostrophe.aff"))
	ispell-hunspell-dict-paths-alist)
  (setq ispell-local-dictionary-alist
	`(("en_US_apostrophe" "[[:alpha:]]" "[^[:alpha:]]" "[0-9'’]" t ("-d" ,(substitute-in-file-name "$HOME/dots/en_US_apostrophe")) nil utf-8))))

(with-eval-after-load "ispell"
  ;; Hunspell treats apostrophes as part of a word, which behaves badly
  ;; when a word is single-quoted.  This change makes leading and trailing
  ;; single-quotes be ignored.  Here is my Superuser question about it,
  ;; which suggests a different solution:
  ;; http://superuser.com/questions/588548/make-hunspell-ignore-leading-and-trailing-single-quote-characters-apostrophes
  (add-to-list 'ispell-skip-region-alist '("'+\\(\\s \\|[.!?,:]\\|$\\)"))
  (add-to-list 'ispell-skip-region-alist '("\\(\\s \\|^\\)'"))

  ;; ispell.el permits these letters but ignores LocalWords starting
  ;; with these letters.  Skip over leading such characters so we never
  ;; even see a word starting whith these.
  (add-to-list 'ispell-skip-region-alist '("\\b['0-9]+"))

  ;; Hexadecimal numbers
  (add-to-list 'ispell-skip-region-alist '("\\b0x[0-9A-Fa-f]+\\b"))

  )

(with-eval-after-load "ispell"
  ;; These regexes for AMStex block comments interact badly with my
  ;; "[[...]]" style of commenting and TeX's "%" style of commenting.
  ;; As an alternative, I could advise ispell to warn if it saw
  ;; any "%[" strings in the buffer.
  (setcar ispell-tex-skip-alists
          (delete '("%\\[" . "%\\]") (car ispell-tex-skip-alists))))

;; This is needed because `ispell-message' uses this if it is bound,
;; whether or not VM has been used; and VM gets loaded for me when I visit
;; certain files.
;; (setq vm-included-text-prefix "> ")   ; default " > "
(setq vm-included-text-prefix ">")      ; not good for VM, but good for ispell-message


;; Version control

(defalias 'vc-dired 'vc-directory)

(setq vc-follow-symlinks t)

(setq vc-make-backup-files t)
;; Emacs 21.2 uses both vc-diff-switches and diff-switches, so I can't include
;; "-r" in diff-switches.  Bad Emacs 21.2!  I submitted a patch.  Also, the
;; advice below fixes things for me.
;; Unfortunately, as of Emacs 22.0, this is still necessary.
(setq diff-switches
      `("-u" "-b" "-r"
        ,(substitute-in-file-name "--exclude-from=$HOME/dots/diff-exclude-patterns.txt")
        "--minimal"                     ; smaller diffs, but slower execution time
        "--unidirectional-new-file"
        ))
(setq vc-diff-switches '("-u" "-b" "--unidirectional-new-file"))
(setq svn-status-default-diff-arguments '("-x" "--ignore-eol-style" "-x" "--ignore-space-change"))
(defun vc-diff-internal--clear-diff-switches (orig-fun async vc-fileset rev1 rev2 &optional verbose buffer)
  (let ((diff-switches nil))
    (funcall orig-fun async vc-fileset rev1 rev2 verbose buffer)))
(advice-add 'vc-diff-internal :around #'vc-diff-internal--clear-diff-switches)

(defun vc-annotate-revision-next-to-line ()
  "Visit the annotation of the revision after the revision at line.
This is the dual to `vc-annotate-revision-previous-to-line'."
  (interactive)
  (if (not (equal major-mode 'vc-annotate-mode))
      (message "Cannot be invoked outside of a vc annotate buffer")
    (let* ((rev-at-line (vc-annotate-extract-revision-at-line))
           (next-rev nil)
           (rev (car rev-at-line))
           (fname (cdr rev-at-line)))
      (if (not rev-at-line)
          (message "Cannot extract revision number from the current line")
        (setq next-rev
              (vc-call-backend vc-annotate-backend 'next-revision
                               fname rev))
        (vc-annotate-warp-revision next-rev fname)))))
;; vc-annotate-revision-previous-to-line is bound to "a"; use "e" for symmetry.
(with-eval-after-load "vc-annotate"
  (define-key vc-annotate-mode-map "e" 'vc-annotate-revision-next-to-line))



;; The below comment was made on 2010-01-01.  Let's hope it isn't true any longer.
;; ;; connection to tricycle.cs.washington.edu seems to hang with "scp".
;; (setq tramp-default-method "ssh")
(setq tramp-default-method "scp")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jdee-server-dir (expand-file-name "~/.emacs.d/jdee-server"))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((whisper :url "https://github.com/natrys/whisper.el" :branch
              "master")))
 '(tramp-password-prompt-regexp "^.*\\([pP]assword\\|passphrase\\|Response\\).*:\0? *"))


;; To debug tramp, do:
;; (require 'tramp)
;; (setq tramp-debug-buffer t)
;; (setq tramp-verbose 10)



;; Do this rather than setting INFOPATH environment variable becuase that
;; fails to catch the directory in which Emacs put its Info files, I think.
;; I should check that this is still the right thing in FSF Emacs 20.
(setq Info-default-directory-list
      ;; ((Not true any more. -MDE)  These directories need to have a dir or dir.info file.)
      ;; No (cl-remove-duplicates ... :test 'string-equal): duplicates are harmless
      ;; but removal of first occurrence can change order in the Info directory.
      (nconc (list
              "/usr/local/pkg/gcc/gcc-3.2.1/info/"
              "/usr/local/share/info/"
              "/uns/share/info/"        ; unsupported software
              "/usr/local/info/"
              "/usr/share/info/"
              "/usr/info/"
              ;; My customizations at the end
              (expand-file-name "~/emacs/mew/info")
              (expand-file-name "~/tex/info")
              ;; Nothing in ~/bin/src/info/; all is in /uns/share/info at present
              ;; (expand-file-name "~/bin/src/info/")
              )
             ;; info.el should really take care of this; things get confused
             ;;   when default-directory lacks a trailing slash
             (mapcar (function file-name-as-directory)
                     Info-default-directory-list)))
(setq Info-auto-advance t)

(setq browse-url-generic-program
      (cond ((executable-find "google-chrome")
             "google-chrome")
            ((executable-find "chromium-browser")
             "chromium-browser")))

(setq browse-url-browser-function
      (cond ((or (eq window-system 'x)
                 (eq window-system 'pgtk))
             'browse-url-generic)
            (t
             'w3m-browse-url)))
(setq exec-path (append exec-path (list (substitute-in-file-name "$HOME/bin/Linux-i686"))))

(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; no multiframe
(setq-default ediff-ignore-similar-regions t)   ; ignore whitespace differences
(setq ediff-auto-refine-limit 100000)    ; default 14000
(setq ediff-diff-options "-w")

;; TODO: Make a function that toggles this.
;; (setq-default ediff-forward-word-function 'forward-char) ; show character-level differences

(defun strip-single-quotes (string)
  "Remove leading and trailing single quotes if they exist."
  (and string
       (save-match-data
         (if (string-match "^'\\(.*\\)'$" string)
             (match-string 1 string)
           string))))
;; (cl-assert (equal "foo bar" (strip-single-quotes "'foo bar'")))
;; (cl-assert (equal "foo bar" (strip-single-quotes "foo bar")))


;; In diff mode, M-k invokes diff-hunk-kill
(defun diff-hunk-kill--refine-hunk ()
  "Refine the next hunk after killing a hunk."
  (diff-refine-hunk))
(advice-add 'diff-hunk-kill :before #'diff-hunk-kill--refine-hunk)

(setq diff-refine 'font-lock)

(defun smerge-refine-all ()
  "Refine all hunks (within conflict markers) in the current buffer."
  (if (and (featurep 'smerge-mode) smerge-mode)
      (save-excursion
	(condition-case nil
	    (while t
	      (smerge-next)
	      (if (not diff-refine)
		  (smerge-refine)))
	  (error nil)))))
(add-hook 'find-file-hook 'smerge-refine-all t)


(setq apropos-sort-by-scores t)

(setq use-file-dialog nil)

(setq line-move-visual nil)


;;; Ediff

;;; Expermintally commented out, 2025-04-06.  (Would replace with :filter-args advice.)
;; (defadvice ediff-merge-files-with-ancestor (before strip-quotes activate)
;;   (ad-set-arg 0 (strip-single-quotes (ad-get-arg 0)))
;;   (ad-set-arg 1 (strip-single-quotes (ad-get-arg 1)))
;;   (ad-set-arg 2 (strip-single-quotes (ad-get-arg 2)))
;;   (ad-set-arg 4 (strip-single-quotes (ad-get-arg 4))))


(defun ediff-hunk ()
  "Ediff the file containing the current hunk."
  (interactive)
  (goto-char (min (+ (point) 4) (point-max)))
  (let ((case-fold-search nil))
    (or (re-search-backward "^diff" nil t)
        (re-search-backward "^--- ")))
  (re-search-forward "^--- \\([^\t]*\\).*\n\\+\\+\\+ \\([^\t]*\\)")
  (ediff-files (match-string 1) (match-string 2)))

(setq visible-bell t)


;; Garbage-collect whenever switching away from Emacs.
(add-function :after
              after-focus-change-function
              #'(lambda () (unless (frame-focus-state) (garbage-collect))))


;; Default nil, which means never wait.
(setq jit-lock-stealth-time 2.5)


(setq eshell-prompt-function #'(lambda () "ℰ "))

(setq warning-fill-column 120)          ; default 78

(use-package eat :ensure t)
(use-package claude-code :ensure t)
;; (use-package claude-code :ensure t
;;   :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
;;   :config (claude-code-mode)
;;   :bind-keymap ("C-c c" . claude-code-command-map))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(autoload 'claude-code-vterm-mode "claude-code-ui")
(autoload 'vterm-mode "vterm")
;; Claude-code uses projectile
(defun projectile-project-root--not-nil (orig-fn &optional dir)
  "Avoid returning nil from `projectile-project-root'. Can still sometimes do so."
  (let ((result (funcall orig-fn dir)))
    (or result dir default-directory)))
(advice-add 'projectile-project-root :around #'projectile-project-root--not-nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs compatibility
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hacks and bug fixes
;;;

(with-eval-after-load "ediff"
  (require 'file-comparison)
  )
(autoload 'diff-clean "file-comparison"
  "Cleans up a diff to remove uninteresting changes." t)
(autoload 'diff-clean-imports "file-comparison"
  "Cleans up a diff to remove uninteresting changes, including import statements." t)
(autoload 'diff-clean-javadoc "file-comparison"
  "Cleans up a diff to remove uninteresting changes, including API documentation." t)
(autoload 'diff-clean-json "file-comparison"
  "Cleans up a diff to remove uninteresting changes, including .json files." t)
;; This is for pathnames.
;; For basenames (simple file names), use file ~/bin/src/mdedots/dots/diff-exclude-patterns.txt,
;; but that file isn't respected by `git diff`, only by `diff`.
(setq diff-clean-removed-files
      '(
        ".*/annotation-file-utilities/bib/.*"
        ;; Is this pattern desirable?  If so, should it include .*.output?
        ".*/annotation-file-utilities/tests/.*.log"
        ".*/beepbeep-3[^/]*/CoreTest/bin/.*"
        ".*/checker-framework-inference[^/]*/testdata/tmp/.*"
        ".*/checker-framework[^/]*/checker/bin-devel/dockerdir/.*"
        ".*/checker-framework[^/]*/checker/bin-devel/\\.git-scripts/.*"
        ".*/checker-framework[^/]*/checker/bin-devel/\\.html-tools/.*"
        ".*/checker-framework[^/]*/checker/bin-devel/\\.plume-scripts/.*"
        ".*/checker-framework[^/]*/checker/bin-devel/dockerdir/Dockerfile"
        ".*/checker-framework[^/]*/checker/tests/command-line/issue618/out.txt"
        ".*/checker-framework[^/]*/checker/tests/nullness-extra/[^/]*/Out.txt"
        ".*/checker-framework[^/]*/checker/tests/whole-program-inference/annotated/.*" ;; obsolescent
        ".*/checker-framework[^/]*/checker/tests/\\(ainfer\\|wpi\\)-.*/\\(annotated\\|inference-output\\)/.*"
        ".*/checker-framework[^/]*/dataflow/manual/dataflow.\\(html\\|log\\|out\\)"
        ".*/checker-framework[^/]*/docs/examples/MavenExample/Out.txt"
        ".*/checker-framework[^/]*/docs/manual/.*\\.svg"
        ".*/checker-framework[^/]*/docs/manual/bib/.*"
        ".*/checker-framework[^/]*/docs/manual/figures/chainlink.pdf"
        ".*/checker-framework[^/]*/docs/manual/manual.\\(bbl\\|html\\|log\\|out\\)"
        ".*/checker-framework[^/]*/docs/tutorial/src/personalblog-demo/src/net/eyde/personalblog/service/PersonalBlogService.java.orig"
        ".*/checker-framework[^/]*/docs/tutorial/src/personalblog-demo/src/net/eyde/personalblog/struts/action/ReadAction.java.orig"
        ".*/checker-framework[^/]*/framework/tests/returnsreceiverdelomboked/.*"
        ".*/commons-bcel[^/]*/target/.*"
        ".*/daikon[^/]*/java/daikon/config/InvariantDoclet.java"
        ".*/daikon[^/]*/java/daikon/config/ParameterDoclet.java"
        ".*/daikon[^/]*/chicory/Test.log"
        ".*/daikon[^/]*/doc/\\(daikon\\|developer\\).\\(fn\\|cp\\)s?"
        ".*/daikon[^/]*/doc/\\(daikon\\|developer\\).log"
        ".*/daikon[^/]*/doc/\\(daikon\\|developer\\)/.*" ; generated HTML files
        ".*/daikon[^/]*/doc/\\(daikon\\|developer\\)\\.html"
        ".*/daikon[^/]*/java/daikon/chicory/ChicoryTest.log"
        ".*/daikon[^/]*/java/daikon/chicory/Test.inv.out"
        ".*/daikon[^/]*/java/compilation-output.txt"
        ".*/daikon[^/]*/java/dcomp-rt/META-INF/MANIFEST.MF"
        ".*/daikon[^/]*/java/dcomp-rt/dcomp_jdk_static_field_id"
        ".*/daikon[^/]*/java/dcomp-rt/java/lang/jdk_classes.txt"
        ".*/daikon[^/]*/java/dcomp/std_dcomp_out.txt"
        ".*/daikon[^/]*/scripts/dockerdir/.*"
        ".*/daikon[^/]*/utils/.*"
        ".*/defects4j[^/]*/framework/test/d4j.log"
        ".*/jspecify[^/]*/docs/_build/.*"
        ".*/\\(java\\|stub\\)parser.*/target/.*/inputFiles.lst"
        ".*/\\(java\\|stub\\)parser.*/target/.*/GeneratedJavaParser.\\(java\\|html\\)"
        ".*/logging-log4j2.*/target/.*"
        ".*/untangling-tools-benchmark.*/\\.pytest_cache/.*"
        ".*/__pycache__/.*"
        ".*/api/copy.svg"
        ".*/api/element-list"
        ".*/api/legal/COPYRIGHT"
        ".*/api/legal/jquery.md"
        ".*/api/legal/jqueryUI.md"
        ".*/api/legal/LICENSE"
        ".*/api/link.svg"
        ".*/api/module-search-index.js"
        ".*/api/package-search-index.js"
        ".*/api/script-dir/jquery-.*.min.js"
        ".*/api/script-dir/jquery-ui.min.css"
        ".*/api/script.js"
        ".*/api/search.js"
        ".*/api/search-page.js"
        ".*/api/stylesheet.css"
        ".*/api/tag-search-index.js"
        ".*/api/type-search-index.js"
        ".*/\\.git/.*"
        ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs compatibility
;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hacks and bug fixes
;;;

;;; Experimentally commented out, 2025-04-06.
;; (defadvice thing-at-point-url-at-point (around no-trailing-paren activate)
;;   ad-do-it
;;   (if (and ad-return-value (string-match "^\\([^(]*\\))$" ad-return-value))
;;       (setq ad-return-value (match-string 1 ad-return-value))))


;;; Experimentally commented out, 2025-04-06.
;; (defadvice comment-padright (before handle-integer-comment-padding activate)
;;   "If `comment-padding' is an integer, convert it into a string."
;;   (unless comment-padding (setq comment-padding 0))
;;   (when (integerp comment-padding)
;;     (setq comment-padding (make-string comment-padding ? ))))


(defun recenter-top-bottom--redraw-frame (&optional _arg)
  "Redraw the frame if called more than once."
  (if (eq this-command last-command)
      (redraw-frame)))
(advice-add 'recenter-top-bottom :after #'recenter-top-bottom--redraw-frame)


;; This needs to come first to avoid ":around" clobbering other advice.
(defun buffer-menu--pop-to-buffer (_buffer-menu-function &optional arg)
  "Use `pop-to-buffer' instead of `switch-to-buffer'."
  (pop-to-buffer (list-buffers-noselect arg))
  (buffer-menu--display-help))
(advice-add 'buffer-menu :around #'buffer-menu--pop-to-buffer)


(defun strip-line-numbers ()
  "Remove line numbers from error messages in current buffer.
This can make comparisons easier."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp-noninteractive "^\\([^ :]+:\\)[0-9]+" "\\1")))


(defun replace-all-occurrrences-iteratively (regex replacement)
  "Replace all occurrences of REGEX by REPLACEMENT, iterating until
no more occurrences of REGEX appear in the buffer."
  (save-excursion
    (while (progn
             (goto-char (point-min))
             (re-search-forward regex nil t))
      (goto-char (match-beginning 0))
      (replace-regexp-noninteractive regex replacement))))


;; Dramatically improve performance in Emacs 24
(setq-default bidi-display-reordering nil)


;; Set fonts in .Xresources, not here.  A form such as
;;   (inconsolata 13)
;; has an effect, but then the font size changes right back.
;; Apparently .Xresources is read after the .emacs file is?


(if (string-match "Linux.*Microsoft.*Linux"
                  (shell-command-to-string "uname -a"))
    (progn
      ;; (setq system-type-specific 'wsl/linux) ;; for later use.
      (setq
       browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
       browse-url-generic-args     '("/c" "start" "")
       browse-url-browser-function 'browse-url-generic)
      ))

(if (eq system-type 'darwin)
    (progn
      ;; Emacs should interpret the Apple/Command key as the Meta key
      (setq mac-command-modifier 'meta)
      ;; Other possibilities:
      ;; (setq mac-option-modifier 'alt)
      ;; (setq mac-option-modifier 'super)
      ;; (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
      ))

(defun shell-eval-command--remove-carriage-return (orig-fun command)
  (replace-regexp-in-string "\r\n\\'" "\n" (funcall orig-fun command)))
(advice-add 'shell-eval-command :around #'shell-eval-command--remove-carriage-return)


;; Needed on Ubuntu 25.10 and Emacs 30.2.  Was fine on Ubuntu 25.04 and Emacs 30.2.
(defun dired-move-to-end-of-filename--handle-symbolic-link (orig-fun &optional no-error)
  (if-let* ((arrow-end (save-excursion (search-forward " -> " (line-end-position) 'noerror))))
      (progn
        (goto-char (- arrow-end 4))
        (point))
    (funcall orig-fun no-error)))
(advice-add 'dired-move-to-end-of-filename :around #'dired-move-to-end-of-filename--handle-symbolic-link)


(setq use-dialog-box nil)               ; no popups, use keyboard instead


(setq rg-group-result nil)
(setq rg-command-line-flags '("--hidden" "--sort" "path"))
;;; Not needed any more, I think, as of 2025-04-06.
;; (setq rg-default-alias-fallback "everything")
;; (defadvice rg-default-alias (after everything-in-dired-mode activate)
;;   "If in a Dired buffer, search all files rather than depending on the directory name."
;;   (if (eq major-mode 'dired-mode)
;;       (setq ad-return-value (cadr rg-internal-type-aliases))))
;; ;; Why does rg ever pass "--type all"?  rg doesn't return anything in that case.
;; (defadvice rg-default-alias (after dont-use-all activate)
;;   (message "ad-return-value %s" ad-return-value)
;;   (if (equal (car ad-return-value) "all")
;;       (setq ad-return-value (cadr rg-internal-type-aliases))))
;; (with-eval-after-load "rg-result"
;;      (define-key rg-mode-map "\C-b" 'backward-char) ; was rg-back-history
;;      (define-key rg-mode-map "\C-f" 'forward-char)  ; was rg-forward-history
;;      (define-key rg-mode-map "\C-n" 'next-line)     ; was rg-next-file
;;      (define-key rg-mode-map "\C-p" 'previous-line) ; was rg-prev-file
;;      (define-key rg-mode-map "\M-n" 'rg-next-file)  ; was unbound
;;      (define-key rg-mode-map "\M-p" 'rg-prev-file)  ; was unbound
;;      )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of file
;;;

(provide 'dot-emacs)

;;; end of dot-emacs.el
