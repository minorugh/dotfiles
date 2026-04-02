;;; 09-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; compilation (builtin)
(defun compile-autoclose (buffer string)
  "Close compile window if BUFFER finished successfully, report STRING otherwise."
  (if (and (string-match "compilation" (buffer-name buffer))
	   (string-match "finished" string))
      (progn
	(delete-other-windows)
	(message "Compile successful."))
    (message "Compilation exited abnormally: %s" string)))

(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))
(setq compilation-scroll-output t)
(setq compilation-always-kill t)
(setq compilation-finish-functions #'compile-autoclose)

(leaf git-peek
  :vc (:url "https://github.com/minorugh/git-peek")
  :config
  ;; git-peek: preview window height ratio (1.0 = full height)
  (setq git-peek-preview-height 1.0)

  (defun git-peek-emergency-quit ()
    "Force quit git-peek session and restore the previous window configuration.
Restores modeline color, re-enables dimmer-mode if it was active, and kills
all git-peek buffers."
    (interactive)
    (when git-peek--preview-modeline-cookie
      (set-face-background 'mode-line git-peek--modeline-color-default)
      (setq git-peek--preview-modeline-cookie nil))
    (when (and git-peek--dimmer-was-on (fboundp 'dimmer-mode))
      (dimmer-mode 1))
    (when git-peek--saved-wconf
      (set-window-configuration git-peek--saved-wconf)
      (setq git-peek--saved-wconf nil))
    (dolist (bname '("*git-peek-commits*" "*git-peek-preview*"))
      (when (get-buffer bname) (kill-buffer bname)))
    (message "git-peek: emergency quit")))


(defun my-muhenkan ()
  "Muhenkan key handler: toggle evil state or rescue from any situation.
In evil normal state: switch to insert state.
Otherwise: git-peek quit → minibuffer quit (top-level fallback)
→ deactivate mark → deactivate input method → return to evil normal state."
  (interactive)
  (cond
   ((get-buffer "*git-peek-commits*") (git-peek-emergency-quit))
   ((minibuffer-window-active-p (selected-window))
    (minibuffer-keyboard-quit)
    (when (minibuffer-window-active-p (selected-window))
      (top-level)))
   ((evil-normal-state-p) (evil-insert-state))
   ((use-region-p) (deactivate-mark))
   (current-input-method (deactivate-input-method))
   (t (evil-normal-state) (message "-- NORMAL --"))))

(bind-key "<muhenkan>" #'my-muhenkan)


;;; ps-print / ps-mule (builtin)
;; https://tam5917.hatenablog.com/entry/20120914/1347600433
(when (executable-find "lpr")
  (setq ps-multibyte-buffer 'non-latin-printer)
  (setq ps-paper-type       'a4)
  (setq ps-printer-name      nil)
  (setq ps-print-header      nil)
  (setq ps-print-footer      nil)
  (setq ps-font-size         9)
  (setq ps-font-family      'Courier)
  (setq ps-line-number-font 'Courier)
  (setq ps-line-number       t)
  (setq ps-show-n-of-n       t)
  (defalias 'ps-mule-header-string-charsets 'ignore))


;;; gist (external command)
(defun gist-description ()
  "Add gist description."
  (shell-quote-argument (read-from-minibuffer "Add gist description: ")))

(defun gist-filename ()
  "The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer file name."
  (interactive)
  (let ((file (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (read-from-minibuffer (format "File name (%s): " file) file)))

(defun gist-region-or-buffer ()
  "If region is selected, post from the region.
If region isn't selected, post from the buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not (use-region-p))
	(compile (concat "gist -od " (gist-description) " " file))
      (compile (concat "gist -oPd " (gist-description) " -f " (gist-filename)))))
  (delete-other-windows))

(defun my-tig ()
  "Open tig in the current directory's git repository with gnome-terminal."
  (interactive)
  (let ((root (or (locate-dominating-file default-directory ".git")
                  (error "Git repository not found"))))
    (start-process "tig" nil "gnome-terminal" "--" "bash" "-c"
                   (format "cd %s && tig; exec bash" root))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 09-funcs.el ends here
