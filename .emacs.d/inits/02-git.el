;;; 02-git.el --- Git configulations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; compilation (builtin)

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
;;; 02-git.el ends here
