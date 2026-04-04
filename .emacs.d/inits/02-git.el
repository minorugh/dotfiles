;;; 02-git.el --- Git configulations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; compilation (builtin)

(leaf git-peek
  :vc (:url "https://github.com/minorugh/git-peek")
  :commands (git-peek git-peek-emergency-quit)
  :config
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


(defun gitk-open ()
  "Open gitk with current dir.
see https://riptutorial.com/git/example/18336/gitk-and-git-gui"
  (interactive)
  (start-process "gitk" nil "gitk")
  (delete-other-windows))


(defun my-tig ()
  "Open tig in the current directory's git repository with gnome-terminal."
  (interactive)
  (let ((root (or (locate-dominating-file default-directory ".git")
                  (error "Git repository not found"))))
    (start-process "tig" nil "gnome-terminal" "--" "bash" "-c"
                   (format "cd %s && tig; exec bash" root))))


(leaf browse-at-remote :ensure t
  :doc "Open github page from Emacs"
  :config
  (setq browse-at-remote-prefer-symbolic nil))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 02-git.el ends here
