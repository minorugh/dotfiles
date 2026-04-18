;;; 02-git.el --- Git configulations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; compilation (builtin)

(leaf git-peek
  :vc (:url "https://github.com/minorugh/git-peek")
  :commands (git-peek git-peek-emergency-quit)
  :bind ("C-x t" . open-tig)
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


(leaf browse-at-remote :ensure t
  :doc "Open github page from Emacs"
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(defun github-deploy ()
  "Insert current changelog buffer into CHANGELOG.md and open it."
  (interactive)
  (let* ((src (buffer-file-name))
         (basename (file-name-nondirectory src))
         (date (and (string-match "changelog-\\([0-9]\\{8\\}\\)\\.md" basename)
                    (match-string 1 basename)))
         (changelog (expand-file-name "~/src/github.com/minorugh/minorugh.github.io/CHANGELOG.md")))
    (if (not date)
        (message "Not a changelog file: %s" basename)
      (let* ((src-content (with-temp-buffer
                            (insert-file-contents src)
                            (buffer-string)))
             (changelog-content (with-temp-buffer
                                  (insert-file-contents changelog)
                                  (buffer-string)))
             (date-header (concat "## "
                                  (substring date 0 4) "-"
                                  (substring date 4 6) "-"
                                  (substring date 6 8) "\n\n")))
        (with-temp-file changelog
          (insert date-header)
          (insert src-content)
          (insert "\n---\n\n")
          (insert changelog-content))
        (find-file changelog)
        (message "CHANGELOG.md updated: %s" date)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolvedc)
;; End:
;;; 02-git.el ends here
