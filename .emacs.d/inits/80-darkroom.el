;;; 80-darkroom.el --- Writing mode configulation. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf darkroom :ensure t
  :defun evil-emacs-state evil-normal-state
  :doc "Remove visual distractions and focus on writing"
  :bind (([f8] . my:darkroom-in)
	 (:darkroom-mode-map
	  ([f8] . my:darkroom-out)))
  :config
  (defun my:darkroom-in ()
    "Enter to the `darkroom-mode'."
    (interactive)
    (diff-hl-mode 0)
    (display-line-numbers-mode 0)
    (darkroom-tentative-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing .2)
    (evil-emacs-state))
  (defun my:darkroom-out ()
    "Returns from `darkroom-mode' to the previous state."
    (interactive)
    (darkroom-tentative-mode 0)
    (display-line-numbers-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing 0)
    (evil-normal-state)))

;; checker for textlint
;; see "https://qiita.com/mhatta/items/8f2aaa4e27c8f5a4c001?utm_source=pocket_shared"
(flycheck-define-checker textlint
  "A linter for prose."
  :command ("textlint" "--format" "unix" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode web-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 80-darkroom.el ends here
