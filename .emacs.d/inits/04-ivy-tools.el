;;; 04-ivy-tools.el --- Ivy-based tools: describe helpers and Git project switcher -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This file configures two Ivy-based utilities:
;;
;; 1. Describe helpers
;;    `my-describe-command'  -- Search all interactive commands by keybinding
;;                              or name and describe the selected one.
;;    `my-describe-variable' -- Search all bound variables and describe the
;;                              selected one.
;;
;; 2. Git project switcher (`ivy-git-project')
;;    Jump to any Git project directory via Ivy.
;;    See https://github.com/minorugh/ivy-git-project for the package.
;;    Personal search paths and alias map are configured here to keep
;;    private path information out of the public repository.
;;
;;; Code:
;; (setq debug-on-error t)

(leaf ivy
  :ensure t
  :doc "Generic completion mechanism for Emacs."
  :hook (after-init-hook . ivy-mode)
  :chord (("df" . my-describe-command)
          ("fg" . my-describe-variable))
  :bind (:ivy-minibuffer-map
         ("<down>" . ivy-next-line-and-call)
         ("<up>"   . ivy-previous-line-and-call))
  :config
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-use-selectable-prompt    t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-extra-directories        nil)

  (defvar my-describe-history nil "History for `my-describe-command'.")

  (defun my-describe-command ()
    "Search all interactive commands by keybinding or name using Ivy,
then call `describe-function' on the selected one."
    (interactive)
    (let ((cands nil))
      (mapatoms
       (lambda (s)
         (when (commandp s)
           (let* ((name (symbol-name s))
                  (key (where-is-internal s nil t))
                  (key-desc (if key (key-description key) "")))
             (push (cons (format "%-18s %s" key-desc name) s) cands)))))
      (ivy-read "Find: " cands
                :action (lambda (x) (describe-function (cdr x)))
                :initial-input "^"
                :require-match t
                :history 'my-describe-history
                :caller 'my-describe-command)))

  (defun my-describe-variable ()
    "Search all bound variables using Ivy, then call `describe-variable'
on the selected one."
    (interactive)
    (let ((cands nil))
      (mapatoms (lambda (s) (when (boundp s) (push (symbol-name s) cands))))
      (ivy-read "Variable: " (sort cands #'string<)
                :action (lambda (x) (describe-variable (intern x)))
                :require-match t))))

;; ─────────────────────────────────────────
;; Git Project Switcher
;;
;; Provides `ivy-git-project-switch', an Ivy-based command to
;; jump to any Git project directory.
;;
;; How it works:
;;   1. Searches each directory listed in `ivy-git-project-search-dirs'
;;      up to `ivy-git-project-search-depth' levels deep, collecting
;;      every entry named ".git" (both directories and gitfiles).
;;   2. Applies `ivy-git-project-alias-map': any project whose real
;;      .git lives outside Dropbox (to avoid sync conflicts) is
;;      replaced by its Dropbox counterpart, which holds the actual
;;      data.  The canonical .git path is also excluded from the list
;;      so it does not appear twice.
;;   3. Presents the deduplicated, sorted list via Ivy and opens the
;;      chosen directory in Dired.
;; ─────────────────────────────────────────
(leaf ivy-git-project
  :doc "Ivy-based Git project switcher with alias map support."
  :vc (:url "https://github.com/minorugh/ivy-git-project")
  :bind ("C-x p" . ivy-git-project-switch)
  :config
  (setq ivy-git-project-search-dirs
        '("~/src/github.com/minorugh/" "~/Dropbox/GH/" "~/Dropbox/minorugh.com/" "~/.env_source/"))
  (setq ivy-git-project-search-depth 3)
  (setq ivy-git-project-alias-map
        '(("~/src/github.com/minorugh/GH/"           . "~/Dropbox/GH/")
          ("~/src/github.com/minorugh/minorugh.com/" . "~/Dropbox/minorugh.com/"))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 04-ivy-tools.el ends here
