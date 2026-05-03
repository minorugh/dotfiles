;;; 04-ivy-tools.el --- My Ivy-based describe tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf ivy
  :ensure t
  :hook (after-init-hook . ivy-mode)
  :chord (("df" . my-describe-command)
          ("fg" . my-describe-variable))
  :bind (:ivy-minibuffer-map
         ;; ("C-n"    . ivy-next-line-and-call)
         ;; ("C-p"    . ivy-previous-line-and-call)
         ("<down>" . ivy-next-line-and-call)
         ("<up>"   . ivy-previous-line-and-call))
  :config
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-use-selectable-prompt    t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-extra-directories        nil)

  (defvar my-describe-history nil "Variable to store history of describe-command.")

  (defun my-describe-command ()
    "Search and describe commands by keybinding or name using Ivy."
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
    "Search and describe variables using Ivy."
    (interactive)
    (let ((cands nil))
      (mapatoms (lambda (s) (when (boundp s) (push (symbol-name s) cands))))
      (ivy-read "Variable: " (sort cands #'string<)
		:action (lambda (x) (describe-variable (intern x)))
		:require-match t))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 04-ivy-tools.el ends here
