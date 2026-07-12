;;; 03-ivy.el --- Ivy core settings and describe helpers -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Ivy Core & Tool
;; ============================================================

(leaf ivy
  :ensure t
  :doc "Generic completion mechanism for Emacs."
  :hook (after-init-hook . ivy-mode)
  :bind (:ivy-minibuffer-map
         ("<down>" . ivy-next-line)
         ("<up>"   . ivy-previous-line))
  :config
  (key-chord-define-global "df" 'my-describe-command)
  (key-chord-define-global "fg" 'my-describe-variable)
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-use-selectable-prompt    t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-extra-directories        nil)
  :preface
  (defvar my-describe-history nil "History for `my-describe-command'.")

  (defun my-describe-command ()
    "Search all interactive commands by keybinding or name via Ivy, then call `describe-function' on the selected one."
    (interactive)
    (let ((cands nil))
      (mapatoms
       (lambda (s)
         (when (commandp s)
           (let* ((name     (symbol-name s))
                  (key      (where-is-internal s nil t))
                  (key-desc (if key (key-description key) "")))
             (push (cons (format "%-18s %s" key-desc name) s) cands)))))
      (ivy-read "Find: " cands
                :action        (lambda (x) (describe-function (cdr x)))
                :initial-input "^"
                :require-match t
                :history       'my-describe-history
                :caller        'my-describe-command)))

  (defun my-describe-variable ()
    "Search all bound variables via Ivy, then call `describe-variable'."
    (interactive)
    (let ((cands nil))
      (mapatoms (lambda (s) (when (boundp s) (push (symbol-name s) cands))))
      (ivy-read "Variable: " (sort cands #'string<)
                :action        (lambda (x) (describe-variable (intern x)))
                :require-match t))))


;; ============================================================
;;  Ivy-rich
;; ============================================================

(leaf ivy-rich
  :ensure t
  :doc "More friendly display transformer for ivy."
  :hook (after-init-hook . ivy-rich-mode))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 03-ivy.el ends here
