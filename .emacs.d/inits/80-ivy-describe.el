;;; 80-ivy-describe.el --- My Ivy-based describe tools -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Replicate the usability of Counsel-describe-function using only Ivy
;;
;;; Code:
;; (setq debug-on-error t)


(leaf my-ivy-describe
  :doc "Description tool using Ivy."
  :chord (("df" . my-describe-command)
          ("fg" . my-describe-variable))
  :config
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
;;; 80-ivy-describe.el ends here
