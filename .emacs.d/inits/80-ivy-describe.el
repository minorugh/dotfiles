;;; 80-ivy-describe.el --- My Ivy-based describe tools -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Replicate the usability of Counsel-describe-function using only Ivy
;;
;;; Code:
;; (setq debug-on-error t)


(leaf my-ivy-describe
  :doc "Description tool using Ivy."
  :chord (("df" . ivy-describe-command)
          ("fg" . ivy-describe-variable))
  :config
  (defvar ivy-describe-history nil "ivy-describe-commandの履歴を保存する変数")

  (defun ivy-describe-command ()
    "Ivy's learn-function-enabled describe-function alternative."
    (interactive)
    (let ((cands nil))
      (mapatoms
       (lambda (s)
         (when (commandp s)
           (let* ((name (symbol-name s))
                  (key (where-is-internal s nil t))
                  (key-desc (if key (key-description key) "")))
             (push (format "%-40s %s" name key-desc) cands)))))
      (ivy-read "Command: " cands
                :action (lambda (x)
                          (describe-function (intern (car (split-string x)))))
                :require-match t
                :history 'ivy-describe-history
                :caller  'ivy-describe-command)))

  (defun ivy-describe-variable ()
    "Ivy's describe-variable."
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
