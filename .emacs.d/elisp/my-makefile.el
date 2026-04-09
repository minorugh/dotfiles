;;; my-makefile.el --- ivy-based make target selector  -*- lexical-binding: t -*-

;;; Commentary:
;; Select and run Makefile targets via ivy completion.
;; Supports Makefile buffers and dired directories containing a Makefile.
;;
;; Targets are parsed from `make -qp` output.
;; Inline descriptions are extracted from ## comments on target lines:
;;   mytarget: ## description shown in ivy
;;
;; Keybinding: @ in evil-normal-state (set via leaf in 09-funcs.el)

;;; Code:
;; (setq debug-on-error t)

;;; compilation (builtin)

(defun my-make--find-makefile ()
  "Return Makefile path if available in current dired or buffer context."
  (let ((dir (cond
              ;; diredバッファーのカレントディレクトリ
              ((derived-mode-p 'dired-mode)
               (dired-current-directory))
              ;; Makefileを直接開いている
              ((and buffer-file-name
                    (string= (file-name-nondirectory buffer-file-name) "Makefile"))
               (file-name-directory buffer-file-name))
              ;; その他：バッファーのデフォルトディレクトリ
              (t default-directory))))
    (let ((mk (expand-file-name "Makefile" dir)))
      (when (file-exists-p mk) mk))))

(defun my-make--targets-with-desc (makefile)
  "Return alist of (target . description) from MAKEFILE, in file order."
  (let ((ignore-targets '("Makefile" "makefile" "all" "a.out" ".PHONY")))
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (let ((targets '()))
        (while (re-search-forward
                "^\\([^.#[:space:]%][^[:space:]]*\\):\\(?:.*##[[:space:]]*\\(.*\\)\\)?$" nil t)
          (let ((target (match-string 1))
                (desc   (match-string 2)))
            (unless (or (assoc target targets)
                        (member target ignore-targets))
              (push (cons target (or desc "")) targets))))
        (nreverse targets)))))

(defun my-make--format-candidate (pair)
  "Format (target . desc) as ivy candidate string."
  (let ((target (car pair))
        (desc   (or (cdr pair) "")))
    (if (string= desc "")
        (propertize target 'face 'font-lock-function-name-face)
      (format "%-24s %s"
              (propertize target 'face 'font-lock-function-name-face)
              (propertize desc   'face 'font-lock-comment-face)))))

;;;###autoload
(defun my-make-ivy ()
  "Select make target with ivy and run make via compile."
  (interactive)
  (let ((makefile (my-make--find-makefile)))
    (unless makefile
      (user-error "Makefile not found"))
    (let* ((pairs      (my-make--targets-with-desc makefile))
           (candidates (mapcar (lambda (p)
                                 (propertize (my-make--format-candidate p)
                                             'my-target (car p)))
                               pairs))
           (default-directory (file-name-directory makefile)))
      (ivy-read "Make target: "
                candidates
		:action (lambda (candidate)
			  (let ((target (get-text-property 0 'my-target candidate)))
			    (compile (format "make -f %s %s" makefile target))
			    (switch-to-buffer-other-window "*compilation*")))))))
		
(provide 'my-makefile)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; my-makefile.el ends here
