;;; my-makefile.el --- ivy-based make target selector  -*- lexical-binding: t -*-

;;; Commentary:
;; Select and run Makefile targets via ivy completion.
;; Supports Makefile buffers and `dired' directories containing a Makefile.
;;
;; Targets are parsed from `make -qp` output.
;; Inline descriptions are extracted from ## comments on target lines:
;;   mytarget: ## description shown in ivy
;;
;; Keybinding: @ in evil-normal-state (set via leaf in 09-funcs.el)

;;; Code:
;; (setq debug-on-error t)

;;; compilation (builtin)

(require 'ivy)

(defun my-make--find-makefile ()
  "Return the Makefile path from the current context (Dired or buffer)."
  (let ((dir (cond
              ((derived-mode-p 'dired-mode) (dired-current-directory))
              ((and buffer-file-name
                    (string= (file-name-nondirectory buffer-file-name) "Makefile"))
               (file-name-directory buffer-file-name))
              (t default-directory))))
    (let ((mk (expand-file-name "Makefile" dir)))
      (when (file-exists-p mk) mk))))

(defun my-makefile-imenu-create-index ()
  "Indexing for Makefile only."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
        (let ((target (match-string 1))
              (pos    (match-beginning 1)))
          (push (cons target pos) index))))
    (nreverse index)))

;;;###autoload
;; Enter : jump to that location
;; C-c C-c : execute make
;; C-g : return to the original position"
(defun my-make-ivy ()
  "Select Makefile target with preview."
  (interactive)
  (let ((makefile (my-make--find-makefile))
        (orig-point (point))
        (orig-buf (current-buffer))
        (candidates nil))
    (unless makefile (user-error "Makefileが見つかりません"))

    ;; 候補の抽出
    (with-current-buffer (find-file-noselect makefile)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
          (let* ((target (match-string 1))
                 (desc   (match-string 2))
                 (pos    (match-beginning 1))
                 (ignore '("Makefile" "makefile" "all" "a.out" ".PHONY"))
                 (target-fmt (propertize (format "%-24s" target) 'face 'font-lock-function-name-face))
                 (desc-fmt (propertize (or desc "") 'face 'font-lock-comment-face)))
            (unless (member target ignore)
              (push (cons (concat target-fmt " " desc-fmt)
                          (propertize target 'pos pos 'makefile makefile))
                    candidates))))))

    (if (not candidates)
        (message "ターゲットが見つかりませんでした。")
      (let ((map (copy-keymap ivy-minibuffer-map))
            (cands (nreverse candidates)))

        ;; 矢印キーでリアルタイムプレビュー
        (define-key map (kbd "<down>") 'ivy-next-line-and-call)
        (define-key map (kbd "<up>")   'ivy-previous-line-and-call)

        ;; C-c C-c で make 実行
        (define-key map (kbd "C-c C-c")
                    (lambda ()
                      (interactive)
                      (ivy-exit-with-action
                       (lambda (x)
                         (let ((target (cdr x))
                               (mk (get-text-property 0 'makefile (cdr x))))
                           (compile (format "make -C %s %s"
                                            (file-name-directory mk) target)))))))

        (ivy-read "Make target: "
                  cands
                  :keymap map
                  :action (lambda (x)
                            (let ((pos (get-text-property 0 'pos (cdr x)))
                                  (mk (get-text-property 0 'makefile (cdr x))))
                              (find-file mk)
                              (goto-char pos)
                              (recenter)))
                  :unwind (lambda ()
                            (unless (eq ivy-exit 'done)
                              (switch-to-buffer orig-buf)
                              (goto-char orig-point)
                              (recenter)))
                  :caller 'my-make-ivy)))))

(provide 'my-makefile)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved unused-vars)
;; End:
;;; my-makefile.el ends here
