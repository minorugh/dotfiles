;;; 80-ivy-describe.el --- My Ivy-based describe tools -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Replicate the usability of Counsel-describe-function using only Ivy
;;
;;; Code:
;; (setq debug-on-error t)

;; (leaf ivy
;;   :ensure t
;;   :hook (after-init-hook . ivy-mode))

(leaf my-ivy-commands
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
                :require-match t)))

  (defun my-make-integrated ()
  "Makefileターゲット選択。
Enter    : その場所へジャンプして終了
↑↓      : リアルタイムプレビュー
C-c C-c  : その場で make 実行 (Ivyは閉じる)"
  (interactive)
  (require 'ivy)
  (let ((candidates nil)
        (orig-point (point))
        (map (copy-keymap ivy-minibuffer-map)))

    ;; リアルタイムプレビュー用
    (define-key map (kbd "<down>") 'ivy-next-line-and-call)
    (define-key map (kbd "<up>")   'ivy-previous-line-and-call)

    ;; 実行用のショートカット (C-c C-c)
    (define-key map (kbd "C-c C-c")
                (lambda ()
                  (interactive)
                  (let ((current (ivy-state-current ivy-last)))
                    (ivy-exit-with-action
                     (lambda (x)
                       (compile (concat "make " (cdr x))))))))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
        (let* ((target (match-string 1))
               (desc   (match-string 2))
               (pos    (match-beginning 1))
               (target-fmt (propertize (format "%-20s" target) 'face 'font-lock-function-name-face))
               (desc-fmt (propertize desc 'face 'font-lock-comment-face)))
          ;; cdr にターゲット名、プロパティに位置を保持
          (push (cons (concat target-fmt " " desc-fmt)
                      (propertize target 'pos pos))
                candidates))))

    (if (not candidates)
        (message "ターゲットが見つかりませんでした。")
      (ivy-read "Makefile Targets: "
                (nreverse candidates)
                :keymap map
                :action (lambda (x)
                          (let ((pos (get-text-property 0 'pos (cdr x))))
                            (goto-char pos)
                            (recenter)))
                :unwind (lambda () (unless (eq ivy-exit 'done) (goto-char orig-point) (recenter)))
                :caller 'my-make-ivy-integrated)))))

  ;; (defun my-targets-preview ()
  ;;   "Makefile内のターゲットを色分けし、↑↓キーの移動でリアルタイムにバッファも動かす。"
  ;;   (interactive)
  ;;   (require 'ivy)
  ;;   (let ((candidates nil)
  ;;         (orig-point (point))
  ;;         (map (copy-keymap ivy-minibuffer-map)))
  ;;     (define-key map (kbd "<down>") 'ivy-next-line-and-call)
  ;;     (define-key map (kbd "<up>")   'ivy-previous-line-and-call)
  ;;     (save-excursion
  ;; 	(goto-char (point-min))
  ;; 	(while (re-search-forward "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
  ;;         (let* ((target (match-string 1))
  ;; 		 (desc   (match-string 2))
  ;; 		 (pos    (match-beginning 1))
  ;; 		 (target-fmt (propertize (format "%-20s" target) 'face 'font-lock-function-name-face))
  ;; 		 (desc-fmt (propertize desc 'face 'font-lock-comment-face)))
  ;;           (push (cons (concat target-fmt " " desc-fmt) pos) candidates))))
  ;;     (if (not candidates)
  ;;         (message "ターゲットが見つかりませんでした。")
  ;; 	(ivy-read "Targets: "
  ;;                 (nreverse candidates)
  ;;                 :keymap map
  ;;                 :action (lambda (x) (goto-char (cdr x)) (recenter))
  ;;                 :unwind (lambda () (unless (eq ivy-exit 'done) (goto-char orig-point) (recenter)))
  ;;                 :caller 'my-makefile-ivy-targets-preview)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 80-ivy-describe.el ends here
