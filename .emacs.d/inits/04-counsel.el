;;; 04-counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counsel configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf counsel :ensure t
  :doc "Various completion functions using Ivy."
  :hook (after-init-hook . ivy-mode)
  :bind (("C-:"     . counsel-switch-buffer)
	 ("C-x C-b" . counsel-switch-buffer)
	 ("M-x"     . counsel-M-x)
	 ("M-y"     . counsel-yank-pop)
	 ("M-:"     . counsel-buffer-or-recentf)
	 ("C-,"     . counsel-mark-ring)
	 ("s-a"     . counsel-ag)
	 ("C-x a"   . counsel-linux-app)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf))
  :config
  (setq search-default-mode          nil)
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-use-selectable-prompt    t)
  (setq enable-recursive-minibuffers t)
  (setq counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  (setq ivy-format-functions-alist '((t . my-ivy-format-function-arrow)))

  (defun my-ivy-format-function-arrow (cands)
    "Transform into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (display-graphic-p)
		   "")
	       (propertize " " 'display `(space :align-to 2))
	       (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n"))

  (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
    "Fast full-text search. see https://takaxp.github.io/init.html#org29c7b6b7"
    (apply f (or initial-input
		 (and (not (thing-at-point-looking-at "^\\*+"))
		      (ivy-thing-at-point)))
	   (unless current-prefix-arg
	     (or initial-directory default-directory))
	   extra-ag-args ag-prompt caller))
  (advice-add 'counsel-ag :around #'ad:counsel-ag)
  ;; Make search trigger even with 2 characters
  (add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))
  (ivy-add-actions
   'counsel-ag
   '(("r" my-counsel-ag-in-dir "search in directory")))
  (defun my-counsel-ag-in-dir (_arg)
    "Search again with new root directory."
    (let ((current-prefix-arg '(4)))
      (counsel-ag ivy-text nil ""))))

(leaf avy :ensure t
  :doc "Jump to arbitrary positions quickly."
  :bind ("C-r" . avy-goto-word-1))

(leaf ivy-rich :ensure t
  :doc "More friendly display transformer for ivy."
  :hook (after-init-hook . ivy-rich-mode))

(leaf amx :ensure t
  :doc "Alternative 'M-x' with extra features."
  :config
  (setq amx-save-file (locate-user-emacs-file "tmp/amx-items"))
  (setq amx-history-length 20))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swiper configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf swiper :ensure t
  :doc "Isearch with an overview."
  :bind (
	 ("C-s" . swiper-region)
	 ("s-s" . swiper-thing-at-point))
  :config
  (defun swiper-region ()
    "If region is selected, `swiper-thing-at-point'.
If the region isn't selected, `swiper'."
    (interactive)
    (if (use-region-p)
	(swiper-thing-at-point)
      (swiper))))

(leaf migemo :ensure t
  :doc "Japanese incremental search through dynamic pattern expansion."
  :hook (after-init-hook . migemo-init)
  :config
  (setq migemo-command "/usr/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))


(with-eval-after-load 'swiper
  (defun my-ivy-migemo-re-builder (str)
    "Build a regexp for swiper using migemo for Japanese incremental search.
  STR is split into segments by separators (space, ^, ., *).
  Each segment is converted via `migemo-get-pattern', separators are kept as-is.
  Space is treated as a wildcard '.*?' for flexible matching."
    (let* ((sep " \\|\\^\\|\\.\\|\\*")      ;; 区切り文字パターン（スペース・^・.・*）
           (chars (split-string str "" t))  ;; STR を1文字ずつに分割
           (splitted (let (result group)    ;; 区切り文字で分割し、連続する非区切り文字をグループ
                       (dolist (c chars)
                         (if (string-match-p sep c)
                             (progn
                               (when group ;; グループが溜まっていれば結合してresultに追加
                                 (push (apply #'concat (nreverse group)) result)
                                 (setq group nil))
                               (push c result))       ;; 区切り文字はそのままresultに追加
                           (push c group)))           ;; 非区切り文字はgroupに蓄積
                       (when group                    ;; 末尾のグループを処理
                         (push (apply #'concat (nreverse group)) result))
                       (nreverse result))))
      (mapconcat (lambda (s)                          ;; 各セグメントを変換して結合
                   (cond ((string= s " ") ".*?")      ;; スペース → 任意文字列
                         ((string-match-p sep s) s)   ;; 区切り文字 → そのまま
                         (t (migemo-get-pattern s)))) ;; 日本語 → migemo変換
                 splitted "")))
  ;; swiper のみ migemo re-builder を使用、他は標準の ivy--regex-plus
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
                                (swiper . my-ivy-migemo-re-builder))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 04-counsel.el ends here
