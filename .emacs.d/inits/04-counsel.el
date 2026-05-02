;;; 04-counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel
  :ensure t
  :doc "Various completion functions using Ivy."
  :hook (after-init-hook . ivy-mode)
  :chord (("df" . counsel-describe-function)
          ("fg" . counsel-describe-variable))
  :bind (("C-:"     . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x g"   . counsel-git)         ;; プロジェクト内のファイルを検索
         ("s-a"     . counsel-git-grep)    ;; プロジェクト内を全文検索 (agの代わり)
         ("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("C-,"     . counsel-mark-ring))
  :config
  (setq search-default-mode        nil)
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
		   (nerd-icons-octicon "nf-oct-chevron_right")
		 "")
	       (propertize " " 'display `(space :align-to 2))
	       (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n")))

(leaf ivy-rich
  :ensure t
  :doc "More friendly display transformer for ivy."
  :hook (after-init-hook . ivy-rich-mode))

(leaf amx
  :ensure t
  :doc "Alternative 'M-x' with extra features."
  :config
  (setq amx-save-file (locate-user-emacs-file "tmp/amx-items"))
  (setq amx-history-length 20))


(leaf swiper
  :ensure t
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
