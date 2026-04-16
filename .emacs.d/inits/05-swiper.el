;;; 05-swiper.el --- Swiper configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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
;;; 05-swiper.el ends here
