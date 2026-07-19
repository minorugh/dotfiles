;;; 04-counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Counsel + Ivy
;; ============================================================

(leaf counsel
  :ensure t
  :doc "Various completion functions using Ivy."
  :bind (("C-:"     . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x g"   . counsel-git)         ; プロジェクト内のファイルを検索
         ("s-a"     . counsel-git-grep)    ; プロジェクト内を全文検索 (ag の代わり)
         ("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("C-,"     . counsel-mark-ring))
  :config
  (setq search-default-mode             nil)
  (setq counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  (setq ivy-format-functions-alist      '((t . my-ivy-format-function-arrow)))

  (defun my-ivy-format-function-arrow (cands)
    "Transform CANDS into a string for the minibuffer with a chevron indicator."
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


;; ============================================================
;;  Amx  (enhanced M-x history)
;; ============================================================

(leaf amx
  :ensure t
  :doc "Alternative M-x with frecency-based history."
  :config
  (setq amx-save-file (locate-user-emacs-file "tmp/amx-items"))
  (setq amx-history-length 20))


;; ============================================================
;;  Swiper
;; ============================================================

(leaf swiper
  :ensure t
  :doc "Isearch with an overview."
  :bind (("C-s" . swiper-region)
         ("s-s" . swiper-thing-at-point))
  :config
  (defun swiper-region ()
    "If region is active, call `swiper-thing-at-point'; otherwise `swiper'."
    (interactive)
    (if (use-region-p)
        (swiper-thing-at-point)
      (swiper))))


;; ============================================================
;;  Migemo  (Japanese incremental search)
;; ============================================================
(leaf migemo
  :ensure t
  :doc "Japanese incremental search through dynamic pattern expansion."
  :hook (after-init-hook . migemo-init)
  :config
  (setq migemo-command          "/usr/bin/cmigemo")
  (setq migemo-options          '("-q" "--emacs"))
  (setq migemo-dictionary       "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary  nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system   'utf-8-unix)

  (defun my-ivy-migemo-re-builder (str)
    "Build a regexp for swiper using migemo for Japanese incremental search."
    (let* ((sep      " \\|\\^\\|\\.\\|\\*")     ; 区切り文字パターン（スペース・^・.・*）
           (chars    (split-string str "" t))    ; STR を1文字ずつ分割
           (splitted (let (result group)         ; 区切り文字で分割し、連続する非区切り文字をグループ化
                       (dolist (c chars)
			 (if (string-match-p sep c)
                             (progn
                               (when group      ; グループが溜まっていれば結合して result に追加
				 (push (apply #'concat (nreverse group)) result)
				 (setq group nil))
                               (push c result)) ; 区切り文字はそのまま result に追加
                           (push c group)))     ; 非区切り文字は group に蓄積
                       (when group              ; 末尾のグループを処理
			 (push (apply #'concat (nreverse group)) result))
                       (nreverse result))))
      (mapconcat                                ; 各セグメントを変換して結合
       (lambda (s)
	 (cond ((string= s " ") ".*?")          ; スペース → 任意文字列
               ((string-match-p sep s) s)        ; 区切り文字 → そのまま
               (t (migemo-get-pattern s))))      ; 日本語 → migemo 変換
       splitted "")))
  ;; swiper のみ migemo re-builder を使用、他は標準の ivy--regex-plus
  (setq ivy-re-builders-alist '((t      . ivy--regex-plus)
				(swiper . my-ivy-migemo-re-builder))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 04-counsel.el ends here
