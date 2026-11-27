;;; 05-counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Counsel + Ivy
;; ============================================================

(leaf counsel
  :ensure t
  :doc "Various completion functions using Ivy."
  :bind (("C-c b"   . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)      ; ファイルを開く
         ("C-x f"   . counsel-find-file)      ; 同上（C-x C-f の短縮）
         ("C-x m"   . counsel-imenu)          ; バッファ内の定義へ移動
         ("C-x C-g" . counsel-git)            ; repo の追跡ファイルを名前で探して開く
         ("C-x g"   . counsel-git-grep)       ; repo 全体の全文検索（追跡ファイルのみ）
         ("s-a"     . my-counsel-rg-here)     ; 今のディレクトリ以下を全文検索（C-u で選択）
         ("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)       ; kill-ring から選んで貼り付ける
         ("C-,"     . counsel-mark-ring))     ; マークした位置へジャンプする
  :config
  (setq counsel-rg-base-command
        '("rg" "--max-columns" "1000" "--with-filename" "--no-heading"
          "--line-number" "--color" "never" "--hidden" "-g" "!.git" "%s"))
  (setq search-default-mode             nil)
  (setq counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  (setq ivy-format-functions-alist      '((t . my-ivy-format-function-arrow)))
  (ivy-configure 'counsel-imenu :update-fn 'auto)
  (add-to-list 'ivy-height-alist '(counsel-imenu . 20))

  (defun my-counsel-pcre-whitespace (re)
    "Replace the Emacs-only `\\s-*' in RE with `\\s*' for PCRE."
    (string-replace "\\s-*" "\\s*" re))
  (advice-add 'counsel--elisp-to-pcre :filter-return #'my-counsel-pcre-whitespace)

  (defun my-counsel-rg-here ()
    "Search the current directory with `counsel-rg'.
With a prefix argument, choose the directory."
    (interactive)
    (counsel-rg nil (unless current-prefix-arg default-directory)))

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
    "Build a regexp for swiper and counsel-rg using migemo for Japanese search."
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
  ;; swiper と counsel-rg に migemo re-builder を使用、他は標準の ivy--regex-plus
  ;; counsel-rg に渡す前の \s-* → \s* の変換は my-counsel-pcre-whitespace が行う
  (setq ivy-re-builders-alist '((t          . ivy--regex-plus)
                                (swiper     . my-ivy-migemo-re-builder)
                                (counsel-rg . my-ivy-migemo-re-builder))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 05-counsel.el ends here
