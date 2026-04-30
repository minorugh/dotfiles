;;; 04-consult.el --- Consult/Vertico config. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Completion framework: vertico / orderless / marginalia / consult
;; Replaces: counsel / ivy-rich / swiper / amx
;;
;; Note: ivy package itself is retained for git-peek.el and my-makefile.el,
;; which call ivy-read directly.  ivy-mode is intentionally NOT enabled.
;;
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;; 1. vertico  — ミニバッファ候補表示
;;; ============================================================
(leaf vertico
  :ensure t
  :doc "Vertical completion UI for the default completion system."
  :hook (after-init-hook . vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 10)
  (setq enable-recursive-minibuffers t)
  (custom-set-faces
   `(vertico-current ((t (:background ,(doom-color 'base4)
				      :foreground ,(doom-color 'fg)
				      :weight bold))))))

;;; ============================================================
;;; 2. orderless — スペース区切りで複数パターンを AND 検索
;;; ============================================================
(leaf orderless
  :ensure t
  :after migemo
  :config
  ;; Orderless用のMigemo変換関数
  (defun orderless-migemo (component)
    (when (featurep 'migemo)
      (migemo-get-pattern component)))
  ;; 入力が 2文字以上のローマ字なら Migemo とみなす設定
  (defun my-orderless-dispatch (pattern _index _total)
    (cond
     ;; 入力が "," で終わる場合は Migemo (例: "nihongo,")
     ((string-suffix-p "," pattern)
      `(orderless-migemo . ,(substring pattern 0 -1)))
     ;; 2文字以上のアルファベットなら Migemo を試す
     ((string-match-p "^[[:ascii:]]\\{2,\\}$" pattern)
      'orderless-migemo)))

  (setq completion-styles '(orderless basic))
  (setq orderless-matching-styles '(orderless-literal orderless-regexp))
  ;; ディスパッチャを登録
  (setq orderless-style-dispatchers '(my-orderless-dispatch)))

;;; ============================================================
;;; 3. marginalia — 候補の横にアノテーション表示 (ivy-rich の代替)
;;; ============================================================
(leaf marginalia
  :ensure t
  :doc "Annotate completion candidates (replaces ivy-rich)."
  :hook (after-init-hook . marginalia-mode))

;;; ============================================================
;;; 4. consult — 強化補完コマンド群 (counsel の代替)
;;; ============================================================
(leaf consult
  :ensure t
  :doc "Consulting completing-read (replaces counsel)."
  :commands (consult-buffer consult-line consult-git-grep
			    consult-mark consult-yank-pop)
  :bind (("C-x C-f" . find-file)
	 ("C-x b"   . consult-project-buffer)
         ("C-x f"   . project-find-file)
	 ("C-x m"   . consult-mark)
	 ("s-z f"   . consult-flymake)
	 ("s-z g"   . consult-goto-line)
	 ("s-z o"   . consult-outline)
	 ("s-z i"   . consult-imenu)	 ;; bound also to F2, see 10-funcsions.el
         ("M-y"     . consult-yank-pop)
         ("C-:"     . consult-buffer)
         ("C-s"     . consult-line-region)
         ("s-a"     . my-consult-grep-dwim)
	 ("s-g"     . my-consult-grep)
         ("s-s"     . consult-line-thing-at-point))
  :config
  ;; consult-grep に migemo を適用
  (setq consult-async-split-style 'semicolon)
  (with-eval-after-load 'migemo
    (defun consult--migemo-regexp-compiler (input type ignore-case)
      (let ((pattern (migemo-get-pattern input)))
	(consult--default-regexp-compiler pattern type ignore-case)))
    (setq consult--regexp-compiler #'consult--migemo-regexp-compiler))

  ;; swiper-thing-at-point 相当
  (defun consult-line-thing-at-point ()
    "Call `consult-line' with the symbol at point as initial input."
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  ;; swiper-region 相当
  (defun consult-line-region ()
    "Call `consult-line' with the selected region as initial input,
or plain consult-line."
    (interactive)
    (consult-line
     (when (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end)))))
  :preface
  ;; Run grep from project root if in git, otherwise from default-directory
  (defun my-consult-grep ()
    "Run `consult-grep' from project root if in git, otherwise `default-directory'."
    (interactive)
    (let ((default-directory
           (or (locate-dominating-file default-directory ".git")
               default-directory)))
      (consult-grep)))

  ;; git管理下なら consult-git-grep、そうでなければ consult-grep を自動判別して実行.
  ;; ~/Dropbox/GH と ~/Dropbox/minorugh.com は .git が外部にあるため専用関数で処理する.
  (defun my-consult-grep-dwim ()
    "Auto-dispatch grep command based on current directory context."
    (interactive)
    (cond
     ((string-prefix-p "/home/minoru/Dropbox/GH" default-directory)
      (my-consult-git-grep-GH))
     ((string-prefix-p "/home/minoru/Dropbox/minorugh.com" default-directory)
      (my-consult-git-grep-minorugh))
     ((locate-dominating-file default-directory ".git")
      (consult-git-grep))
     (t
      (consult-grep)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 04-consult.el ends here
