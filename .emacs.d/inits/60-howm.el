;;; 60-howm.el --- Howm mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf howm
  :ensure t
  :doc "Wiki-like note-taking tool."
  :url "https://howm.osdn.jp"
  :hook (emacs-startup-hook . howm-mode)
  :bind ((:howm-view-summary-mode-map
	  ([backtab]  . howm-view-summary-previous-section)
	  ("<return>" . howm-view-summary-open)
	  (","        . my-howm-create-with-category)))
  :init
  (setq howm-use-migemo t)
  (setq howm-migemo-client '((type . cmigemo) (command . "/usr/bin/cmigemo")))
  (setq howm-migemo-client-option '("-q" "-d" "/usr/share/cmigemo/utf-8/migemo-dict"))
  (setq howm-view-title-header "=")            ;; # → = に変更
  (setq howm-directory "~/Dropbox/howm")
  (setq howm-file-name-format "%Y/%m/%Y%m%d%H%M.md")
  :config
  (setq howm-view-title-regexp "^= [^=]")      ;; # → = に変更
  (setq howm-view-use-grep t)
  (setq howm-view-split-horizontally t)
  (setq howm-view-summary-persistent nil)
  (setq howm-normalizer 'howm-sort-items-by-reverse-date)

  ;; ──────────────────────────────────────────────────────────────
  ;; カテゴリ定義
  ;;   (キー 表示名 挿入文字列 フェイス)
  ;;   ここを編集するだけでカテゴリ追加・削除・色変更が完結する
  ;; ──────────────────────────────────────────────────────────────
  (defvar my-howm-categories
    '((?m "memo"  "memo: " my-howm-face-memo)
      (?i "idea"  "idea: " my-howm-face-idea)
      (?t "tech"  "tech: " my-howm-face-tech)
      (?n "note"  "note: " my-howm-face-note)
      (?d "日記"  "日記: " my-howm-face-diary)
      (?w "創作"  "創作: " my-howm-face-creative)
      (?c "教会"  "教会: " my-howm-face-church)
      (?e "園芸"  "園芸: " my-howm-face-garden))
    "howmカテゴリ定義。各要素: (キー文字 表示名 挿入文字列 フェイス名)")

  ;; ── フェイス定義 ─────────────────────────────────────────────
  (defface my-howm-face-memo
    '((((background light)) :foreground "#534AB7" :weight bold)
      (((background dark))  :foreground "#AFA9EC" :weight bold))
    "memo: — purple")

  (defface my-howm-face-idea
    '((((background light)) :foreground "#185FA5" :weight bold)
      (((background dark))  :foreground "#85B7EB" :weight bold))
    "idea: — blue")

  (defface my-howm-face-tech
    '((((background light)) :foreground "#3B6D11" :weight bold)
      (((background dark))  :foreground "#97C459" :weight bold))
    "tech: — green")

  (defface my-howm-face-note
    '((((background light)) :foreground "#0F6E56" :weight bold)
      (((background dark))  :foreground "#5DCAA5" :weight bold))
    "note: — teal")

  (defface my-howm-face-diary
    '((((background light)) :foreground "#854F0B" :weight bold)
      (((background dark))  :foreground "#EF9F27" :weight bold))
    "日記: — amber")

  (defface my-howm-face-creative
    '((((background light)) :foreground "#993556" :weight bold)
      (((background dark))  :foreground "#ED93B1" :weight bold))
    "創作: — pink")

  (defface my-howm-face-church
    '((((background light)) :foreground "#993C1D" :weight bold)
      (((background dark))  :foreground "#F0997B" :weight bold))
    "教会: — coral")

  (defface my-howm-face-garden
    '((((background light)) :foreground "#A32D2D" :weight bold)
      (((background dark))  :foreground "#F09595" :weight bold))
    "園芸: — red")

  ;; ── font-lock: カテゴリ定義から自動生成 ─────────────────────
  ;; "= カテゴリ: ..." 行全体をカテゴリ色で上書き（markdown干渉なし）
  (setq howm-user-font-lock-keywords
	(mapcar (lambda (cat)
		  (let ((pattern (concat "^= " (regexp-quote (nth 2 cat)) ".*$"))
			(face    (nth 3 cat)))
		    `(,pattern (0 ',face t))))
		my-howm-categories))

  ;; ── テンプレート ────────────────────────────────────────────
  (setq howm-template "= %cursor\n%date%file")  ;; # → = に変更

  ;; ── プロンプト用フェイス ─────────────────────────────────────
  (defface my-howm-prompt-key
    '((((background light)) :foreground "#222222" :weight bold)
      (((background dark))  :foreground "#FFFFFF" :weight bold))
    "プロンプトのカテゴリ名 — white/black plain")

  (defun my-howm--propertize-key (key-char face-sym label)
    "[KEY] LABEL をfaceつき文字列で返す。"
    (concat
     (propertize (format "[%c]" key-char) 'face face-sym)
     (propertize (format " %s" label)     'face 'my-howm-prompt-key)))

  ;; ── hydra風カテゴリ選択 ──────────────────────────────────────
  (defun my-howm--category-prompt ()
    "カテゴリ一覧を色付き1行プロンプトとして返す。"
    (concat
     (propertize "Category " 'face 'my-howm-prompt-key)
     (mapconcat (lambda (cat)
		  (my-howm--propertize-key (nth 0 cat) (nth 3 cat) (nth 1 cat)))
		my-howm-categories
		"  ")
     (propertize " : " 'face 'my-howm-prompt-key)))

  (defun my-howm--insert-category (category-str)
    "新規howmファイルを作成し CATEGORY-STR をタイトル行に挿入する。"
    (howm-create 0 nil)
    (delete-other-windows)
    (goto-char (point-min))
    (search-forward "= " nil t)
    (insert category-str)
    (evil-insert-state))

  (defun my-howm-create-with-category ()
    "hydra風プロンプトでカテゴリを1キー選択してhowmメモを新規作成する。"
    (interactive)
    (let* ((prompt (my-howm--category-prompt))
	   (keys   (mapcar #'car my-howm-categories))
	   (key    (read-char-choice prompt keys)))
      (when-let* ((cat (assq key my-howm-categories))
		  (str (nth 2 cat)))
	(my-howm--insert-category str))))

  (define-key howm-mode-map (kbd "C-c ,") #'my-howm-create-with-category))


(leaf migemo
  :ensure t
  :doc "Japanese incremental search through dynamic pattern expansion."
  :hook (after-init-hook . migemo-init)
  :config
  (setq migemo-command "/usr/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix))

(defun my-howm-to-simplenote ()
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (buffer-string)))
         ;; 1行目をタイトル扱い（Simplenoteで見やすい）
         (title (car (split-string text "\n"))))
    (kill-new text)
    (browse-url "https://app.simplenote.com")
    (message "Copied to clipboard: %s" title)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 60-howm.el ends here
