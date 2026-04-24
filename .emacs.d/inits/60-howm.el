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
	  (","        . my-howm-create-with-category)
	  ("/"        . my-howm-search-by-category)))
  :init
  (setq howm-use-migemo t)
  (setq howm-migemo-client '((type . cmigemo) (command . "/usr/bin/cmigemo")))
  (setq howm-migemo-client-option '("-q" "-d" "/usr/share/cmigemo/utf-8/migemo-dict"))
  (setq howm-view-title-header "=")
  (setq howm-directory "~/Dropbox/howm")
  (setq howm-file-name-format "%Y/%m/%Y%m%d%H%M.md")
  :config
  (setq howm-view-title-regexp "^= [^=]")
  (setq howm-view-use-grep t)
  (setq howm-view-split-horizontally t)
  (setq howm-view-summary-persistent nil)
  (setq howm-normalizer 'howm-sort-items-by-reverse-date)

  ;; ──────────────────────────────────────────────────────────────
  ;; カテゴリ定義
  ;;   (キー 表示名 挿入文字列 フェイス)
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
    "memo: — purple"
    :group 'howm)

  (defface my-howm-face-idea
    '((((background light)) :foreground "#185FA5" :weight bold)
      (((background dark))  :foreground "#85B7EB" :weight bold))
    "idea: — blue"
    :group 'howm)

  (defface my-howm-face-tech
    '((((background light)) :foreground "#3B6D11" :weight bold)
      (((background dark))  :foreground "#97C459" :weight bold))
    "tech: — green"
    :group 'howm)

  (defface my-howm-face-note
    '((((background light)) :foreground "#0F6E56" :weight bold)
      (((background dark))  :foreground "#5DCAA5" :weight bold))
    "note: — teal"
    :group 'howm)

  (defface my-howm-face-diary
    '((((background light)) :foreground "#854F0B" :weight bold)
      (((background dark))  :foreground "#EF9F27" :weight bold))
    "日記: — amber"
    :group 'howm)

  (defface my-howm-face-creative
    '((((background light)) :foreground "#993556" :weight bold)
      (((background dark))  :foreground "#ED93B1" :weight bold))
    "創作: — pink"
    :group 'howm)

  (defface my-howm-face-church
    '((((background light)) :foreground "#993C1D" :weight bold)
      (((background dark))  :foreground "#F0997B" :weight bold))
    "教会: — coral"
    :group 'howm)

  (defface my-howm-face-garden
    '((((background light)) :foreground "#A32D2D" :weight bold)
      (((background dark))  :foreground "#F09595" :weight bold))
    "園芸: — red"
    :group 'howm)

  ;; ── font-lock キーワード生成 ──────────────────────────────────
  (defun my-howm--font-lock-keywords ()
    "記事バッファ用: 行頭 ^= にマッチ"
    (mapcar (lambda (cat)
	      (let ((pattern (concat "^= " (regexp-quote (nth 2 cat)) ".*$"))
		    (face    (nth 3 cat)))
		`(,pattern (0 ',face t))))
	    my-howm-categories))

  (defun my-howm--font-lock-keywords-summary ()
    "サマリーバッファ用: '| = カテゴリ: ' 形式にマッチ"
    (mapcar (lambda (cat)
	      (let ((pattern (concat "| = " (regexp-quote (nth 2 cat)) ".*$"))
		    (face    (nth 3 cat)))
		`(,pattern (0 ',face t))))
	    my-howm-categories))

  ;; 記事バッファ用
  (setq howm-user-font-lock-keywords (my-howm--font-lock-keywords))

  ;; サマリーバッファ用
  (defun my-howm--apply-summary-font-lock (&rest _)
    "howmサマリーバッファにカテゴリ色を適用する。"
    (when (derived-mode-p 'howm-view-summary-mode)
      (font-lock-add-keywords nil (my-howm--font-lock-keywords-summary) 'set)
      (font-lock-flush)
      (font-lock-ensure)))

  (with-eval-after-load 'howm-view
    (advice-add 'howm-view-summary-mode :after #'my-howm--apply-summary-font-lock))

  ;; ── テンプレート ────────────────────────────────────────────
  (setq howm-template "= %cursor\n%date%file")

  ;; ── プロンプト用フェイス ─────────────────────────────────────
  (defface my-howm-prompt-key
    '((((background light)) :foreground "#222222" :weight bold)
      (((background dark))  :foreground "#FFFFFF" :weight bold))
    "プロンプトのカテゴリ名 — white/black plain"
    :group 'howm)

  (defun my-howm--propertize-key (key-char face-sym label)
    "[KEY] LABEL をfaceつき文字列で返す。"
    (concat
     (propertize (format "[%c]" key-char) 'face face-sym)
     (propertize (format " %s" label)     'face 'my-howm-prompt-key)))

  ;; ── hydra風カテゴリ選択（新規作成）──────────────────────────
  (defun my-howm--category-prompt (prefix)
    "PREFIX付きカテゴリ一覧を色付き1行プロンプトとして返す。"
    (concat
     (propertize prefix 'face 'my-howm-prompt-key)
     (mapconcat (lambda (cat)
		  (my-howm--propertize-key (nth 0 cat) (nth 3 cat) (nth 1 cat)))
		my-howm-categories
		"  ")
     (propertize "  [q] quit : " 'face 'my-howm-prompt-key)))

  (defun my-howm--insert-category (category-str)
    "新規howmファイルを作成し CATEGORY-STR をタイトル行に挿入する。"
    (howm-create 0 nil)
    (delete-other-windows)
    (goto-char (point-min))
    (search-forward "= " nil t)
    (insert category-str)
    (evil-insert-state))

  (defun my-howm-create-with-category ()
    "hydra風プロンプトでカテゴリを1キー選択してhowmメモを新規作成する。
q で中断してリスト画面に戻る。"
    (interactive)
    (let* ((prompt (my-howm--category-prompt "New "))
	   (keys   (cons ?q (mapcar #'car my-howm-categories)))
	   (key    (read-char-choice prompt keys)))
      (if (eq key ?q)
	  (howm-list-all)
	(when-let* ((cat (assq key my-howm-categories))
		    (str (nth 2 cat)))
	  (my-howm--insert-category str)))))

  ;; ── カテゴリ検索（ivy一覧から選択してgrep）──────────────────
  (defun my-howm-search-by-category ()
    "ivyでカテゴリを選択し、そのカテゴリのhowmメモを一覧表示する。"
    (interactive)
    (ivy-read "Search category: "
	      (mapcar (lambda (cat)
			(propertize (nth 1 cat) 'face (nth 3 cat)))
		      my-howm-categories)
	      :require-match t
	      :action (lambda (selected)
		        ;; face propertiesを除いた純粋な表示名を取得
		        (let* ((name (substring-no-properties selected))
			       (cat  (cl-find name my-howm-categories
					      :key #'cadr :test #'string=))
			       (str  (nth 2 cat)))
			  (howm-search (concat "= " str) nil)))))

  ;; ── ゴミ箱へ移動 ────────────────────────────────────────────
  (defvar my-howm-trash-dir
    "~/src/github.com/minorugh/dotfiles/.emacs.d/tmp/trash"
    "howmメモの移動先ゴミ箱ディレクトリ。")

  (defun my-howm-move-to-trash ()
    "サマリーバッファのカーソル行メモをゴミ箱へ移動する。
確認プロンプトあり。削除後はカーソル位置を維持する。"
    (interactive)
    (let* ((item  (riffle-summary-current-item))
	   (file  (car item))
	   (fname (when file (file-name-nondirectory file)))
	   (line  (line-number-at-pos)))
      (if (not file)
	  (message "対象ファイルが見つかりません")
	(when (yes-or-no-p (format "「%s」to the Trash? " fname))
	  (make-directory (expand-file-name my-howm-trash-dir) t)
	  (let* ((src   (expand-file-name file))
		 (base  (file-name-sans-extension fname))
		 (ext   (file-name-extension fname t))
		 (dest  (expand-file-name
			 (format "%s_%s%s" base (format-time-string "%H%M%S") ext)
			 (expand-file-name my-howm-trash-dir))))
	    (copy-file src dest)
	    (delete-file src))
	  (let ((buf (find-buffer-visiting file)))
	    (when buf (kill-buffer buf)))
	  (howm-list-all)
	  ;; 削除した行の1つ前付近にカーソルを戻す
	  (goto-char (point-min))
	  (forward-line (max 0 (- line 2)))))))

  (define-key howm-view-summary-mode-map (kbd "d") #'my-howm-move-to-trash)
  (define-key howm-view-summary-mode-map (kbd "/") #'my-howm-search-by-category)
  (define-key howm-mode-map (kbd "C-c ,") #'my-howm-create-with-category)
  (define-key howm-mode-map (kbd "C-c /") #'my-howm-search-by-category))


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


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 60-howm.el ends here
