;;; 50-howm.el --- Howm mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Howm Core
;;; ============================================================

(leaf howm
  :ensure t
  :doc "Wiki-like note-taking tool."
  :url "https://howm.osdn.jp"
  :commands (howm-list-all my-howm-create-with-category)
  :hook (emacs-startup-hook . howm-mode)
  :chord (("@@" . howm-list-all)
          (",," . my-howm-create-with-category))
  :bind ((:howm-view-summary-mode-map
          ([backtab]  . howm-view-summary-previous-section)
          ("<return>" . howm-view-summary-open)
          (","        . my-howm-create-with-category)
          ("/"        . my-howm-search-by-category)))
  :init
  (setq howm-use-migemo       t)
  (setq howm-migemo-client    '((type . cmigemo) (command . "/usr/bin/cmigemo")))
  (setq howm-migemo-client-option '("-q" "-d" "/usr/share/cmigemo/utf-8/migemo-dict"))
  (setq howm-view-title-header "=")
  (setq howm-directory         "~/Dropbox/howm")
  (setq howm-file-name-format  "%Y/%m/%Y%m%d%H%M.md")
  :config
  (setq howm-view-title-regexp        "^= [^=]")
  (setq howm-view-use-grep            t)
  (setq howm-view-split-horizontally  t)
  (setq howm-view-summary-persistent  nil)
  (setq howm-normalizer               'howm-sort-items-by-reverse-date)


;;; ============================================================
;;;  カテゴリ定義
;;;
;;;  通常カテゴリ: (キー 表示名 挿入文字列 フェイス)
;;;  特殊エントリ: (キー 表示名 フェイス 呼び出す関数)
;;;               検索メニューには表示されない。外部関数に委譲。
;;; ============================================================

  (defvar my-howm-categories
    '((?m " memo"  "memo: " my-howm-face-memo)
      (?i " idea"  "idea: " my-howm-face-idea)
      (?t " tech"  "tech: " my-howm-face-tech)
      (?d " 日記"  "日記: " my-howm-face-diary)
      (?g " 園芸"  "園芸: " my-howm-face-garden))
    "howm 通常カテゴリ定義。検索・作成メニュー両方で使用する。")

  (defvar my-howm-special-entries
    '((?c " code"  my-howm-face-church  my-junk-new)
      (?p " 創作"  my-howm-face-creative  my-haiku-note-post)
      (?n " 推敲"  my-howm-face-note      my-haiku-note))
    "作成メニュー専用の特殊エントリ。動作は外部関数に委譲する。")


;;; ============================================================
;;;  フェイス定義
;;; ============================================================

  (defface my-howm-face-memo
    '((((background light)) :foreground "#534AB7" :weight bold)
      (((background dark))  :foreground "#AFA9EC" :weight bold))
    "memo: — purple" :group 'howm)

  (defface my-howm-face-idea
    '((((background light)) :foreground "#185FA5" :weight bold)
      (((background dark))  :foreground "#85B7EB" :weight bold))
    "idea: — blue" :group 'howm)

  (defface my-howm-face-tech
    '((((background light)) :foreground "#3B6D11" :weight bold)
      (((background dark))  :foreground "#97C459" :weight bold))
    "tech: — green" :group 'howm)

  (defface my-howm-face-note
    '((((background light)) :foreground "#0F6E56" :weight bold)
      (((background dark))  :foreground "#5DCAA5" :weight bold))
    "note: — teal" :group 'howm)

  (defface my-howm-face-diary
    '((((background light)) :foreground "#854F0B" :weight bold)
      (((background dark))  :foreground "#EF9F27" :weight bold))
    "日記: — amber" :group 'howm)

  (defface my-howm-face-creative
    '((((background light)) :foreground "#993556" :weight bold)
      (((background dark))  :foreground "#ED93B1" :weight bold))
    "創作: — pink" :group 'howm)

  (defface my-howm-face-church
    '((((background light)) :foreground "#993C1D" :weight bold)
      (((background dark))  :foreground "#F0997B" :weight bold))
    "code: — orange" :group 'howm)

  (defface my-howm-face-garden
    '((((background light)) :foreground "#A32D2D" :weight bold)
      (((background dark))  :foreground "#F09595" :weight bold))
    "園芸: — red" :group 'howm)


;;; ============================================================
;;;  Font-lock  (カテゴリ色付け)
;;; ============================================================

  (defun my-howm--font-lock-keywords ()
    "記事バッファ用: 行頭 ^= にマッチするキーワードリストを生成する."
    (mapcar (lambda (cat)
              (let ((pattern (concat "^= " (regexp-quote (nth 2 cat)) ".*$"))
                    (face    (nth 3 cat)))
                `(,pattern (0 ',face t))))
            my-howm-categories))

  (defun my-howm--font-lock-keywords-summary ()
    "サマリーバッファ用: '| = カテゴリ: ' 形式にマッチするキーワードリストを生成する."
    (mapcar (lambda (cat)
              (let ((pattern (concat "| = " (regexp-quote (nth 2 cat)) ".*$"))
                    (face    (nth 3 cat)))
                `(,pattern (0 ',face t))))
            my-howm-categories))

  ;; 記事バッファへの適用
  (setq howm-user-font-lock-keywords (my-howm--font-lock-keywords))

  ;; サマリーバッファへの適用
  (defun my-howm--apply-summary-font-lock (&rest _)
    "howm サマリーバッファにカテゴリ色を適用する."
    (when (derived-mode-p 'howm-view-summary-mode)
      (font-lock-add-keywords nil (my-howm--font-lock-keywords-summary) 'set)
      (font-lock-flush)
      (font-lock-ensure)))

  (with-eval-after-load 'howm-view
    (advice-add 'howm-view-summary-mode :after #'my-howm--apply-summary-font-lock))


;;; ============================================================
;;;  Junk (Perl スクラッチ)
;;; ============================================================

  (defun my-junk-new ()
    "タイムスタンプ付き Perl スクラッチファイルを開く."
    (interactive)
    (let* ((file   (format-time-string "~/Dropbox/howm/junk/%Y%m%d%H%M.pl"))
           (is-new (not (file-exists-p file))))
      (find-file file)
      (when is-new
        (insert "#!/usr/bin/perl\nuse strict;\nuse warnings;\n\n")
        (when evil-mode (evil-insert-state)))))


;;; ============================================================
;;;  新規メモ作成
;;; ============================================================

  (setq howm-template "= %cursor\n%date%file")

  (defun my-howm--insert-category (category-str)
    "新規 howm ファイルを作成し CATEGORY-STR をタイトル行に挿入する."
    (howm-create 0 nil)
    (delete-other-windows)
    (goto-char (point-min))
    (search-forward "= " nil t)
    (insert category-str)
    (evil-insert-state))

  (defun my-howm-create-with-category ()
    "カテゴリを縦リストで表示し 1 キーで選択して howm メモを新規作成する.
通常カテゴリは新規 howm ファイルを作成。特殊エントリは外部関数を呼び出す。"
    (interactive)
    (let* ((fmt-cat
            (lambda (cat face-idx)
              (concat "  "
                      (propertize (format "[%c]" (nth 0 cat)) 'face (nth face-idx cat))
                      " "
                      (propertize (nth 1 cat) 'face (nth face-idx cat)))))
           (all-entries (append my-howm-categories my-howm-special-entries))
           (keys (mapcar #'car all-entries))
           (prompt
            (concat
             "New memo:\n"
             (mapconcat (lambda (cat) (funcall fmt-cat cat 3)) my-howm-categories "\n")
             "\n"
             (mapconcat (lambda (cat) (funcall fmt-cat cat 2)) my-howm-special-entries "\n")
             "\n> "))
           (key (read-char-choice prompt keys)))
      (cond
       ((assq key my-howm-categories)
        (my-howm--insert-category (nth 2 (assq key my-howm-categories))))
       ((assq key my-howm-special-entries)
        (funcall (nth 3 (assq key my-howm-special-entries)))
        (delete-other-windows)
        (message "")))))


;;; ============================================================
;;;  カテゴリ検索
;;; ============================================================

  (defun my-howm-search-by-category ()
    "ivy でカテゴリを選択し、そのカテゴリの howm メモを一覧表示する."
    (interactive)
    (ivy-read "Search category: "
              (mapcar (lambda (cat)
                        (propertize (nth 1 cat) 'face (nth 3 cat)))
                      my-howm-categories)
              :require-match t
              :action (lambda (selected)
                        (let* ((raw (substring-no-properties selected))
                               (cat (cl-find raw my-howm-categories
                                             :test (lambda (s c)
                                                     (string-prefix-p (cadr c) s))))
                               (str (nth 2 cat)))
                          (howm-search (concat "= " str) nil)))))


;;; ============================================================
;;;  ゴミ箱へ移動
;;; ============================================================

  (defvar my-howm-trash-dir
    (locate-user-emacs-file "tmp/trash")
    "howm メモの移動先ゴミ箱ディレクトリ.")

  (defun my-howm-move-to-trash ()
    "サマリーバッファのカーソル行メモをゴミ箱へ移動する.
確認プロンプトあり。削除後はカーソル位置を維持する。"
    (interactive)
    (let* ((item  (riffle-summary-current-item))
           (file  (car item))
           (fname (when file (file-name-nondirectory file)))
           (line  (line-number-at-pos)))
      (if (not file)
          (message "対象ファイルが見つかりません")
        (when (yes-or-no-p (format "「%s」to the trash? " fname))
          (make-directory (expand-file-name my-howm-trash-dir) t)
          (let* ((src  (expand-file-name file))
                 (base (file-name-sans-extension fname))
                 (ext  (file-name-extension fname t))
                 (dest (expand-file-name
                        (format "%s_%s%s" base (format-time-string "%H%M%S") ext)
                        (expand-file-name my-howm-trash-dir))))
            (copy-file src dest)
            (delete-file src))
          (let ((buf (find-buffer-visiting file)))
            (when buf (kill-buffer buf)))
          (howm-list-all)
          (goto-char (point-min))
          (forward-line (max 0 (- line 2)))))))


;;; ============================================================
;;;  キーバインド
;;; ============================================================

  (define-key howm-view-summary-mode-map (kbd "d") #'my-howm-move-to-trash)
  (define-key howm-view-summary-mode-map (kbd "/") #'my-howm-search-by-category)
  (define-key howm-mode-map (kbd "C-c ,") #'my-howm-create-with-category)
  (define-key howm-mode-map (kbd "C-c /") #'my-howm-search-by-category)


;;; ============================================================
;;;  NOTE: howm-mode-hook は (leaf howm ...) の外に配置する
;;;
;;;  :config 内で add-hook するとバイトコンパイル時に howm がまだ
;;;  ロードされておらず、font-lock-add-keywords が効かないため。
;;; ============================================================

  :preface
  (add-hook 'howm-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("^\\(= \\)\\(.*\\)$"
                  (1 'shadow t)
                  (2 (let* ((line (buffer-substring (line-beginning-position)
                                                    (line-end-position)))
                            (cat  (cl-find-if
                                   (lambda (c)
                                     (string-match (regexp-quote (nth 2 c)) line))
                                   my-howm-categories))
                            (face (when cat (nth 3 cat))))
                       (or face 'default))
                     t)))
               t)
              (font-lock-flush))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-howm.el ends here
