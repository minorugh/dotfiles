;;; 61-howm-category.el --- Howm category extensions. -*- lexical-binding: t -*-
;;; Commentary:
;; Category-based color, creation, search, and trash for howm.
;; Requires my-howm-categories defined in 60-howm.el.
;;; Code:

;; ── フェイス定義（howmロード前でも定義可）────────────────────
(defface my-howm-face-memo
  '((((background light)) :foreground "#534AB7" :weight bold)
    (((background dark))  :foreground "#AFA9EC" :weight bold))
  "Memo: — purple." :group 'howm)

(defface my-howm-face-idea
  '((((background light)) :foreground "#185FA5" :weight bold)
    (((background dark))  :foreground "#85B7EB" :weight bold))
  "Idea: — blue." :group 'howm)

(defface my-howm-face-tech
  '((((background light)) :foreground "#3B6D11" :weight bold)
    (((background dark))  :foreground "#97C459" :weight bold))
  "Tech: — green." :group 'howm)

(defface my-howm-face-note
  '((((background light)) :foreground "#0F6E56" :weight bold)
    (((background dark))  :foreground "#5DCAA5" :weight bold))
  "Note: — teal." :group 'howm)

(defface my-howm-face-diary
  '((((background light)) :foreground "#854F0B" :weight bold)
    (((background dark))  :foreground "#EF9F27" :weight bold))
  "日記: — amber." :group 'howm)

(defface my-howm-face-creative
  '((((background light)) :foreground "#993556" :weight bold)
    (((background dark))  :foreground "#ED93B1" :weight bold))
  "創作: — pink." :group 'howm)

;; (defface my-howm-face-church
;;   '((((background light)) :foreground "#993C1D" :weight bold)
;;     (((background dark))  :foreground "#F0997B" :weight bold))
;;   "教会: — coral." :group 'howm)

(defface my-howm-face-garden
  '((((background light)) :foreground "#A32D2D" :weight bold)
    (((background dark))  :foreground "#F09595" :weight bold))
  "園芸: — red." :group 'howm)

;; ── howm-mode-hook: howmロード前に登録しておく必要があるため外に置く ──
(add-hook 'howm-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("^\\(= \\)\\(.*\\)$"
                (1 'shadow t)
                (2 (let* ((line (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                          (cat  (cl-find-if
                                 (lambda (c) (string-match (regexp-quote (nth 2 c)) line))
                                 my-howm-categories))
                          (face (when cat (nth 3 cat))))
                     (or face 'default))
                   t)))
             t)
            (font-lock-flush)))

;; ── 以下はhowmロード後に評価（my-howm-categoriesが必要なため）────
(with-eval-after-load 'howm

  ;; ── font-lock ───────────────────────────────────────────────
  (defun my-howm--font-lock-keywords ()
    "Generate font-lock keywords for article buffers matching ^= at line start."
    (mapcar (lambda (cat)
	      (let ((pattern (concat "^= " (regexp-quote (nth 2 cat)) ".*$"))
		    (face    (nth 3 cat)))
		`(,pattern (0 ',face t))))
	    my-howm-categories))

  (defun my-howm--font-lock-keywords-summary ()
    "Generate font-lock keywords for summary buffers matching | = category format."
    (mapcar (lambda (cat)
	      (let ((pattern (concat "| = " (regexp-quote (nth 2 cat)) ".*$"))
		    (face    (nth 3 cat)))
		`(,pattern (0 ',face t))))
	    my-howm-categories))

  ;; 記事バッファ用
  (setq howm-user-font-lock-keywords (my-howm--font-lock-keywords))

  ;; サマリーバッファ用
  (defun my-howm--apply-summary-font-lock (&rest _)
    "Apply category colors to howm summary buffer."
    (when (derived-mode-p 'howm-view-summary-mode)
      (font-lock-add-keywords nil (my-howm--font-lock-keywords-summary) 'set)
      (font-lock-flush)
      (font-lock-ensure)))

  (with-eval-after-load 'howm-view
    (advice-add 'howm-view-summary-mode :after #'my-howm--apply-summary-font-lock))

  ;; ── 新規作成: ivy縦表示カテゴリ選択 ──────────────────────────
  (defun my-howm--insert-category (category-str)
    "Create a new howm file and insert CATEGORY-STR into the title line."
    (howm-create 0 nil)
    (delete-other-windows)
    (goto-char (point-min))
    (search-forward "= " nil t)
    (insert category-str)
    (evil-insert-state))

  (defun my-howm-create-with-category ()
    "Create a new howm memo by selecting a category with ivy."
    (interactive)
    (ivy-read "New memo: "
	      (mapcar (lambda (cat)
			(concat
			 (propertize (nth 1 cat) 'face (nth 3 cat))
			 (propertize (format "  [%c]" (nth 0 cat)) 'face (nth 3 cat))))
		      my-howm-categories)
	      :require-match t
	      :action (lambda (selected)
		        (let* ((raw (substring-no-properties selected))
			       (cat (cl-find raw my-howm-categories
					     :test (lambda (s c)
						     (string-prefix-p (cadr c) s))))
			       (str (nth 2 cat)))
			  (my-howm--insert-category str)))))

  ;; ── カテゴリ検索 ────────────────────────────────────────────
  (defun my-howm-search-by-category ()
    "Search howm memos by selecting a category with ivy."
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

  ;; ── ゴミ箱へ移動 ──────────────────────────────────────────
  (defvar my-howm-trash-dir
    (locate-user-emacs-file "tmp/trash")
    "Directory to move deleted howm memos to.")

  (defun my-howm-move-to-trash ()
    "Move the memo at point in summary buffer to trash.
Prompt for confirmation.  Restore cursor position after deletion."
    (interactive)
    (let* ((item  (riffle-summary-current-item))
	   (file  (car item))
	   (fname (when file (file-name-nondirectory file)))
	   (line  (line-number-at-pos)))
      (if (not file)
	  (message "対象ファイルが見つかりません")
	(when (yes-or-no-p (format "Move 「%s」 to trash? " fname))
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

  ;; ── キーバインド ──────────────────────────────────────────
  (define-key howm-view-summary-mode-map (kbd "d") #'my-howm-move-to-trash)
  (define-key howm-view-summary-mode-map (kbd "/") #'my-howm-search-by-category)
  (define-key howm-mode-map (kbd "C-c ,") #'my-howm-create-with-category)
  (define-key howm-mode-map (kbd "C-c /") #'my-howm-search-by-category))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 61-howm-category.el ends here
