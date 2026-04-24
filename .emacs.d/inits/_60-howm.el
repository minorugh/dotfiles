;;; 60-howm.el --- Howm mode basic configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf howm
  :ensure t
  :doc "Wiki-like note-taking tool."
  :url "https://howm.osdn.jp"
  :hook (emacs-startup-hook . howm-mode)
  :chord (",," . my-howm-create-with-category)
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
  (setq howm-template "= %cursor\n%date%file")

  ;; ──────────────────────────────────────────────────────────────
  ;; カテゴリ定義
  ;;   (キー 表示名 挿入文字列 フェイス)
  ;;   ここを編集するだけでカテゴリ追加・削除・色変更が完結する
  ;; ──────────────────────────────────────────────────────────────
  (defvar my-howm-categories
    '((?m " memo"  "memo: " my-howm-face-memo)
      (?i " idea"  "idea: " my-howm-face-idea)
      (?t " tech"  "tech: " my-howm-face-tech)
      (?n " note"  "note: " my-howm-face-note)
      (?d " 日記"  "日記: " my-howm-face-diary)
      (?w " 創作"  "創作: " my-howm-face-creative)
      ;; (?c "  教会"  "教会: " my-howm-face-church)
      (?e " 園芸"  "園芸: " my-howm-face-garden))
    "howmカテゴリ定義。各要素: (キー文字 表示名 挿入文字列 フェイス名)"))

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
