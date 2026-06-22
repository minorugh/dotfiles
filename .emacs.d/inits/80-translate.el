;;; 80-translate.el --- DeepL translate configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;; 2026-03-10 DeepL API 仕様変更に対応
;;   認証方式を POST ボディ (auth_key) から Authorization ヘッダーに変更。
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  DeepL API  (ミニバッファ翻訳 + クリップボードコピー)
;; ============================================================

(leaf deepl-translate
  :doc "Translation in minibuffer & copy result to clipboard."
  :vc (:url "https://github.com/minorugh/deepl-translate")
  :bind ("C-c d" . deepl-translate)
  :config
  (load (locate-user-emacs-file "~/.env_source/tokens/deepl-api.el")))


;; ============================================================
;;  DeepL Web  (ブラウザで DeepL を開く)
;; ============================================================

(leaf deepl-translate-web
  :doc "Open DeepL Translator in a web browser with selected text."
  :bind ("C-c w" . my-deepl-translate)
  :preface
  (require 'url-util)

  (defun my-deepl-translate (&optional string)
    "Translate region or sentence at point using DeepL web interface.
Auto-detects Japanese ↔ English direction."
    (interactive)
    (let* ((text (or string
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'sentence t))))
           (is-ja (string-match-p "[ぁ-んァ-ン一-龯]" text))
           (src   (if is-ja "ja" "en"))
           (tgt   (if is-ja "en" "ja"))
           ;; "://deepl.com" 形式にすることでフリー版インターフェースに誘導
           (url   (format "https://://deepl.com#%s/%s/%s"
                          src tgt (url-hexify-string (string-trim text)))))
      (when (use-region-p) (deactivate-mark))
      (browse-url url))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-translate.el ends here
