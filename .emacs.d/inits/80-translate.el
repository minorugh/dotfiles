;;; 80-translate.el --- DeepL / Google translate key bindings.  -*- lexical-binding: t -*-
;;; Commentary:
;; DeepL API 翻訳の実体は ~/.emacs.d/elisp/my-deepl-translate.el に分離し、
;; ここでは autoload 宣言とキーバインドのみを行う。
;; Google 翻訳は軽量なので分離せずこのファイルに直書きする。
;;; Code:

;; ============================================================
;;  DeepL API  (ミニバッファ翻訳 + クリップボードコピー)
;;  実体: ~/.emacs.d/elisp/my-deepl-translate.el
;; ============================================================

(load "~/.env_source/tokens/deepl-api.el")

(autoload 'deepl-translate "my-deepl-translate" nil t)
(autoload 'deepl-ej "my-deepl-translate" nil t)
(autoload 'deepl-je "my-deepl-translate" nil t)

(global-set-key (kbd "C-c d") #'deepl-translate)


;; ============================================================
;;  Google Web  (ブラウザで Google-translate を開く)
;; ============================================================

(defun my-google-translate (&optional string)
  "Translate region STRING  or sentence at point using Google Translate."
  (interactive)
  (let* ((text (or string
                   (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     (thing-at-point 'sentence t)))))
    (unless (and text (not (string-empty-p text)))
      (user-error "No text found"))

    (let* ((is-ja (string-match-p "[ぁ-んァ-ン一-龯]" text))
           (src   (if is-ja "ja" "en"))
           (tgt   (if is-ja "en" "ja"))
           (url   (format
                   "https://translate.google.com/?sl=%s&tl=%s&text=%s&op=translate"
                   src
                   tgt
                   (url-hexify-string (string-trim text))))) ; URLパラメータ形式

      (when (use-region-p)
        (deactivate-mark))

      (browse-url url))))

(global-set-key (kbd "C-c w") #'my-google-translate)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-translate.el ends here
