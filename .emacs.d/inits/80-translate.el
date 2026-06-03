;;; 70-translate.el --- Deepl translate configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;; 2026-03-10 DeepL API 仕様変更に対応
;; 認証方式を POST ボディ (auth_key) から Authorization ヘッダーに変更
;;; Code:
;; (setq debug-on-error t)

(leaf deepl-translate
  :doc "Translation in mini-buffer & copy to clipboard."
  :vc (:url "https://github.com/minorugh/deepl-translate")
  :bind ("C-c d" . deepl-translate)
  :config (load (locate-user-emacs-file "~/.env_source/tokens/deepl-api.el")))


(leaf deepl-translate-web
  :doc "Use DeepL Translator on a web browser."
  :bind ("C-c w" . my-deepl-translate)
  :preface
  (require 'url-util)
  (defun my-deepl-translate (&optional string)
    "Translate at DeepL. Regions, or for sentences in the current location."
    (interactive)
    (let* ((text (or string
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'sentence t))))
           (is-ja (string-match-p "[ぁ-んァ-ン一-龯]" text))
           (src   (if is-ja "ja" "en"))
           (tgt   (if is-ja "en" "ja"))
	   ;; If you include the language path, such as "://deepl.com"
           ;; The interface of the free version is more likely to be preferred.
           (url   (format "https://://deepl.com#%s/%s/%s"
                          src tgt (url-hexify-string (string-trim text)))))
      (when (use-region-p) (deactivate-mark))
      (browse-url url))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 70-translate.el ends here
