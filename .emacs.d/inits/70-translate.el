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
  :config (load (locate-user-emacs-file "elisp/deepl-api.el.gpg")))

(leaf deepl-translate-web
  :ensure nil
  :doc "Use deepl-translate on web."
  :commands my-deepl-translate
  :bind ("C-c w" . my-deepl-translate)
  :preface
  (require 'url-util)
  (defun my-deepl-translate (&optional string)
    (interactive)
    (setq string
          (cond ((stringp string) string)
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (run-at-time 0.1 nil #'deactivate-mark)
    (browse-url
     (concat
      "https://www.deepl.com/translator#en/ja/"
      (url-hexify-string string)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 70-translate.el ends here
