;;; 70-translate.el --- Deepl translate configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;; 2026-03-10 DeepL API 仕様変更に対応
;; 認証方式を POST ボディ (auth_key) から Authorization ヘッダーに変更
;;; Code:
;; (setq debug-on-error t)

(leaf deepl-translate
  :vc (:url "https://github.com/minorugh/deepl-translate")
  :doc "Translation in mini-buffer & copy to clipboard."
  :url "https://gist.github.com/masatoi/" ; 原作者 gist（2026-03-10 DeepL仕様変更に伴い Authorization ヘッダー方式に修正済み)
  :bind ("C-c d" . deepl-translate)
  :init
  ;; Load deepl-auth-key
  (load "~/Dropbox/backup/deepl/deepl-api.el"))

(leaf google-translate :ensure t
  :doc "Emacs interface to Google Translate."
  :defun google-translate-translate
  :hook (after-init-hook . (lambda () (require 'google-translate)))
  :bind ("C-c t" . google-translate-auto)
  :config
  (defun google-translate-auto ()
    "Automatically recognize and translate Japanese and English."
    (interactive)
    (let ((string (if (use-region-p)
		      (prog1
			  (buffer-substring-no-properties (region-beginning) (region-end))
			(deactivate-mark))
		    (read-string "Google Translate: "))))
      (if (string-match (format "\\`[%s]+\\'" "[:ascii:]") string)
	  (google-translate-translate "en" "ja" string)
	(google-translate-translate "ja" "en" string))))

  ;; Workaround for "Failed to search TKK" error
  ;; see https://github.com/atykhonov/google-translate/issues/137
  (defun google-translate--get-b-d1 ()
    (list 427110 1469889687)))

(leaf deepl-translate-web
  :ensure nil
  :doc "Use deepl-translate on web."
  :commands my:deepl-translate
  :bind ("C-c w" . my:deepl-translate)
  :preface
  (require 'url-util)
  (defun my:deepl-translate (&optional string)
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
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 70-translate.el ends here
