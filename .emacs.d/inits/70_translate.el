;;; 70_translate.el --- Deepl translate configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Deepl translate with go-translate
(leaf go-translate
  :ensure t
  :bind ("C-t" . gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-default-translator
		(gts-translator
		 :picker (gts-noprompt-picker)
		 :engines (list
				   (gts-deepl-engine :auth-key "7f4efb81-0c38-589c-2da0-97ae1e7f2ff3:fx" :pro nil)
				   (gts-google-engine))
 		 :render (gts-buffer-render))))


;; Deepl translation with Google-Chrome
(leaf my:deeple-traqslate
  :bind ("C-c t" . my:deepl-translate)
  :preface
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
	(run-at-time 0.1 nil 'deactivate-mark)
	(browse-url
	 (concat
      "https://www.deepl.com/translator#en/ja/"
      (url-hexify-string string)
      ))))


;; Deepl translation appears in minibuffer.
;; Also, the same content is copied to the clipboard
(leaf deepl-translate
  :el-get minorugh/deepl-translate
  :bind ("C-c C-d" . deepl-translate)
  :custom (deepl-auth-key . "7f4efb81-0c38-589c-2da0-97ae1e7f2ff3:fx"))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
