;;; 70_translate.el --- Deepl translate configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Load deepl api key
(load-file "~/Dropbox/backup/emacs/deepl-api.el")


;; Display translation results on minibuffer
(leaf deepl-translate
  :el-get minorugh/deepl-translate
  :bind ("C-c C-t" . deepl-translate))


;; Display Deepl and Google Translate results in other buffer
(leaf go-translate
  :ensure t
  :bind ("C-t" . gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-default-translator
		(gts-translator
		 :picker
		 (gts-noprompt-picker)
		 :engines (list
				   (gts-google-engine)
				   ;; (gts-bing-engine)
				   (gts-deepl-engine
					:auth-key (format deepl-auth-key) :pro nil))
		 :render (gts-buffer-render))))


;; Deepl translation in web page
(leaf my:deeple-translate
  :bind ("C-c t" . my:deepl-translate)
  :init
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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
