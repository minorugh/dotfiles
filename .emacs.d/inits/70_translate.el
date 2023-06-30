;;; 70_translate.el --- Deepl translate configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf deepl-translate
  :doc "Display translation results in mini-buffer & copy to clipboard"
  :url "https://gist.github.com/masatoi/ec90d49331e40983427025f8167d01ee"
  :el-get minorugh/deepl-translate
  :bind ("C-c C-t" . deepl-translate)
  :init
  (load-file "~/Dropbox/backup/emacs/deepl-api.el"))


(leaf go-translate
  :doc "Display Deepl and Google Translat eresults in other buffer"
  :url "https://github.com/lorniu/go-translate"
  :ensure t
  :bind ("C-c t" . gts-do-translate)
  :config
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-default-translator
		(gts-translator
		 :picker
		 (gts-noprompt-picker)
		 :engines (list
				   (gts-google-engine)
				   (gts-deepl-engine
					:auth-key (format deepl-auth-key) :pro nil))
		 :render
		 (gts-buffer-render))))


(leaf *cus-deeple-translate
  :doc "Deepl translation in web page"
  :bind ("C-c C-d" . my:deepl-translate)
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
	  (url-hexify-string string)))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
