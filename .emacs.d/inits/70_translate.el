;;; 70_translate.el --- Deepl translate configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Suppress flycheck recognition errors
(eval-when-compile (leaf-keywords-init))

(leaf deepl-translate
  :doc "Display translation results in mini-buffer & copy to clipboard"
  :url "https://gist.github.com/masatoi/ec90d49331e40983427025f8167d01ee"
  :el-get "minorugh/deepl-translate"
  :bind ("C-c C-d" . deepl-translate))


(leaf go-translate
  :doc "Display Deepl and Google Translat eresults in other buffer"
  :url "https://github.com/lorniu/go-translate"
  :ensure	t
  :defun ((gts-translator)
		  (gts-noprompt-picker)
		  (gts-google-engine)
		  (gts-deepl-engine)
		  (ts-buffer-render)
		  (gts-buffer-render))
  :bind ("C-c t" . gts-do-translate)
  :init
  (load "~/Dropbox/backup/emacs/api/deepl-api")
  :config
  (setq gts-translate-list '(("en" "ja") ("ja" "en")))
  (setq gts-default-translator
		(gts-translator
		 :picker  (gts-noprompt-picker)
		 :engines (list
				   (gts-google-engine)
				   (gts-deepl-engine
					:auth-key (format deepl-auth-key) :pro nil))
		 :render  (gts-buffer-render))))


(leaf *chromium-deepl-translate
  :doc "Deepl translation in web page"
  :bind ("C-c C-t" . chromium-deepl-translate)
  :init
  (defun chromium-deepl-translate (&optional string)
	"Hoge."
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
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 70_translate.el ends here
