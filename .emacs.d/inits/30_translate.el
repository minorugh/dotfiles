;;; 30_translate.el --- Google translate configurations. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf google-translate
  :ensure t
  :bind (("C-c t" . chromium-translate)
		 ("C-t" . google-translate-auto))
  :config
  (defun google-translate-auto ()
	"Automatically recognize and translate Japanese and English."
	(interactive)
	(if (use-region-p)
		(let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
		  (deactivate-mark)
		  (if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
							string)
			  (google-translate-translate
			   "en" "ja"
			   string)
			(google-translate-translate
			 "ja" "en"
			 string)))
	  (let ((string (read-string "Google Translate: ")))
		(if (string-match
			 (format "\\`[%s]+\\'" "[:ascii:]")
			 string)
			(google-translate-translate
			 "en" "ja"
			 string)
		  (google-translate-translate
		   "ja" "en"
		   string)))))

  ;; Fix error of "Failed to search TKK"
  (defun google-translate--get-b-d1 ()
  	"Search TKK."
  	(list 427110 1469889687))

  (defun chromium-translate ()
	"Open google translate with chromium."
	(interactive)
	(if (use-region-p)
		(let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
		  (deactivate-mark)
		  (if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
							string)
			  (browse-url (concat "https://translate.google.com/?source=gtx#en/ja/"
								  (url-hexify-string string)))
			(browse-url (concat "https://translate.google.com/?source=gtx#ja/en/"
								(url-hexify-string string)))))
	  (let ((string (read-string "Google Translate: ")))
		(if (string-match
			 (format "\\`[%s]+\\'" "[:ascii:]")
			 string)
			(browse-url
			 (concat "https://translate.google.com/?source=gtx#en/ja/" (url-hexify-string string)))
		  (browse-url
		   (concat "https://translate.google.com/?source=gtx#ja/en/" (url-hexify-string string))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 30_translate.el ends here
