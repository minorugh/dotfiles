;;; 70_translate.el --- Google translate configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google translate configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf google-translate
  :ensure t
  :bind ("C-t" . google-translate-auto)
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

  (defun google-translate--get-b-d1 ()
	"Fix error of `Failed to search TKK`."
	(list 427110 1469889687)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
