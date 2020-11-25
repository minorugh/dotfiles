;;; 40_translate.el --- emacs interface to google translate  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

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
		   string))))))


;; Fix error of "Failed to search TKK"
;; (defun google-translate--get-b-d1 ()
;;   "Search TKK."
;;   (list 427110 1469889687))


;; Fix error of "args out of range"
;; ---------------------------------------------------------------
;; https://qiita.com/akicho8/items/cae976cb3286f51e4632
;; ---------------------------------------------------------------
;; --- a/google-translate-core.el
;; +++ b/google-translate-core.el
;; @@ -252,7 +252,7 @@ speech."
;; does matter when translating misspelled word. So instead of
;; translation it is possible to get suggestion."
;; (let ((info (aref json 7)))
;; - (when info
;; + (when (and info (> (length info) 0))
;; (aref info 1))))
;; (defun google-translate-version ()
;; -----------------------------------------------------------------


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 40_translate.el ends here
