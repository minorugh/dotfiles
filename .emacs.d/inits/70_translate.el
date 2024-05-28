;;; 70_translate.el --- Google translate configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf google-translate :ensure t
  :doc "Google translate configurations"
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


(leaf deepl-translate :el-get "minorugh/deepl-translate"
  :doc "Display translation results in mini-buffer & copy to clipboard"
  :url "https://gist.github.com/masatoi/ec90d49331e40983427025f8167d01ee"
  :bind ("C-c d" . deepl-translate)
  :init
  (load "~/Dropbox/backup/emacs/api/deepl-api.el"))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
