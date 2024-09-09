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


(leaf *deepl-api
  :doc "Load Deeepl-auth-key from external file (for security)"
  :config
  (load "~/Dropbox/backup/emacs/api/deepl-key.el"))

(leaf deepl-translate
  :vc (:url "https://github.com/minorugh/deepl-translate")
  :doc "Translation in mini-buffer & copy to clipboard"
  :url "https://gist.github.com/masatoi/"
  :bind ("C-c d" . deepl-translate)
  :config
  (setq deepl-auth-key 'deepl-auth-key))

(leaf go-translate :ensure t
  :doc "Translation framework on Emacs"
  :url "https://github.com/lorniu/go-translate"
  :bind ("C-c t" . gt-do-translate)
  :config
  (setq gt-langs '(en ja))
  (setq gt-default-translator
	(gt-translator
	 :taker   (gt-taker :text 'buffer :pick 'paragraph)
	 :engines (list
		   (gt-google-engine)
		   (gt-deepl-engine :key deepl-auth-key :pro nil))
	 :render  (gt-buffer-render))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
