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


(leaf deepl-translate
  :vc (:url "https://github.com/minorugh/deepl-translate")
  :doc "Display translation results in mini-buffer & copy to clipboard"
  :url "https://gist.github.com/masatoi/ec90d49331e40983427025f8167d01ee"
  :bind ("C-c d" . deepl-translate)
  :init
  (load "~/Dropbox/backup/emacs/api/deepl-key.el"))

(leaf go-translate
  :ensure t
  :bind ("C-c t" . gt-do-translate)
  :config
  (setq gt-langs '(en ja))
  (defvar gt-default-translator nil)
  (load "~/Dropbox/backup/emacs/api/deepl-key.el"))

;; Deepl translation on web page
(leaf my:deeple-traqslate
  :bind ("C-c D" . my:deepl-translate)
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

(leaf *my:deepl-translate
  :doc "Use deepl-transrate on web"
  :commands my:deepl-translate
  :bind (("C-x T" . my:deepl-translate))
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
    (run-at-time 0.1 nil 'deactivate-mark)
    (browse-url
     (concat
      "https://www.deepl.com/translator#en/ja/"
      (url-hexify-string string)))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
