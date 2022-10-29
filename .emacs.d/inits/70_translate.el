;;; 70_translate.el --- Deepl translate configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Include auth-key settings from a separate file
(add-to-list 'load-path "~/Dropbox/backup/emacs")
(require 'deepl-auth)

;; Deepl translate for mini-buffer display and kill-ring-save
(leaf deepl-translate
  :doc "auth-key settings are read from a separate file"
  :el-get minorugh/deepl-translate
  :bind ("C-c t" . deepl-translate))

;; Deepl translation on web page
(leaf my:deeple-translate
  :bind ("C-c C-t" . my:deepl-translate)
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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 70_translate.el ends here
