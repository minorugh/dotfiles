;;; 70_translate.el --- Deepl translate configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *deepl-api
  :doc "Load auth key from backup"
  :config
  (load "~/Dropbox/backup/deepl/deepl-apikey.el"))


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
  :bind ("C-t" . gt-do-translate)
  :config
  (setq gt-langs '(en ja))
  (setq gt-default-translator
	(gt-translator
	 :taker   (gt-taker :text 'buffer :pick 'paragraph)
	 :engines (list
		   (gt-google-engine)
		   (gt-deepl-engine :key deepl-auth-key :pro nil))
	 :render  (gt-buffer-render))))


(leaf deepl-translate-web
  :doc "Use deepl-transrate on web"
  :commands my:deepl-translate
  :bind (("C-c t" . my:deepl-translate))
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


;;; 70_translate.el ends here
