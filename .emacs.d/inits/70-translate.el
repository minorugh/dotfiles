;;; 70-translate.el --- Deepl translate configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf my:deepl-api
  :doc "Load auth key from Dropbox/backup"
  :config
  (load "~/Dropbox/backup/deepl/deepl-api.el"))


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
  :defun gt-deepl-engine gt-taker gt-translator gt-buffer-render gt-google-engine
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
  :bind (("C-c w" . my:deepl-translate))
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
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 70-translate.el ends here
