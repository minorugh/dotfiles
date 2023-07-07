;;; 60_easy-hugo.el --- Easy-Hugo configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf easy-hugo
  :doc "Write blogs made with hugo"
  :url "https://github.com/masasam/emacs-easy-hugo"
  :ensure t
  :bind (("C-c C-e" . easy-hugo)
		 (:easy-hugo-mode-map
		  ([tab] . easy-hugo-no-help)
		  ("o" . easy-hugo-open-basedir)
		  ("r" . easy-hugo-rename)
		  ("e" . my:edit-easy-hugo)))
  :init
  ;; Load custom function
  (load-file "~/.emacs.d/template/my:evil-easy-hugo.el")

  ;; Main blog (=blog1)
  (setq easy-hugo-basedir "~/Dropbox/myama.site/snap/")
  (setq easy-hugo-url "https://snap.myama.site")
  (setq easy-hugo-sshdomain "xsrv")
  (setq easy-hugo-root "/home/minorugh/myama.site/public_html/snap/")
  (setq easy-hugo-previewtime "300")

  ;; Customize for my help menu
  (setq easy-hugo-help-line 5
		easy-hugo-help "
  n .. New blog post    r .. Rename file     p .. Preview          g .. Refresh
  d .. Delete post      a .. Search blog ag  P .. Publish clever   G .. GitHub deploy
  c .. Open config      o .. Open base dir   < .. Previous blog    > .. Next bloge
  , .. Prev postdir     . .. Next postdir    ; .. Select blog      v .. Open view mode
  N .. No help [tab]    s .. Sort time       u .. Sort Publish     e .. Edit easy-hugo
"))

(leaf popup :ensure t)
(leaf request :ensure t
  :custom (request-storage-directory . "~/.emacs.d/tmp/request"))

(defun my:edit-easy-hugo ()
  "Edit setting file for 'easy-hugo'."
  (interactive)
  (find-file "~/.emacs.d/inits/60_easy-hugo.el"))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 60_easy-hugo.el ends here
