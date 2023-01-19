;;; 60_easy-hugo.el --- Easy-Hugo configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf easy-hugo
  :ensure t
  :bind (("C-c C-e" . easy-hugo)
		 (:easy-hugo-mode-map
		  ([tab] . easy-hugo-no-help)
		  ("n" . my:evil-easy-hugo-newpost)
		  ("o" . easy-hugo-open-basedir)
		  ("r" . easy-hugo-rename)
		  ("e" . my:edit-easy-hugo)))
  :config
  ;; Open newpost with evil-isert-state
  (defun my:evil-easy-hugo-newpost (post-file)
	(interactive (list (read-from-minibuffer
						"Filename: "
						`(,easy-hugo-default-ext . 1) nil nil nil)))
	(easy-hugo-with-env
	 (let ((filename (expand-file-name post-file easy-hugo-postdir)))
	   (when (file-exists-p (file-truename filename))
		 (error "%s already exists!" filename))
	   (call-process easy-hugo-bin nil "*hugo*" t "new"
					 (file-relative-name filename
										 (expand-file-name "content" easy-hugo-basedir)))
	   (when (get-buffer "*hugo*")
		 (kill-buffer "*hugo*"))
	   (find-file filename)
	   (evil-insert-state)
	   (goto-char (point-max))
	   (save-buffer))))
  :init
  ;; Main blog (=blog1)
  (setq easy-hugo-basedir "~/Dropbox/minorugh.com/snap/")
  (setq easy-hugo-url "https://snap.minorugh.com")
  (setq easy-hugo-sshdomain "xsrv")
  (setq easy-hugo-root "/home/minorugh/minorugh.com/public_html/snap/")
  (setq easy-hugo-previewtime "300")
  ;; Bloglist
  (setq easy-hugo-bloglist
		'(;; blog2 setting
		  ((easy-hugo-basedir . "~/src/github.com/minorugh/emacs.d/hugo/")
		   (easy-hugo-url . "https://minorugh.github.io/emacs.d")
		   (easy-hugo-postdir . "content/startup")
		   (easy-hugo-preview-url . "http://localhost:1313/emacs.d/"))
		  ;; blog3 setting
		  ((easy-hugo-basedir . "~/src/github.com/minorugh/minorugh.github.io/")
		   (easy-hugo-url . "https://minorugh.github.io")
		   (easy-hugo-postdir . "content/posts"))
		  ;; blog4 setting
		  ((easy-hugo-basedir . "~/Dropbox/GH/gg/")
		   (easy-hugo-url . "https://gg.gospel-haiku.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/gg/"))
		  ;; blog5 setting
		  ((easy-hugo-basedir . "~/Dropbox/GH/blog/")
		   (easy-hugo-url . "https://blog.gospel-haiku.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/blog/"))
		  ;; blog6 setting
		  ((easy-hugo-basedir . "~/Dropbox/GH/es/")
		   (easy-hugo-url . "https://es.gospel-haiku.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/es/"))
		  ;; blog7 setting
		  ((easy-hugo-basedir . "~/Dropbox/minorugh.com/bible/")
		   (easy-hugo-url . "https://bible.minorugh.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/bible/"))
		  ;; blog8 setting
		  ((easy-hugo-basedir . "~/Dropbox/minorugh.com/tube/")
		   (easy-hugo-url . "https://tube.minorugh.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/tube/"))
		  ;; blog9 setting
		  ((easy-hugo-basedir . "~/Dropbox/minorugh.com/ryo/")
		   (easy-hugo-url . "https://ryo.minorugh.com")
		   (easy-hugo-sshdomain . "xsrv")
		   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/ryo/"))))

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
(leaf request
  :ensure t
  :custom (request-storage-directory . "~/.emacs.d/tmp/request"))

(defun my:edit-easy-hugo ()
  "Edit setting file for 'easy-hugo'."
  (interactive)
  (find-file "~/.emacs.d/inits/60_easy-hugo.el"))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 60_easy-hugo.el ends here
