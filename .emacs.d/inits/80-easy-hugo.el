;;; 60-easy-hugo.el --- Easy-Hugo configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf easy-hugo
  :ensure t
  :doc "Write blogs made with hugo in evil-mode"
  :url "https://github.com/masasam/emacs-easy-hugo"
  :require  my:evil-hugo  ;; Load user defines
  :bind ((:easy-hugo-mode-map
	  ("<tab>" . easy-hugo-no-help)
	  ("o"     . easy-hugo-open-basedir)
	  ("SPC"   . easy-hugo-view)
	  ("e"     . my:edit-easy-hugo)))
  :init
  ;; Customize for my help menu
  (setq easy-hugo-help-line 4)
  (setq easy-hugo-help
	"n .. New blog post    R .. Rename file     p .. Preview          g .. Refresh
d .. Delete post      a .. Seach blog ag   P .. Publish server   e .. Edit easy-hugo
S .. Sort char        s .. Sort time       < .. Previous blog    > .. Next blog
N .. No help [tab]    . .. Next postdir    c .. Open config      o .. Open base dir
")
  ;; Main blog (=blog1)
  (setq easy-hugo-basedir "~/Dropbox/minorugh.com/snap/")
  (setq easy-hugo-url "https://snap.minorugh.com")
  (setq easy-hugo-sshdomain "xsrv")
  (setq easy-hugo-root "/home/minorugh/minorugh.com/public_html/snap/")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-bloglist
	'(;; blog2 setting
	  ((easy-hugo-basedir . "~/src/github.com/minorugh/minorugh.github.io/")
	   (easy-hugo-url . "https://minorugh.github.io")
	   (easy-hugo-postdir . "docs"))
	  ;; blog3 setting
	  ((easy-hugo-basedir . "~/Dropbox/GH/gg/")
	   (easy-hugo-url . "https://gg.gospel-haiku.com")
	   (easy-hugo-sshdomain . "xsrv")
	   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/gg/"))
	  ;; blog4 setting
	  ((easy-hugo-basedir . "~/Dropbox/GH/blog/")
	   (easy-hugo-url . "https://blog.gospel-haiku.com")
	   (easy-hugo-sshdomain . "xsrv")
	   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/blog/"))
	  ;; blog5 setting
	  ((easy-hugo-basedir . "~/Dropbox/GH/es/")
	   (easy-hugo-url . "https://es.gospel-haiku.com")
	   (easy-hugo-sshdomain . "xsrv")
	   (easy-hugo-root . "/home/minorugh/gospel-haiku.com/public_html/es/"))
	  ;; blog6 setting
	  ((easy-hugo-basedir . "~/Dropbox/minorugh.com/bible/")
	   (easy-hugo-url . "https://bible.minorugh.com")
	   (easy-hugo-sshdomain . "xsrv")
	   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/bible/"))
	  ;; blog7 setting
	  ((easy-hugo-basedir . "~/Dropbox/minorugh.com/tube/")
	   (easy-hugo-url . "https://tube.minorugh.com")
	   (easy-hugo-sshdomain . "xsrv")
	   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/tube/"))
	  ;; blog8 setting
	  ((easy-hugo-basedir . "~/Dropbox/minorugh.com/ryo/")
	   (easy-hugo-url . "https://ryo.minorugh.com")
	   (easy-hugo-sshdomain . "xsrv")
	   (easy-hugo-root . "/home/minorugh/minorugh.com/public_html/ryo/"))))
  :config
  (defun my:edit-easy-hugo ()
    "Edit setting file for `easy-hugo'."
    (interactive)
    ( find-file-noselect "~/.emacs.d/inits/80-easy-hugo.el"))
  :preface
  (leaf request	:ensure t
    :config
    (setq request-storage-directory "~/.emacs.d/tmp/request")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 60-easy-hugo.el ends here
