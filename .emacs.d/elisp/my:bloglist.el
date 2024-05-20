;;; my:bloglist.el --- Bloglist for easy-hugo.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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
	((easy-hugo-basedir . "~/Dropbox/myama.site/bible/")
	 (easy-hugo-url . "https://bible.myama.site")
	 (easy-hugo-sshdomain . "xsrv")
	 (easy-hugo-root . "/home/minorugh/myama.site/public_html/bible/"))
	;; blog8 setting
	((easy-hugo-basedir . "~/Dropbox/myama.site/tube/")
	 (easy-hugo-url . "https://tube.myama.site")
	 (easy-hugo-sshdomain . "xsrv")
	 (easy-hugo-root . "/home/minorugh/myama.site/public_html/tube/"))
	;; blog9 setting
	((easy-hugo-basedir . "~/Dropbox/myama.site/ryo/")
	 (easy-hugo-url . "https://ryo.myama.site")
	 (easy-hugo-sshdomain . "xsrv")
	 (easy-hugo-root . "/home/minorugh/myama.site/public_html/ryo/"))))


(provide 'my:bloglist)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; my:bloglist.el ends here
