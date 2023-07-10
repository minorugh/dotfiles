;;; my:evil-easy-hugo.el --- User custom configurations for evil-mode. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Rewrite easy-hugo-newpost configuration for `evil-mode'.
;; If you create a new file,
;; make sure you open it with `evil-insert-state'.
;;
;;; Code:
;; (setq debug-on-error t)

;; -----------------------------------------------------------------------------------------
;;
;; Custom newpost function for Evil
;;
;; -----------------------------------------------------------------------------------------

(defun easy-hugo-newpost (post-file)
  "Create a new post with hugo.
POST-FILE needs to have and extension '.md' or '.org' or '.ad' or '.rst' or '.mmark' or '.html'."
  (interactive (list (read-from-minibuffer
					  "Filename: "
					  `(,easy-hugo-default-ext . 1) nil nil nil)))
  (easy-hugo-with-env
   (let ((filename (expand-file-name post-file easy-hugo-postdir))
		 (file-ext (file-name-extension post-file)))
     (when (not (member file-ext easy-hugo--formats))
	   (error "Please enter .%s or .org or .%s or .rst or .mmark or .%s file name"
			  easy-hugo-markdown-extension
			  easy-hugo-asciidoc-extension
			  easy-hugo-html-extension))
     (when (file-exists-p (file-truename filename))
	   (error "%s already exists!" filename))
     (if (null easy-hugo-org-header)
		 (call-process easy-hugo-bin nil "*hugo*" t "new"
					   (file-relative-name filename
										   (expand-file-name "content" easy-hugo-basedir)))
	   (progn
		 (if (or (string-equal file-ext easy-hugo-markdown-extension)
				 (string-equal file-ext easy-hugo-asciidoc-extension)
				 (string-equal file-ext "rst")
				 (string-equal file-ext "mmark")
				 (string-equal file-ext easy-hugo-html-extension))
			 (call-process easy-hugo-bin nil "*hugo*" t "new"
						   (file-relative-name filename
											   (expand-file-name "content" easy-hugo-basedir))))))
     (when (get-buffer "*hugo*")
	   (kill-buffer "*hugo*"))
     (find-file filename)
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;; Stert inserting commands for evil
	 (when evil-mode (evil-insert-state))
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (when (and easy-hugo-org-header
				(string-equal file-ext "org"))
	   (insert (easy-hugo--org-headers (file-name-base post-file))))
     (goto-char (point-max))
     (save-buffer))))


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


;; Local Variables:
;; End:
;;; my:evil-easy-hugo.el ends here
