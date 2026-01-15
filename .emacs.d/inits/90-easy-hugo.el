;;; 90-easy-hugo.el --- Easy-Hugo configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf easy-hugo
  :ensure t
  :doc "Write blogs made with hugo in evil-mode"
  :url "https://github.com/masasam/emacs-easy-hugo"
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
  (defun easy-hugo-newpost (post-file)
    "Create a new post with hugo.
POST-FILE needs to have an extension '.md', '.org', '.ad', '.rst',
'.mmark', or '.html'."
    (interactive (list (read-from-minibuffer
			"Filename: "
			`(,easy-hugo-default-ext . 1) nil nil nil)))
    (easy-hugo-with-env
     (let ((filename (expand-file-name post-file easy-hugo-postdir))
           (file-ext (file-name-extension post-file)))
       (when (not (member file-ext easy-hugo--formats))
	 (error "Please enter .%s, .%s, .%s, .%s, .%s, or .%s file name"
		easy-hugo-markdown-extension
		easy-hugo--org-extension
		easy-hugo-asciidoc-extension
		easy-hugo--rst-extension
		easy-hugo--mmark-extension
		easy-hugo-html-extension))
       (when (file-exists-p (file-truename filename))
	 (error "%s already exists!" filename))
       (call-process
	easy-hugo-bin nil "*hugo*" t "new"
	(file-relative-name filename
                            (expand-file-name "content" easy-hugo-basedir)))
       (when (get-buffer "*hugo*")
	 (kill-buffer "*hugo*"))
       (find-file filename)
       ;; Customize from here
       (when evil-mode
	 (evil-emacs-state)
       ;; so far
       (goto-char (point-max))
       (save-buffer))))

  (defun my:edit-easy-hugo ()
    "Edit setting file for `easy-hugo'."
    (interactive)
    (find-file "~/.emacs.d/inits/90-easy-hugo.el"))
  :preface
  (leaf request	:ensure t
    :config
    (setq request-storage-directory "~/.emacs.d/tmp/request")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 90-easy-hugo.el ends here
