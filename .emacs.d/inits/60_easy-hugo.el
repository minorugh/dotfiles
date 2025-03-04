;;; 60_easy-hugo.el --- Easy-Hugo configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf easy-hugo
  :ensure t
  :doc "Write blogs made with hugo in evil-mode"
  :url "https://github.com/masasam/emacs-easy-hugo"
  :hook (view-mode-hook . evil-emacs-state)
  :bind ((:easy-hugo-mode-map
	  ("<tab>" . easy-hugo-no-help)
	  ("o" . easy-hugo-open-basedir)
	  ("r" . easy-hugo-rename)
	  ("e" . my:edit-easy-hugo)))
  :init
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
  (leaf request	:ensure t
    :config
    (setq request-storage-directory "~/.emacs.d/tmp/request"))

  (defun easy-hugo-newpost (post-file)
    "Overwrite `easy-hugo-newpost' for evil-mode."
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
       (when evil-mode
	 (evil-insert-state))
       (when (and easy-hugo-org-header
		  (string-equal file-ext "org"))
	 (insert (easy-hugo--org-headers (file-name-base post-file))))
       (goto-char (point-max))
       (save-buffer))))

  (defun my:edit-easy-hugo ()
    "Edit setting file for 'easy-hugo'."
    (interactive)
    (find-file "~/.emacs.d/inits/60_easy-hugo.el")))


;;; 60_easy-hugo.el ends here
