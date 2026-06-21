;;; 80-easy-hugo.el --- Easy-Hugo configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Easy-Hugo Core
;;; ============================================================

(leaf easy-hugo
  :ensure t
  :doc "Write blogs made with Hugo in evil-mode."
  :url "https://github.com/masasam/emacs-easy-hugo"
  :bind ((:easy-hugo-mode-map
          ("<tab>" . easy-hugo-no-help)
          ("o"     . easy-hugo-open-basedir)
          ("SPC"   . easy-hugo-view)
          ("e"     . my-edit-easy-hugo)))
  :init
  (setq easy-hugo-help-line 4)
  (setq easy-hugo-help
        "n .. New blog post    R .. Rename file     p .. Preview          g .. Refresh
d .. Delete post      a .. Seach blog ag   P .. Publish server   e .. Edit easy-hugo
S .. Sort char        s .. Sort time       < .. Previous blog    > .. Next blog
N .. No help [tab]    . .. Next postdir    c .. Open config      o .. Open base dir
")


;;; ============================================================
;;;  Blog Settings
;;;
;;;  blog1 (main): snap.minorugh.com
;;;  blog2: minorugh.github.io
;;;  blog3–8: minorugh.com サブサイト群
;;; ============================================================

  ;; blog1 (main)
  (setq easy-hugo-basedir    "~/Dropbox/minorugh.com/snap/")
  (setq easy-hugo-url        "https://snap.minorugh.com")
  (setq easy-hugo-sshdomain  "xsrv")
  (setq easy-hugo-root       "/home/minorugh/minorugh.com/public_html/snap/")
  (setq easy-hugo-previewtime "300")

  (setq easy-hugo-bloglist
        '(;; blog2
          ((easy-hugo-basedir . "~/src/github.com/minorugh/minorugh.github.io/")
           (easy-hugo-url     . "https://minorugh.github.io")
           (easy-hugo-postdir . "docs"))
          ;; blog3
          ((easy-hugo-basedir    . "~/Dropbox/minorugh.com/gg/")
           (easy-hugo-url        . "https://gg.minorugh.com")
           (easy-hugo-sshdomain  . "xsrv")
           (easy-hugo-root       . "/home/minorugh/minorugh.com/public_html/gg/"))
          ;; blog4
          ((easy-hugo-basedir    . "~/Dropbox/minorugh.com/blog/")
           (easy-hugo-url        . "https://blog.minorugh.com")
           (easy-hugo-sshdomain  . "xsrv")
           (easy-hugo-root       . "/home/minorugh/minorugh.com/public_html/blog/"))
          ;; blog5
          ((easy-hugo-basedir    . "~/Dropbox/minorugh.com/es/")
           (easy-hugo-url        . "https://es.minorugh.com")
           (easy-hugo-sshdomain  . "xsrv")
           (easy-hugo-root       . "/home/minorugh/minorugh.com/public_html/es/"))
          ;; blog6
          ((easy-hugo-basedir    . "~/Dropbox/minorugh.com/bible/")
           (easy-hugo-url        . "https://bible.minorugh.com")
           (easy-hugo-sshdomain  . "xsrv")
           (easy-hugo-root       . "/home/minorugh/minorugh.com/public_html/bible/"))
          ;; blog7
          ((easy-hugo-basedir    . "~/Dropbox/minorugh.com/tube/")
           (easy-hugo-url        . "https://tube.minorugh.com")
           (easy-hugo-sshdomain  . "xsrv")
           (easy-hugo-root       . "/home/minorugh/minorugh.com/public_html/tube/"))
          ;; blog8
          ((easy-hugo-basedir    . "~/Dropbox/minorugh.com/ryo/")
           (easy-hugo-url        . "https://ryo.minorugh.com")
           (easy-hugo-sshdomain  . "xsrv")
           (easy-hugo-root       . "/home/minorugh/minorugh.com/public_html/ryo/"))))


;;; ============================================================
;;;  Helper Commands
;;; ============================================================

  :config
  (defun my-edit-easy-hugo ()
    "Open the easy-hugo configuration file for editing."
    (interactive)
    (find-file "~/.emacs.d/inits/80-easy-hugo.el"))

  (defun my-easy-hugo-newpost-after (&rest _)
  "After creating a new post, switch to Emacs state and
move to end of buffer."
    (when (bound-and-true-p evil-mode)
      (evil-emacs-state)
      (goto-char (point-max))
      (save-buffer)))

  (advice-add 'easy-hugo-newpost :after #'my-easy-hugo-newpost-after))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved callargs)
;; End:
;;; 80-easy-hugo.el ends here
