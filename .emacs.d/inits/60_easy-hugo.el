;;; 60_easy-hugo.el --- Easy-Hugo configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf easy-hugo
  :doc "Write blogs made with hugo"
  :url "https://github.com/masasam/emacs-easy-hugo"
  :ensure t
  :config
  (with-eval-after-load "easy-hugo"
	(global-set-key (kbd "C-c C-e") 'easy-hugo)
	(define-key easy-hugo-mode-map (kbd "<tab>") 'easy-hugo-no-help)
	(define-key easy-hugo-mode-map (kbd "o") 'easy-hugo-open-basedir)
	(define-key easy-hugo-mode-map (kbd "r") 'easy-hugo-rename)
	(define-key easy-hugo-mode-map (kbd "e") 'my:edit-easy-hugo))
  :init
  ;; Load custom function
  (require 'my:evil-easy-hugo)

  ;; Main blog (=blog1)
  (setq easy-hugo-basedir "~/Dropbox/minorugh.com/snap/")
  (setq easy-hugo-url "https://snap.minorugh.com")
  (setq easy-hugo-sshdomain "xsrv")
  (setq easy-hugo-root "/home/minorugh/minorugh.com/public_html/snap/")
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
(leaf request
  :ensure t
  :config
  (setq request-storage-directory "~/.emacs.d/tmp/request"))

(defun my:edit-easy-hugo ()
  "Edit setting file for 'easy-hugo'."
  (interactive)
  (find-file "~/.emacs.d/inits/60_easy-hugo.el"))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 60_easy-hugo.el ends here
