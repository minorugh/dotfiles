;;; 99_my:dired.el --- defun user dired  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(defun my:dotfiles-dir ()
  "Open dotfiles dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/github.com/minorugh/dotfiles/"))

(defun my:root-dir ()
  "Open root dir."
  (interactive)
  (find-file "/"))
(bind-key  "S-<return>" 'my:root-dir)

(defun my:home-dir ()
  "Open home dir."
  (interactive)
  (find-file "~/"))

(defun my:dropbox ()
  "Open dropbox dir."
  (interactive)
  (find-file "~/Dropbox/"))

(defun my:xsrv-dir ()
  "Open Web dir."
  (interactive)
  (find-file "~/Dropbox/xsrv/"))

(defun my:web-dir ()
  "Open Web dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/web/"))

(defun my:emacs-dir ()
  "Open .emacs.d dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/dotfiles/.emacs.d/"))

(defun my:gh-dir ()
  "Open GH dir."
  (interactive)
  (find-file "~/Dropbox/GH/"))

(defun my:inits-dir ()
  "Open inits dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))

(defun my:backup-dir ()
  "Open backup dir."
  (interactive)
  (find-file "~/Dropbox/backup/"))

(defun my:junk-file-dir ()
  "Open junk-file dir."
  (interactive)
  (find-file "~/howm/junk/"))

(defun my:diary ()
  "Open diary dir."
  (interactive)
  (find-file "~/Dropbox/GH/dia/diary.txt")
  (goto-char (point-min)))

(defun my:d_kukai ()
  "Open dkukai minoru_seq file."
  (interactive)
  (find-file "~/Dropbox/GH/d_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

(defun my:w_kukai ()
  "Open wkukai minoru_seq file."
  (interactive)
  (find-file "~/Dropbox/GH/w_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

(defun my:m_kukai ()
  "Open mkukai minoru_sen file."
  (interactive)
  (find-file "~/Dropbox/GH/m_select/tex/mkukai.txt")
  (goto-char (point-min)))

(defun my:swan ()
  "Open swan minoru_sen file."
  (interactive)
  (find-file "~/Dropbox/GH/swan/tex/swan.txt")
  (goto-char (point-min)))

(defun my:teirei ()
  "Open teirei minoru_sen file."
  (interactive)
  (find-file "~/Dropbox/GH/teirei/tex/teirei.txt")
  (goto-char (point-min)))

(defun my:kinnei ()
  "Open kinnei file."
  (interactive)
  (find-file "~/Dropbox/GH/kinnei/kinnei.txt")
  (goto-char (point-min)))

(defun my:otibo ()
  "Open otibo dir."
  (interactive)
  (find-file "~/Dropbox/GH/otibo/tex/otibo.txt")
  (goto-char (point-min)))

(defun my:apvoice ()
  "Open apvoice file."
  (interactive)
  (find-file "~/Dropbox/GH/apvoice/apvoice.txt")
  (goto-char (point-min)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 99_my:dired.el ends here
