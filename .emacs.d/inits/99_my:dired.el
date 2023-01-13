;;; 99_my:dired.el --- User dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my dired configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:dotfiles-dir ()
  "Open dotfile dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/github.com/minorugh/dotfiles/"))

(defun backup-makefile ()
  "Open backup maikefile."
  (interactive)
  (find-file "~/Dropbox/makefile"))

(defun my:zshrc ()
  "Open zshrc."
  (interactive)
  (find-file "~/.zshrc"))

(defun my:root-dir ()
  "Open root dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "/"))

(defun my:xsrv-dir ()
  "Open xsrv dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/xsrv"))

(defun my:github-dir ()
  "Open root dir."
  (interactive)i
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/github.com/minorugh/"))

(defun my:scr-dir ()
  "Open scr dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/"))

(defun my:github-dir ()
  "Open scr dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/github.com/minorugh/"))

(defun my:junk-dir ()
  "Open junk file dir."
  (interactive)
  (find-file "~/Dropbox/howm/junk/"))

(defun my:home-dir ()
  "Open hoge dir."
  (interactive)
  (find-file "~/"))

(defun my:dropbox ()
  "Open dropbox dir."
  (interactive)
  (find-file "~/Dropbox/"))

(defun my:emacs-dir ()
  "Open `.emacs.d' dir."
  (interactive)
  (setq dired-listing-switches "-lgGhFA")
  (find-file "~/src/github.com/minorugh/dotfiles/.emacs.d/"))

(defun my:gh-dir ()
  "Open GH dir."
  (interactive)
  (find-file "~/Dropbox/GH/"))

(defun my:inits-dir ()
  "Open inits dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))

(defun my:org-dir ()
  "Open org dir."
  (interactive)
  (find-file "~/Dropbox/howm/org/"))

(defun my:docker-compose ()
  "Open docker dir."
  (interactive)
  (find-file "~/src/xsrv/docker-compose"))

(defun my:open-capture ()
  "Open `org-caupture' file."
  (interactive)
  (find-file "~/Dropbox/howm/org/capture.org")
  (goto-char (point-min)))

(defun my:diary ()
  "Open diary file."
  (interactive)
  (find-file "~/Dropbox/GH/dia/diary.txt")
  (goto-char (point-min)))

(defun my:d_kukai ()
  "Open d_select file."
  (interactive)
  (find-file "~/Dropbox/GH/d_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

(defun my:w_kukai ()
  "Open w_select file."
  (interactive)
  (find-file "~/Dropbox/GH/w_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

(defun my:m_kukai ()
  "Open m_select file."
  (interactive)
  (find-file "~/Dropbox/GH/m_select/tex/mkukai.txt")
  (goto-char (point-min)))

(defun my:swan ()
  "Open s_select file."
  (interactive)
  (find-file "~/Dropbox/GH/swan/tex/swan.txt")
  (goto-char (point-min)))

(defun my:tpost ()
  "Open tselect file."
  (interactive)
  (find-file "~/Dropbox/GH/tselect/tex/minoru_sen.txt")
  (goto-char (point-min)))

(defun my:tselext ()
  "Open tselect file."
  (interactive)
  (find-file "~/Dropbox/GH/tselext/select.txt")
  (goto-char (point-min)))

(defun my:dselext ()
  "Open tselect file."
  (interactive)
  (find-file "~/Dropbox/GH/d_selext/select.txt")
  (goto-char (point-min)))

(defun my:teirei ()
  "Open teirei file."
  (interactive)
  (find-file "~/Dropbox/GH/teirei/tex/teirei.txt")
  (goto-char (point-min)))

(defun my:kinnei ()
  "Open kinnei file."
  (interactive)
  (find-file "~/Dropbox/GH/kinnei/kinnei.txt")
  (goto-char (point-min)))

(defun my:kinnei-draft ()
  "Open kinnei draft file."
  (interactive)
  (find-file "~/Dropbox/GH/kinnei/draft.txt"))

(defun my:year ()
  "Open year file."
  (interactive)
  (find-file (format-time-string "~/Dropbox/GH/year/%Y.txt"))
  (evil-insert-state)
  (goto-char (point-max))
  (forward-line -10))

(defun my:year-draft ()
  "Open year draft file."
  (interactive)
  (find-file "~/Dropbox/GH/year/draft.txt")
  (evil-insert-state)
  (goto-char (point-min))
  (forward-line))

(defun my:tpdia ()
  "Open year draft file."
  (interactive)
  (find-file "~/Dropbox/GH/tpdia/dia.txt")
  (evil-insert-state)
  (goto-char (point-min))
  (forward-line))

(defun my:apvoice ()
  "Open apvoice file."
  (interactive)
  (find-file "~/Dropbox/GH/apvoice/apvoice.txt")
  (evil-insert-state)
  (goto-char (point-min)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 99_my:dired.el ends here
