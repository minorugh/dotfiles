;;; my-dired.el --- User dired configurations. -*- lexical-binding: t -*-
;;; Commentary:

;; Definitions for opening specific directories or files.


;;; Code:
(declare-function dired-omit-mode "dired-x")
(declare-function evil-emacs-state "evil-states")

;;;###autoload
(defun my-documents ()
  "Open Documents dir."
  (interactive)
  (find-file "~/Dropbox/Documents/"))

;;;###autoload
(defun backup-makefile ()
  "Open backup makefile."
  (interactive)
  (find-file "~/Dropbox/makefile"))

;;;###autoload
(defun my-abook ()
  "Open addressbook."
  (interactive)
  (find-file "~/src/github.com/minorugh/dotfiles/.abook/addressbook"))

;;;###autoload
(defun my-mutt ()
  "Open mutt dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/dotfiles/.mutt/"))

;;;###autoload
(defun my-zshrc ()
  "Open zshrc."
  (interactive)
  (find-file "~/.zshrc"))

;;;###autoload
(defun my-xsrv-dir ()
  "Open xsrv dir."
  (interactive)
  (find-file "~/src/xsrv"))

;;;###autoload
(defun my-github-dir ()
  "Open github dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/"))

;;;###autoload
(defun my-scr-dir ()
  "Open src dir."
  (interactive)
  (find-file "~/src/"))

;;;###autoload
(defun my-minorugh-dir ()
  "Open minorugh.com dir."
  (interactive)
  (find-file "~/Dropbox/minorugh.com/"))

;;;###autoload
(defun my-github-local ()
  "Open github local dir."
  (interactive)
  (find-file "~/src/github.com/minorugh"))

;;;###autoload
(defun my-junk-dir ()
  "Open junk file dir."
  (interactive)
  (find-file "~/Dropbox/howm/junk/"))

;;;###autoload
(defun my-dropbox ()
  "Open Dropbox dir."
  (interactive)
  (find-file "~/Dropbox/"))

;;;###autoload
(defun my-gh-dir ()
  "Open GH dir."
  (interactive)
  (find-file "~/Dropbox/GH/"))

;;;###autoload
(defun my-pages-dir ()
  "Open github pages dir."
  (interactive)
  (find-file "~/src/github.com/minorugh/minorugh.github.io/docs/"))

;;;###autoload
(defun my-org-dir ()
  "Open org dir."
  (interactive)
  (find-file "~/Dropbox/howm/org/"))

;;;###autoload
(defun my-docker-compose ()
  "Open docker-compose dir."
  (interactive)
  (find-file "~/src/xsrv/docker-compose"))

;;;###autoload
(defun my-open-capture ()
  "Open `org-capture' file."
  (interactive)
  (find-file "~/Dropbox/howm/org/capture.org")
  (goto-char (point-min)))

;;;###autoload
(defun my-diary ()
  "Open diary file."
  (interactive)
  (find-file "~/Dropbox/GH/dia/diary.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-marquee-edit ()
  "Open marquee file."
  (interactive)
  (find-file "~/Dropbox/GH/marquee.dat")
  (goto-char (point-min))
  (evil-emacs-state))

;;;###autoload
(defun my-kendai-edit ()
  "Open kendai file."
  (interactive)
  (find-file "~/Dropbox/GH/w_kukai/info/kendai.txt")
  (goto-char (point-min))
  (evil-emacs-state))

;;;###autoload
(defun my-d_kukai ()
  "Open d_select file."
  (interactive)
  (find-file "~/Dropbox/GH/d_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-w_kukai ()
  "Open w_select file."
  (interactive)
  (find-file "~/Dropbox/GH/w_select/tex/minoru_sen.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-m_kukai ()
  "Open m_select file."
  (interactive)
  (find-file "~/Dropbox/GH/m_select/tex/mkukai.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-swan ()
  "Open s_select file."
  (interactive)
  (find-file "~/Dropbox/GH/s_select/tex/swan.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-mj_kukai ()
  "Open mj_select file."
  (interactive)
  (find-file "~/Dropbox/GH/mj_select/tex/mj.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-tselext ()
  "Open tselext file."
  (interactive)
  (find-file "~/Dropbox/GH/tselext/select.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-dselext ()
  "Open d_selext file."
  (interactive)
  (find-file "~/Dropbox/GH/d_selext/select.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-teirei ()
  "Open teirei file."
  (interactive)
  (find-file "~/Dropbox/GH/teirei/tex/teirei.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-kinnei ()
  "Open kinnei file."
  (interactive)
  (find-file "~/Dropbox/GH/kinnei/kinnei.txt")
  (goto-char (point-min)))

;;;###autoload
(defun my-kinnei-draft ()
  "Open kinnei draft file."
  (interactive)
  (find-file "~/Dropbox/GH/kinnei/draft.dat"))

;;;###autoload
(defun my-year ()
  "Open year file."
  (interactive)
  (find-file (format-time-string "~/Dropbox/GH/year/%Y.txt"))
  (goto-char (point-max))
  (forward-line -10))

;;;###autoload
(defun my-year-draft ()
  "Open year draft file."
  (interactive)
  (find-file "~/Dropbox/GH/year/draft.dat")
  (goto-char (point-min))
  (forward-line))

;;;###autoload
(defun my-tpdia ()
  "Open tpdia file."
  (interactive)
  (find-file "~/Dropbox/GH/tpdia/dia.txt")
  (goto-char (point-min))
  (forward-line))

;;;###autoload
(defun my-apvoice ()
  "Open apvoice file."
  (interactive)
  (find-file "~/Dropbox/GH/apvoice/apvoice.txt")
  (goto-char (point-min)))


(provide 'my-dired)
;;; my-dired.el ends here
