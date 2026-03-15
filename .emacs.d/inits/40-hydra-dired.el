;;; 40-hydra-dired.el --- Hydra dired-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-dired
  :defun my:make my:open dired-omit-mode
  :bind ("M-." . hydra-dired/body)
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_^^  _s_rc  _._dotdir  make._c_._k_._b__m__u_  ._l_ocal  Fzilla._9_._0_._-_  _,_Capture
  _r_estart  magit_[__]_  _n_mutt  GH._h__j_  _o_rg  _<home>_   howm._;__@_._v_iew^^^^  key_p_assx  _g_itk.mtmost_:_^^  _f_lyckeck
"
   ("f" flycheck-list-errors)
   ("9" fzilla-s)
   ("0" fzilla-GH)
   ("-" fzilla-minoruGH)
   ("G" (browse-url "https://github.com/minorugh"))
   ("," org-capture)
   ("p" keepassxc)
   ("l" (my:open "~/src/github.com/minorugh"))
   (":" mattermost)
   ("o" (my:open "~/Dropbox/howm/org/"))
   ("q" my:tramp-quit)
   ("<home>" my:open-user)
   ("." my:open-dotfiles)
   ("d" (my:open "~/Dropbox/"))
   ("i" (my:open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("J" (my:open "~/Dropbox/howm/junk/"))
   ("e" (my:open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my:open "~/Dropbox/GH/"))
   ("j" (my:open "~/Dropbox/minorugh.com/"))
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   (";" my:howm-create-memo)
   ("c" (my:make "clean"))
   ("k" (my:make "-k"))
   ("b" (my:make "bk"))
   ("m" (my:make "mv"))
   ("u" (my:make "up"))
   ("/" my:open-root)
   ("_" delete-other-windows)
   ("[" hydra-magit/body)
   ("]" my:magit-status)
   ("s" (my:open "~/src/"))
   ("g" gitk-open)
   ("n" neomutt)
   ("x" xmodmap)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil))
  :init
  (defun my:make (target &optional dir)
    "Run make TARGET in DIR (default: current directory)."
    (interactive "sTarget: ")
    (let ((default-directory (expand-file-name (or dir default-directory))))
      (compile (concat "make " target))))

  (defun my:open (path &optional pos)
    "Open PATH.  POS options: top, bottom, or nil."
    (find-file (expand-file-name path))
    (cond ((eq pos 'top)    (goto-char (point-min)))
          ((eq pos 'bottom) (goto-char (point-max)))))

  (defun my:open-dotfiles ()
    (interactive)
    (my:open "~/src/github.com/minorugh/dotfiles/")
    (dired-omit-mode 0))

  (defun my:open-user ()
    (interactive)
    (my:open "~/")
    (dired-omit-mode 0))

  (defun my:open-root ()
    (interactive)
    (my:open "/")
    (dired-omit-mode 0))

  (defun fzilla-GH ()
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/gospel-haiku.com'"))

  (defun fzilla-minoruGH ()
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/minorugh.com'"))

  (defun fzilla-s ()
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla -s"))

  (defun keepassxc ()
    "Open keepassxc with auto passwd input."
    (interactive)
    (start-process-shell-command "keepass" nil "keepass.sh"))

  (defun xmodmap ()
    "Execute xmodmap."
    (interactive)
    (start-process-shell-command "xmodmap" nil "xmodmap ~/.Xmodmap"))

  (defun my:magit-status ()
    "Open magit status buffer."
    (interactive)
    (if (string= "*dashboard*" (buffer-name))
	(message "Can't magit in Dashboard!")
      (magit-status-setup-buffer))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings)
;; End:
;;; 40-hydra-dired.el ends here
