;;; 40-hydra-dired.el --- Hydra dired-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-dired
  :bind (("M-."  . hydra-dired/body))
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^^^  _i_nits  root_/_^^  ~/_s_rc  _._files  make._k_._b_._m_._u_  ke_y_chin  ftp._9_._0_._-_  meint._:__;_
  _r_estart  Git:_[__-__]_  _n_otes  GH._h__j_  _f_lych  _<home>_  h_@_wm_,_._v_._n_ote  key_p_ass  _g_ithub.i_o_^^  capture_,_
"
   ("a" consult-ripgrep)
   ("f" flymake-show-buffer-diagnostics)
   ("-" fzilla-s)
   ("9" fzilla-GH)
   ("0" fzilla-minoruGH)
   ("t" my-open-tig)
   ("p" keepassxc)
   ("n" (browse-url "https://app.simplenote.com/"))
   ("<home>" (my-open "~/" :omit))
   ("g" (my-open "~/src/github.com/minorugh"))
   ("o" (my-open "~/src/github.com/minorugh/minorugh.github.io/docs/"))
   ("." (my-open "~/src/github.com/minorugh/dotfiles/"))
   ("d" (my-open "~/Dropbox/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   (":" (my-open "~/src/github.com/minorugh/dotfiles/Makefile" :pos 'top))
   (";" (my-open "~/src/github.com/minorugh/dotfiles/cron/Makefile" :pos 'top))
   ("s" (my-open "~/src/"))
   ("/" (my-open "/" :omit))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("," org-capture)
   ("_" delete-other-windows)
   ("[" git-peek)
   ("-" git-peek-deleted)
   ("]" my-make-git)
   ("y" my-reload-keychain)
   ("q" top-level)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil))
  :init
  (defun my-make (target &optional dir)
    "Run make TARGET in DIR (default: current directory)."
    (interactive "sTarget: ")
    (let ((default-directory (expand-file-name (or dir default-directory))))
      (compile (concat "make " target))))

  (defun my-open (path &rest opts)
    "Open PATH.  OPTS: :pos 'top|'bottom|integer, :omit, :emacs.
e.g. :pos -10 => bottom-10  :pos 1 => top+1"
    (find-file (expand-file-name
		(format-time-string path)))
    (pcase (plist-get opts :pos)
      ('top    (goto-char (point-min)))
      ('bottom (goto-char (point-max)))
      ((pred integerp)
       (let ((n (plist-get opts :pos)))
	 (goto-char (if (< n 0) (point-max) (point-min)))
	 (forward-line n))))
    (when (memq :omit opts)  (dired-omit-mode 0))
    (when (memq :emacs opts) (evil-emacs-state))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 40-hydra-dired.el ends here
