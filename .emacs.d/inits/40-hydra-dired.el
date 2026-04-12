;;; 40-hydra-dired.el --- Hydra dired-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-dired
  :bind (
	 ("M-."  . hydra-dired/body))
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^^^  _i_nits  root_/_^^  ~/_s_rc  _._files  make._k_._b_._m_._u_  ke_y_chin  ftp._9_._0_._-_  meint_@__c_
  _r_estart  Git:_[__-__]_  _n_otes  GH._h__j_  _f_lych  _<home>_  howm._;__:_._v_iew^^  key_p_ass  _g_ithub.i_o_^^  capture_,_  
"
   ("f" flycheck-list-errors)
   ("-" fzilla-s)
   ("9" fzilla-GH)
   ("0" fzilla-minoruGH)
   ("@" my-open-cron-makefile)
   ("p" keepassxc)
   ("<home>" (my-open "~/" :omit))
   ("g" (my-open "~/src/github.com/minorugh"))
   ("o" (my-open "~/src/github.com/minorugh/minorugh.github.io/docs/"))
   ("." (my-open "~/src/github.com/minorugh/dotfiles/" :omit))
   ("d" (my-open "~/Dropbox/"))
   ("n" (my-open "~/Dropbox/notes/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("J" (my-open "~/Dropbox/howm/junk/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   ("T" (my-open "~/Dropbox/GH/tselext/select.txt" :pos 'top))
   ("P" (my-open "~/Dropbox/GH/tpdia/dia.txt" :pos 'top))
   ("c" (my-open "~/src/github.com/minorugh/dotfiles/cron/"))
   ("s" (my-open "~/src/"))
   ("t" (my-open "/tmp"))
   ("/" (my-open "/" :omit))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" restart-emacs)
   ("v" markdown-preview)
   (":" howm-list-all)
   ("," org-capture)
   (";" my-howm-create-memo)
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
