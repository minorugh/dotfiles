;;; 30-hydra-dired.el --- Hydra quick dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf * quick-dired
  :doc "Slection menu for quick access"
  :bind ("M-." . hydra-dired/body)
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_  _s_rc  _._files  _z_illa  make._c__k__g_|_b__m__u_  _:_.Loca_l_  _n_eomutt
  _r_estart  magit_[__]_  _t_ramp  GH.._h_  _o_rg  _<home>_  _p_assx  howm._,__;__@_|md_v_^^^^  _D_ocuments^^  _f_lylist
"
   ("f" flycheck-list-errors)
   ("l" my:github-local)
   (":" (browse-url "https://github.com/minorugh"))
   ("o" my:org-dir)
   ("t" counsel-tramp)
   ("q" my:tramp-quit)
   ("<home>" my:home-dir)
   ("d" my:dropbox)
   ("D" my:documents)
   ("." my:dotfiles-dir)
   ("i" my:inits-dir)
   ("e" my:emacs-dir)
   ("h" my:gh-dir)
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("," my:howm-create-memo)
   (";" my:howm-create-tech)
   ("c" make-commit)
   ("k" my:make-k)
   ("g" my:make-git)
   ("p" keepassxc)
   ("b" my:make-bklog)
   ("m" my:make-move)
   ("u" my:make-upsftp)
   ("/" my:root-dir)
   ("_" delete-other-windows)
   ("[" hydra-magit/body)
   ("]" my:magit-status)
   ("s" my:scr-dir)
   ("z" filezilla)
   ("n" neomutt)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil))
  :init
  (defun keepassxc ()
    "Open keepassxc with auto passwd input."
    (interactive)
    (compile "keepass.sh"))

  (defun my:magit-status ()
    "Display message if magit in dashboard."
    (interactive)
    (if (string= "*dashboard*" (buffer-name))
	(message "Can't magit in Dashboard！")
      (magit-status-setup-buffer)))

  (defun neomutt ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (compile "neomutt.sh"))
  (setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

  (defun filezilla ()
    "Open filezilla."
    (interactive)
    (compile "filezilla -s")
    (delete-other-windows)))

;;; 30-hydra-dired.el ends here
