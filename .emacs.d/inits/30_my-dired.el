;;; 30_my-dired.el --- Hydra quick dired configurations. -*- no-byte-compile: t; -*-
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
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_  _s_rc  _._files  scale_+_  _z_illa  make._c__k__g_|_b__m__u_  ::_l_._p_._n_
  _r_estart  magit_[__]_  _t_ramp  GH.._h_  _o_rg  _<home>_  _f_lychk  _P_assx  howm._,__;__@_|md_v_^^^^  _D_ocuments
  "
   ("n" my:github-new)
   ("l" my:github-local)
   ("+" text-scale-adjust)
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
   ("P" keepassxc)
   ("p" my:pages-dir)
   ("b" my:make-bklog)
   ("m" my:make-move)
   ("u" my:make-upsftp)
   ("/" my:root-dir)
   ("f" flycheck-list-errors)
   ("_" delete-other-windows)
   ("[" hydra-git/body)
   ("]" my:magit-status)
   ("s" my:scr-dir)
   ("z" filezilla)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil))
  :init
  (defun keepassxc ()
    "Open keepassxc with auto passwd input."
    (delete-other-windows))

  (defun my:magit-status ()
    "Display message if magit in dashboard."
    (interactive)
    (if (string= "*dashboard*" (buffer-name))
	(message "Can't magit in Dashboard！")
      (magit-status)))

  (defun filezilla ()
    "Open filezilla."
    (interactive)
    (compile "filezilla -s")
    (delete-other-windows))

  (defun my:github-new ()
    "Open filezilla."
    (interactive)
    (shell-command "github-new hoge")
    ;; (find-file "~/src/github.com/minorugh")
    ))


;;; 30_my-dired.el ends here
