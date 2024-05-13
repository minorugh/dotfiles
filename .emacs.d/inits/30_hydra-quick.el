;;; 30_hydra-quick.el --- Hydra configuration for quick menu.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-quick-menu
  :doc "Slection menu for quick access"
  :bind ("M-." . hydra-quick/body)
  :hydra
  (hydra-quick
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_  _s_rc  _._files  scale_+_  _z_illa  make._c__k__g_|_b__m__u_
  _r_estart  magit_[__]_  _t_ramp  GH_h_  _o_rg  _<home>_  _f_lychk  _p_assx  howm._,__;__@_|md_v_^^^^
  "
   ("p" keepassxc)
   ("+" text-scale-adjust)
   ("o" my:org-dir)
   ("t" counsel-tramp)
   ("q" my:tramp-quit)
   ("<home>" my:home-dir)
   ("d" my:dropbox)
   ("D" chromium-dropbox)
   ("." my:dotfiles-dir)
   ("i" my:inits-dir)
   ("e" my:emacs-dir)
   ("h" my:gh-dir)
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("," my:howm-create-memo)
   (";" my:howm-create-tech)
   ("c" hydra-make/body)
   ("k" my:make-k)
   ("g" my:make-git)
   ("b" my:make-bklog)
   ("m" my:make-move)
   ("u" my:make-upsftp)
   ("/" my:root-dir)
   ("f" flycheck-list-errors)
   ;; ("f" make-frame-command)
   ;; ("0" delete-frame)
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
    (interactive)
    (compile "secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx")
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
    (delete-other-windows)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 30_hydra-quick.el ends here
