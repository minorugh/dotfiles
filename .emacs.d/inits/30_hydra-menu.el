;;; 30_hydra-menu.el --- Hydra configuration for work menu. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;-------------------------------------------------------------------
;;
;; Hydra configuration for quick menu
;;
;;-------------------------------------------------------------------

(leaf *hydra-quick-menu
  :bind ("M-." . hydra-quick/body)
  :hydra
  (hydra-quick
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_  _s_rc  _._files  scale_+_^^  _z_illa  make._c__k__g_|_b__m__u_
  _r_estart  magit_[__]_  _t_ramp  GH_h_  _o_rg  _<home>_  C-x._0__1_  _p_assx  howm._,__;__@_|md_v_^^^^
  "
   ("p" keepassxc)
   ("a" (org-agenda nil "a"))
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
   ("0" delete-window)
   ("1" delete-other-windows)
   ("_" delete-other-windows)
   ("[" hydra-git/body)
   ("]" magit-status)
   ("s" my:scr-dir)
   ("z" filezilla)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil)))


;;-------------------------------------------------------------------
;; Hydra configuration for my work menu
;;-------------------------------------------------------------------
(leaf *hydra-work-menu
  :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Project.menu
  _d_:日記  _m_:毎日  _k_:兼題  _t_:定例  _f_:週秀  _[__]_:創作  _e_:hugo^^  _p_s._r_  _x_srv._g_^^
  _a_:合評  _n_:近詠  _s_:吟行  _._:添削  _w_:添日  _y__,_:年度  g_i_st._l_  re_@_p^^  _b_kup._c_._u_
"
   ("p" ps-print-buffer)
   ("o" my:docker-compose)
   ("r" ps-print-region)
   ("y" my:year)
   ("Y" my:year-new-post)
   ("," my:year-draft)
   ("a" my:apvoice)
   ("A" my:apvoice-new-post)
   ("P" ps-print-buffer)
   ("b" make-backup)
   ("B" backup-makefile)
   ("u" make-ghuser)
   ("c" make-commit)
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("d" my:diary)
   ("D" my:diary-new-post)
   ("w" my:tpdia)
   ("W" my:tpdia-new-post)
   ("i" gist-region-or-buffer)
   ("l" (browse-url "https://gist.github.com/minorugh"))
   ("L" lepton)
   ("t" my:teirei)
   ("T" my:teirei-new-post)
   ("s" my:swan)
   ("S" my:swan-new-post)
   ("N" my:kinnei)
   ("n" my:kinnei-draft)
   ("m" my:d_kukai)
   ("k" my:m_kukai)
   ("." my:tpost)
   ("f" my:dselext)
   ("F" my:dselext-new-post)
   ("+" text-scale-adjust)
   ("]" my:haiku-note)
   ("[" my:haiku-note-post)
   ("x" my:xsrv-dir)
   ("X" chromium-xsrv)
   ("g" my:github-dir)
   ("G" chromium-github)
   ("<henkan>" hydra-quick/body)
   ("<muhenkan>" nil))
  :init
  (defun make-backup ()
	"Backup all."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make backup")))

  (defun make-ghuser ()
	"Sync GH data by rsync."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make rsync-user")))

  (defun make-commit ()
	"Auto commit."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make git-commit")))

  (defun filezilla ()
	"Open filezilla."
	(interactive)
	(compile "filezilla -s")
	(delete-other-windows))

  (defun lepton ()
	"Open lepton."
	(interactive)
	(compile "~/Appimage/Lepton-1.10.0.AppImage")
	(delete-other-windows))

  (defun keepassxc ()
	"Open keepassxc with auto passwd input."
	(interactive)
	(compile "secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx")
	(delete-other-windows)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 30_hydra-menu.el ends here
