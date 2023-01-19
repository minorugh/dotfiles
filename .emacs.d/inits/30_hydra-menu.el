;;; 30_hydra-menu.el --- Hydra configuration for work menu. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra configuration for quick menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *hydra-quick-menu
  :bind ("M-." . hydra-quick/body)
  :hydra
  (hydra-quick
   (:hint nil :exit t)
   "
   Quick Menu
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^--------------------------------------------------------------------------
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_  GH._h_  _s_rc  _._files  _f_zilla  make._c__k__g_|_b__m__u_
  _r_estart  magit_[__]_  _t_ramp  evil_:_  scale_+_  _o_rg  _<home>_  _p_assxc  howm._@__,__;_|md_v_^^^^
  "
   ("p" keepassxc)
   ("a" (org-agenda nil "a"))
   ("+" text-scale-adjust)
   (":" toggle-evil-mode)
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
   ("v" markdown-preview)
   ("r" restart-emacs)
   ("w" markdown-preview)
   ("x" markdown-export)
   ("@" howm-list-all)
   ("," my:howm-create-memo)
   (";" my:howm-create-tech)
   ("j" open-junk-file)
   ("J" open-last-junk-file)
   ("c" hydra-make/body)
   ("k" my:make-k)
   ("g" my:make-git)
   ("b" my:make-bklog)
   ("m" my:make-move)
   ("u" my:make-upsftp)
   ("/" my:root-dir)
   ("0" my:delete-window)
   ("_" my:delete-other-windows)
   ("[" hydra-git/body)
   ("]" magit-status)
   ("s" my:scr-dir)
   ("S" sudo-edit)
   ("l" my:open-capture)
   ("f" filezilla)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra configuration for my work menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *hydra-work-menu
  :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Work Menu
  -------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^------------------------------------------------------------------
  _d_:日記  _m_:毎日  _w_:毎週  _k_:兼題  _t_:定例  _[__]_創作  this_/__\\_  _e_:hugo^^  _p_s._r_  _x_srv._g_^^
  _a_:合評  _n_:近詠  _s_:吟行  _._:添削  _z_:月秀  _f_:週秀^^  _,_:添日^^^  g_i_st._l_  re_@_p^^  _b_kup._c_._u_
"
   ("p" ps-print-buffer)
   ("o" my:docker-compose)
   ("r" ps-print-region)
   ("y" my:year)
   ("Y" my:year-new-post)
   ("h" my:year-draft)
   ("H" my:make-draft)
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
   ("," my:tpdia)
   ("<" my:tpdia-new-post)
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
   ("w" my:w_kukai)
   ("k" my:m_kukai)
   ("." my:tpost)
   ("f" my:dselext)
   ("F" my:dselext-new-post)
   ("z" my:tselext)
   ("Z" my:tselext-new-post)
   (":" git-gui-open)
   ("\\" my:delete-this-file)
   ("/" kill-this-buffer)
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

(defun logout ()
  "Xfce4 session logout."
  (interactive)
  (compile "xfce4-session-logout")
  (delete-other-windows))

(defun reboot ()
  "Xfce4 session logout."
  (interactive)
  (compile "sudo reboot")
  (delete-other-windows))

(defun shutdown ()
  "Xfce4 session logout."
  (interactive)
  (compile "sudo shutdown -h now")
  (delete-other-windows))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 30_hydra-menu.el ends here
