;;; 20_hydra-menu.el --- Hydra configuration for work menu. -*- lexical-binding: t -*-
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
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^--------------------------------------------------------------------------------------
  _d_ropbox  _e_macs.d^^  _i_nits  GH:_h_  root:_/_  _s_rc  _._files  gith_u_b  _m_d:pvie_w_  howm_@__,__;_  _f_zilla
  _r_estart  magit_[__]_  _t_ramp  _j_unk  _b_rowse  _o_rg  _<home>_  _p_asswd  make._k_._g_  _c_ompile^^^^  scale_:_
"
   ("p" keepassxc)
   ("a" (org-agenda nil "a"))
   (":" text-scale-adjust)
   ("o" my:org-dir)
   ("t" counsel-tramp)
   ("q" my:tramp-quit)
   ("<home>" my:home-dir)
   ("d" my:dropbox)
   ("D" my:docker-dir)
   ("." my:dotfiles-dir)
   ("i" my:inits-dir)
   ("e" my:emacs-dir)
   ("h" my:gh-dir)
   ("n" my:nextcloud)
   ("y" company-yasnippet)
   ("N" yas/new-snippet)
   ("v" yas/visit-snippet-file)
   ("r" restart-emacs)
   ("m" hydra-markdown/body)
   ("w" livedown-preview)
   ("W" livedown-kill)
   ("b" hydra-browse/body)
   ("c" hydra-make/body)
   ("@" howm-list-all)
   ("," my:howm-create-memo)
   (";" my:howm-create-tech)
   ("j" open-junk-file)
   ("J" open-last-junk-file)
   ("k" my:make-k)
   ("g" my:make-git)
   ("/" my:root-dir)
   ("_" my:delete-other-windows)
   ("[" hydra-git/body)
   ("]" magit-status)
   ("s" my:scr-dir)
   ("S" sudo-edit)
   ("u" my:github-dir)
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
  -------------^^^^^^^^^^^^^^^^^^^^^^^^^^----------------------------------------------------------------------
  _d_:日記  _m_:毎日  _w_:毎週  _k_:兼題  _t_:定例  _s_:吟行  創作:_[_._]_  c_o_mpose  _p_rint._r_  gitk_.__:_
  _a_:合評  _n_:近詠  _e_:hugo  _b_ackup  _g_rsync  _c_ommit  g_i_st:_l_  remote_@_  _x_srv_u_  web_,_^^
"
   ("p" ps-print-buffer)
   ("o" my:docker-compose)
   ("r" ps-print-region)
   ("a" my:apvoice)
   ("A" my:apvoice-new-post)
   ("P" ps-print-buffer)
   ("b" make-backup)
   ("B" backup-makefile)
   ("g" make-grsync)
   ("c" make-commit)
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("d" my:diary)
   ("D" my:diary-new-post)
   ("i" gist-region-or-buffer)
   ("l" (browse-url "https://gist.github.com/minorugh"))
   ("t" my:teirei)
   ("T" my:teirei-new-post)
   ("s" my:swan)
   ("S" my:swan-new-post)
   ("N" my:kinnei)
   ("n" my:kinnei-draft)
   ("o" my:docker-compose)
   ("m" my:d_kukai)
   ("w" my:w_kukai)
   ("k" my:m_kukai)
   ("h" chromium-tegaki)
   ("." gitk-open)
   (":" git-gui-open)
   ("," counsel-web-suggest)
   ("/" my:delete-this-file)
   ("f" flymake-show-diagnostics-buffer)
   ("+" text-scale-adjust)
   ("_" my:delete-other-windows)
   ("]" my:haiku-note)
   ("[" my:haiku-note-post)
   ("x" my:xsrv-dir)
   ("X" chromium-xsrv)
   ("u" my:github-dir)
   ("U" chromium-github)
   ("z" my:zshrc)
   ("<henkan>" hydra-quick/body)
   ("<muhenkan>" nil))
  :init
  (defun make-backup ()
	"Backup all."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make backup")))

  (defun make-grsync ()
	"Sync GH data by rsync."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make grsync")))

  (defun make-commit ()
	"Auto commit."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make commit")))

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
;;; 20_hydra-menu.el ends here

