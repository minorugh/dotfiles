;;; 30_hydra-menu.el --- Hydra configuration for quick menu.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-dired
  :doc "Slection menu for quick access"
  :bind ("M-." . hydra-dired/body)
  :hydra
  (hydra-dired
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
   ("c" make-commit)
   ("k" my:make-k)
   ("g" my:make-git)
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
   ("M-." hydra-project/body)
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


(leaf *hydra-project
  :doc "Selection menu for project work"
  :bind ("<henkan>" . hydra-project/body)
  :hydra
  (hydra-project
   (:hint nil :exit t)
   "
   Project.menu
  _d_:日記  _m_:毎日  _z_:雑詠  _t_:定例  _M_:ＭＪ  _[__]_:創作  _e_:hugo^^  _p_s._r_  _j_unk._g_^^
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _O_:ＯＧ  _y__,_:年度  g_i_st._l_  re_@_p^^  _b_kup._c_._u_
"
   ("p" ps-print-buffer)
   ("r" ps-print-region)
   ("y" my:year)
   ("Y" my:year-new-post)
   ("," my:year-draft)
   ("a" my:apvoice)
   ("A" my:apvoice-new-post)
   ("b" make-backup)
   ("u" make-ghuser)
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
   ("K" my:kinnei)
   ("k" my:kinnei-draft)
   ("m" my:d_kukai)
   ("M" my:mj_kukai)
   ("O" my:og_kukai)
   ("z" my:m_kukai)
   ("f" my:dselext)
   ("F" my:dselext-new-post)
   ("+" text-scale-adjust)
   ("]" my:haiku-note)
   ("[" my:haiku-note-post)
   ("j" (find-file "~/Dropbox/howm/junk/"))
   ("x" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("g" my:github-dir)
   ("G" chromium-github)
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))
  :init
  (defun lepton ()
    "Open lepton."
    (interactive)
    (compile "~/Appimage/Lepton-1.10.0.AppImage")
    (delete-other-windows))

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
      (compile "make git-commit"))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 30_hydra-menu.el ends here
