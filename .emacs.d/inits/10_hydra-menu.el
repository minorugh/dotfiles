;;; 10_hydra-menu.el --- Hydra configuration for work menu. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-quick-menu
  :bind ("M-." . hydra-quick-menu/body)
  :hydra
  (hydra-quick-menu
   (:hint nil :exit t)
   "
  🐳 Quick Menu
  ---^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----------------------------------------------------------------------------------------------------
  _d_ropbox  _e_macs.d^^  _i_nits  _x_srv.jp^^^^  GH:_h_  _<home>_  root_/_  _._dotfiles  howm_,_list_@_  _g_ithub  _r_estart  _m_arkdown
  shell:_z_  git:_[_._]_  _t_ramp  _y_as:_n_:_v_  _a_g🐾  pinky_:_  _p_rint  _u_ndo-tree  _w_eb-search^^  make:_k_  _c_ompile  _o_rg:_l_ink"
   ("a" counsel-ag)
   ("o" my:org-dir)
   ("l" my:open-link.org)
   ("t" counsel-tramp)
   ("q" my:tramp-quit)
   ("<home>" my:home-dir)
   ("d" my:dropbox)
   ("i" my:inits-dir)
   ("e" my:emacs-dir)
   ("h" my:gh-dir)
   ("x" my:xsrv-dir)
   ("y" ivy-yasnippet)
   ("n" yas/new-snippet)
   ("v" yas/visit-snippet-file)
   ("r" restart-emacs)
   ("w" counsel-web-suggest)
   ("m" hydra-markdown/body)
   ("c" hydra-compile/body)
   ("b" counsel-bookmark)
   ("B" bookmark-set)
   ("g" my:github-show)
   ("@" howm-list-all)
   ("," org-capture)
   ("k" my:make-k)
   ("u" undo-tree-visualize)
   ("/" my:root-dir)
   (";" hydra-work/body)
   ("_" delete-other-windows)
   ("[" hydra-magit/body)
   ("]" magit-status)
   ("s" swiper-thing-at-point)
   ("z" eshell)
   (":" hydra-pinky/body)
   ("p" ps-print-buffer)
   ("." my:dotfiles-dir)
   ("M-." hydra-work/body)
   ("<henkan>" hydra-work/body)
   ("<muhenkan>" nil)))


(leaf hydra-work-menu
  :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
  📝 Work Menu
  --------------^^^^^^^^^^^^^^^^^^^^^^^^^-------------------------------------------------------------------------------------
   Work: _a_:合評   _d_:日記   _m_:毎日   _w_:毎週   _k_:兼題   _t_:定例   _s_:吟行   _o_:落穂   _n_:近詠   創作:_[_:_]_
   Tool: _e_:hugo   _g_ist🐾   _j_unk🐾   _b_ackup   _p_assxc   ps_2_pdf   pinky_:_   _f_tp:🐾   scale_+_   package_@_"
   ("a" my:apvoice)
   ("A" my:apvoice-new-post)
   ("e" easy-hugo)
   ("d" my:diary)
   ("D" my:diary-new-post)
   ("g" (browse-url "https://gist.github.com/minorugh"))
   ("G" gist-from-buffer)
   ("o" my:otibo)
   ("O" my:otibo-new-post)
   ("t" my:teirei)
   ("T" my:teirei-new-post)
   ("s" my:swan)
   ("S" my:swan-new-post)
   ("n" my:kinnei)
   ("N" my:kinnei-new-post)
   ("m" my:d_kukai)
   ("w" my:w_kukai)
   ("k" my:m_kukai)
   ("b" my:backupall)
   ("B" my:backup-dir)
   ("i" my:inits-dir)
   ("@" hydra-package/body)
   ("p" open-keepass)
   ("2" pdfout-select)
   ("+" text-scale-adjust)
   ("/" kill-other-buffers)
   ("_" delete-other-windows)
   ("[" my:haiku-note)
   ("]" my:haiku-note-post)
   ("j" my:open-code.org)
   ("f" ftp-client)
   (":" hydra-pinky/body)
   (";" hydra-quick-menu/body)
   ("<henkan>" hydra-quick-menu/body)
   ("<muhenkan>" nil)))


(leaf user-defined-function
  :config
  (defun ftp-client ()
    "Open Ftp application."
    (interactive)
    (when (getenv "WSLENV")
      (compile "/mnt/c/\"Program Files\"/\"FileZilla FTP Client\"/filezilla.exe"))
    (unless (getenv "WSENV")
      (shell-command "filezilla -s")))

  (defun gist-from-buffer ()
	"Gist from current buffer, then open chromium."
	(interactive)
	(let ((file (buffer-file-name (current-buffer))))
	  (shell-command (concat "gist -o " file))))

  (defun open-keepass ()
	"Open keepassxc withe auto passwd input."
	(interactive)
	(shell-command "secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx"))

  (defun open-calculator ()
	"Open calculator."
	(interactive)
	(shell-command "gnome-calculator"))

  (defun my:backupall ()
	"Backup for melpa package."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox/backup")))
	  (compile "make -k"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10_hydra-menu.el ends here
