;;; 10_hydra-menu.el --- hydra for work menu  -*- lexical-binding: t -*-
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
  ---^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----------------------------------------------------------------------------------------------------
   _d_ropbox   _e_macs.d^^   _i_nits   _x_srv.jp^^^^   GH:_h_   _._files   howm:_@_   _g_ithub   _r_estart   _m_arkdown   _u_ndotree
   init_l_og   git:_[_._]_   _t_ramp   _y_as:_n_:_v_   _a_g🐾   _f_lyERR   _s_earch   make:_k_   _c_ompile   _o_pen-url   capture_,_"
   ("a" counsel-ag)
   ("o" browse-url-at-point)
   ("t" counsel-tramp)
   ("q" my:tramp-quit)
   ("<home>" my:home-dir)
   ("<end>" my:root-dir)
   ("d" my:dropbox)
   ("i" my:inits-dir)
   ("e" my:emacs-dir)
   ("h" my:gh-dir)
   ("x" my:xsrv-dir)
   ("y" ivy-yasnippet)
   ("n" yas/new-snippet)
   ("v" yas/visit-snippet-file)
   ("r" restart-emacs)
   ("s" counsel-web-suggest)
   ("m" hydra-markdown/body)
   (":" hydra-pinky/body)
   ("." my:dotfiles-dir)
   ("c" hydra-compile/body)
   ("b" counsel-bookmark)
   ("B" bookmark-set)
   ("g" my:github-show)
   ("@" howm-list-all)
   ("," org-capture)
   ("k" my:make-k)
   ("f" counsel-flycheck)
   ("u" undo-tree-visualize)
   (";" hydra-work/body)
   ("w" my:mozc-word-regist)
   ("/" kill-other-buffers)
   ("_" delete-other-windows)
   ("[" hydra-magit/body)
   ("]" magit-status)
   ("l" init-loader-show-log)
   ("<muhenkan>" nil)
   ("M-." nil)))


(leaf hydra-work-menu
  :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
  📝 Work Menu
  --------------^^^^^^^^^^^^^^^^^^^^^^^^^-------------------------------------------------------------------------------------
    Work: _a_:合評   _d_:日記   _m_:毎日   _w_:毎週   _k_:兼題   _t_:定例   _s_:吟行   _o_:落穂   _n_:近詠   創作:_[_:_]_
    Tool: _l_epton   _e_:Hugo   _j_unk🐾   _b_ackup   _p_asswd   ps_2_pdf   _i_❤IMG   _f_tp:🐾   scale_+_   package_@_"
   ("a" my:apsh)
   ("A" my:apsh-new-post)
   ("e" easy-hugo)
   ("d" my:diary)
   ("D" my:diary-new-post)
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
   ("c" open-cacher)
   ("l" open-lepton)
   ("i" (browse-url "https://www.iloveimg.com/ja"))
   ("r" counsel-rg)
   ("@" hydra-package/body)
   ("p" open-keepass)
   ("2" pdfout-select)
   (":" my:ps-print)
   ("+" text-scale-adjust)
   ("/" kill-other-buffers)
   ("_" delete-other-windows)
   (";" hydra-quick-menu/body)
   ("[" my:haiku-note)
   ("]" my:haiku-note-post)
   ("j" open-junk-file)
   ("J" open-last-junk-file)
   ("v" view-mode)
   ("f" ftp-client)
   ("<muhenkan>" nil)
   ("<henkan>" nil)))


(leaf user-defined-function
  :config
  (defun my:ps-print ()
    "Narrow the only counsel-command in M-x."
    (interactive)
    (counsel-M-x "^ps-print "))

  (defun ftp-client ()
    "Open Ftp application."
    (interactive)
    (when (getenv "WSLENV")
      (compile "/mnt/c/\"Program Files\"/\"FileZilla FTP Client\"/filezilla.exe"))
    (unless (getenv "WSENV")
      (compile "filezilla -s")))

  (defun open-lepton ()
    "Open Ftp application."
    (interactive)
	(compile "lepton"))

  (defun open-keepass ()
    "Narrow the only espy command in M-x."
    (interactive)
    (compile "secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx"))

  (defun open-calculator ()
    "Narrow the only espy command in M-x."
    (interactive)
    (compile "gnome-calculator"))

  (defun my:backupall ()
    "Backup for melpa package."
    (interactive)
  	(let* ((default-directory (expand-file-name "~/Dropbox/backup")))
	  (compile "make -k"))))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 10_hydra-menu.el ends here
