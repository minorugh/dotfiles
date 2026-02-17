;;; 40-hydra-menu.el --- Hydra quick-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-work
  :doc "Quick menu for workings"
  :bind (("<f3>" . terminal-open)
	 ("<f4>" . xsrv-gh)
	 ("<henkan>" . hydra-work/body))
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _[__]_:創作  _/_:月例^^  _p_rint.r_e_  _._kendai  yas._n_._v_._i_  _c_ap._u_p.d_o_wn
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _y__,_:年度  _g_ist._B_  Browse_@_p^^  _:_mqedit  _b_ackup_j_ob  _+_.scale-adj^^^^
"
   ("+" text-scale-adjust)
   ("c" my:capitalize-word)
   ("u" my:upcase-word)
   ("o" my:downcase-word)
   ("n" yas-new-snippet)
   ("v" yas-visit-snippet-file)
   ("i" yas-insert-snippet)
   ("p" ps-print-buffer)
   ("e" ps-print-region)
   ("y" my:year)
   ("Y" my:year-new-post)
   ("," my:year-draft)
   ("/" my:m_kukai)
   ("a" my:apvoice)
   ("A" my:apvoice-new-post)
   ("b" make-backup)
   ("." my:kendai-edit)
   (":" my:marquee-edit)
   ("z" filezilla-open)
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("j" my:job)
   ("d" my:diary)
   ("D" my:diary-new-post)
   ("g" gist-region-or-buffer)
   ("B" (browse-url "https://gist.github.com/minorugh"))
   ("t" my:teirei)
   ("T" my:teirei-new-post)
   ("s" my:swan)
   ("S" my:swan-new-post)
   ("K" my:kinnei)
   ("k" my:kinnei-draft)
   ("m" my:d_kukai)
   ("w" my:w_kukai)
   ("f" my:dselext)
   ("F" my:dselext-new-post)
   ("+" text-scale-adjust)
   ("]" my:haiku-note)
   ("[" my:haiku-note-post)
   ("q" iedit--quit)
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))
  :init
  (defun terminal-open ()
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (when (and (eq system-type 'gnu/linux)
		 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
	(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
      (compile (concat "gnome-terminal --working-directory " dir))))

  (defun xsrv-gh ()
    (interactive)
    (compile "gnome-terminal --maximize -- ssh xsrv-GH"))

  (defun my:job ()
    (interactive)
    (compile "sh /usr/local/bin/myjob.sh")))


(leaf * hydra-dired
  :doc "Quick access for dired"
  :bind ("M-." . hydra-dired/body)
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_  _s_rc  _j_unks  _._dotdir  make._c_._k_|_b__m__u_  _:__l_ocal  _x_modmap
  _r_estart  magit_[__]_  _t_ramp  GH.._h_  o_r_g  _z_illa  _<home>_   howm._,__;__@_|md_v_^  minoru_g_h  _f_lyerro
"
   ("f" flycheck-list-errors)
   ("l" my:github-local)
   (":" (browse-url "https://github.com/minorugh"))
   ("r" my:org-dir)
   ("t" counsel-tramp)
   ("q" my:tramp-quit)
   ("<home>" my:home-dir)
   ("d" my:dropbox)
   ("D" my:documents)
   ("." my:dotfiles-dir)
   ("i" my:inits-dir)
   ("j" my:junk-dir)
   ("e" my:emacs-dir)
   ("h" my:gh-dir)
   ("g" my:minorugh-dir)
   ("r" restart-emacs)
   ("v" markdwn-preview)
   ("@" howm-list-all)
   ("," my:howm-create-memo)
   (";" my:howm-create-tech)
   ("c" make-commit)
   ("k" my:make-k)
   ("p" keepassxc)
   ("b" my:make-bklog)
   ("m" my:make-move)
   ("u" my:make-upsftp)
   ("/" my:root-dir)
   ("_" delete-other-windows)
   ("[" hydra-magit/body)
   ("]" my:magit-status)
   ("s" my:scr-dir)
   ("z" filezilla-open)
   ("x" xmodmap)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil))
  :init
  (with-eval-after-load 'dired
    "Load user dired for quick accsess."
    (require 'my:dired))

  (defun xmodmap ()
    "Execute xmodmap."
    (interactive)
    (shell-command "xmodmap /home/minoru/.Xmodmap"))

  (defun my:magit-status ()
    "Display message if magit in dashboard."
    (interactive)
    (if (string= "*dashboard*" (buffer-name))
	(message "Can't magit in Dashboard！")
      (magit-status-setup-buffer))))


;;; 40-hydra-menu.el ends here
