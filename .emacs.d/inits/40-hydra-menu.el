;;; 40-hydra-menu.el --- Hydra quick-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-work
  :doc "Quick menu for workings"
  ;; :require my:template  ;; Load user define templates
  :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _[__]_:創作  _e_:hugo^^  _p_rint.r_e_  _._term  _r_ainbow  yas._n_._v_._i_  _c_ap._u_p.d_o_wn
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _y__,_:年度  _g_ist._B_  _@_point^^  ___xsrv  _:_thunar  _b_ackupall^^^^  _+_.scale-adj^^^^
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
   ("a" my:apvoice)
   ("A" my:apvoice-new-post)
   ("b" make-backup)
   ("." terminal-open)
   (":" thunar-open)
   ("z" filezilla-open)
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("r" rainbow-mode)
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
   ("w" my:tpost)
   ("f" my:dselext)
   ("F" my:dselext-new-post)
   ("+" text-scale-adjust)
   ("]" my:haiku-note)
   ("[" my:haiku-note-post)
   ("_" ssh-xsrv)
   ("q" iedit--quit)
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))
  :init
  (defun filezilla-open ()
    (interactive)
    (compile "filezilla -s"))

  (defun thunar-open ()
    (interactive)
    (compile (concat "thunar " default-directory)))

  (defun terminal-open ()
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (when (and (eq system-type 'gnu/linux)
		 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
	(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
      (compile (concat "gnome-terminal --working-directory " dir))))

  (defun ssh-xsrv ()
    (interactive)
    (compile "gnome-terminal --maximize -- ssh xsrv-GH")))


(leaf * hydra-dired
  :doc "Quick access for dired"
  ;; :require my:dired  ;; Load user define for quick accsess
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
  (with-eval-after-load 'dired
    "Load user dired for quick accsess."
    (require 'my:dired))

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

;;; 40-hydra-menu.el ends here
