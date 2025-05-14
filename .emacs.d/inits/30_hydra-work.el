;;; 30_hydra-work.el --- Hydra quick work configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-work
  :doc "Selection menu for project work"
  :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _[__]_:創作  _e_:hugo^^  _p_rint._r_e  _._term  _b_ackup
  _v_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _y__n_:年度  _g_ist._l_  _@_point^^  ___xsrv  Th_u_nar
"
   ("p" ps-print-buffer)
   ("r" ps-print-region)
   ("y" my:year)
   ("Y" my:year-new-post)
   ("n" my:year-draft)
   ("v" my:apvoice)
   ("V" my:apvoice-new-post)
   ("b" make-backup)
   ("." terminal-open)
   ("u" thunar-open)
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("d" my:diary)
   ("D" my:diary-new-post)
   ("g" gist-region-or-buffer)
   ("l" (browse-url "https://gist.github.com/minorugh"))
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
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))
  :init
  (defun thunar-open ()
    "Open thunar with current dir."
    (interactive)
    (compile (concat "thunar " default-directory)))

  (defun terminal-open ()
    "Open termninal with current dir."
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (when (and (eq system-type 'gnu/linux)
		 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
	(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
      (compile (concat "gnome-terminal --working-directory " dir))))

  (defun ssh-xsrv ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (compile "gnome-terminal --maximize -- ssh xsrv-GH")))


;;; 30_hydra-work.el ends here
