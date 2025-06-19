;;; 30-hydra-work.el --- Hydra quick work configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-work
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

;;; 30-hydra-work.el ends here
