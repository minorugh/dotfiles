;;; 30_hydra-work.el --- Hydra configuration for work menu.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-works
  :doc "Selection menu for project work"
  ;; :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _[__]_:創作  _x_panel  _e_:hugo^^  _p_s._r_  _j_unk._g_^^
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _y__,_:年度  _l_epton  g_i_st._l_  re_@_p^^  _b_kup._c_._u_
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
   ("w" my:m_kukai)
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
;;; 30_hydra-work.el ends here
