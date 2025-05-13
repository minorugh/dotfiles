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
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _[__]_:創作  _e_:hugo^^  _p_rint  org:_a__,_  _._:gterm-open
  _v_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _y__n_:年度  _g_ist._l_  repo_@_  _b_ackup^^  ___thunar-open
"
   ("," org-capture)
   ("a" org-agenda)
   ("p" ps-print-buffer)
   ("P" ps-print-region)
   ("y" my:year)
   ("Y" my:year-new-post)
   ("n" my:year-draft)
   ("v" my:apvoice)
   ("V" my:apvoice-new-post)
   ("b" make-backup)
   ("." terminal-open)
   ("_" thunar-open)
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
   ("G" chromium-github)
   ("SPC" set-mark-command)
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))
  :init
  (defun my:upcase-word (arg)
    "convert previous word (or ARG words) to upper case."
    (interactive "p")
    (upcase-word (- arg)))

  (defun my:downcase-word (arg)
    "Convert previous word (or ARG words) to down case."
    (interactive "p")
    (downcase-word (- arg)))

  (defun my:capitalize-word (arg)
    "Convert previous word (or ARG words) to capitalize."
    (interactive "p")
    (capitalize-word (- arg))))


;;; 30_hydra-work.el ends here
