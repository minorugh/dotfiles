;;; 40-hydra-menu.el --- Hydra work-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-work
  :bind (("<f3>" . terminal-open)
	 ("<f4>" . xsrv-gh)
	 ("<f6>" . thunar-open)
	 ("<henkan>" . hydra-work/body))
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _[__]_:創作  _/_:月例^^  _p_rint._r_e  _._kendai  yas._n_._v_._i_  _c_ap._u_p.d_o_wn
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _y__,_:年度  _g_ist._l_  browse_@_p^^  _:_marque  _e_asy-hugo^^^^  _j_unk._+_scale^^
"
   ("+" text-scale-adjust)
   ("c" my-capitalize-word)
   ("u" my-upcase-word)
   ("o" my-downcase-word)
   ("n" yas-new-snippet)
   ("v" yas-visit-snippet-file)
   ("i" yas-insert-snippet)
   ("p" ps-print-buffer)
   ("r" ps-print-region)
   ("y" my-year)
   ("Y" my-year-new-post)
   ("," my-year-draft)
   ("/" (my-open "~/Dropbox/GH/m_select/tex/mkukai.txt" 'top))
   ("a" (my-open-e "~/Dropbox/GH/apvoice/apvoice.txt" 'top))
   ("A" my-apvoice-new-post)
   ("b" (my-make "-k" "~/Dropbox"))
   ("." (my-open-e "~/Dropbox/GH/w_kukai/info/kendai.csv" 'top))
   (":" (my-open-e "~/Dropbox/GH/marquee.dat" 'top))
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("j" (my-open "~/Dropbox/GH/junk"))
   ("d" (my-open "~/Dropbox/GH/dia/diary.txt" 'top))
   ("D" my-diary-new-post)
   ("g" gist-region-or-buffer)
   ("l" (browse-url "https://gist.github.com/minorugh"))
   ("t" (my-open "~/Dropbox/GH/teirei/tex/teirei.txt" 'top))
   ("T" my-teirei-new-post)
   ("s" (my-open "~/Dropbox/GH/s_select/tex/swan.txt" 'top))
   ("S" my-swan-new-post)
   ("K" (my-open "~/Dropbox/GH/kinnei/kinnei.txt" 'top))
   ("k" (my-open "~/Dropbox/GH/kinnei/draft.dat"))
   ("m" (my-open "~/Dropbox/GH/d_select/tex/minoru_sen.txt" 'top))
   ("w" (my-open "~/Dropbox/GH/w_select/tex/minoru_sen.txt" 'top))
   ("f" (my-open "~/Dropbox/GH/d_selext/select.txt" 'top))
   ("F" my-dselext-new-post)
   ("]" my-haiku-note)
   ("[" my-haiku-note-post)
   ("q" top-level)
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))
  :init
  (defun terminal-open ()
    "Open gnome-terminal at current dir on adjacent display."
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (start-process-shell-command
       "gnome-terminal" nil
       (concat "gnome-terminal --working-directory " dir))
      (run-with-timer
       0.5 nil
       (lambda ()
	 (shell-command
          "xdotool search --sync --onlyvisible --class gnome-terminal windowmove 0 0")))))

  (defun thunar-open ()
    "Open Thunar at current dir on adjacent display."
    (interactive)
    (start-process-shell-command
     "thunar" nil
     (concat "thunar " default-directory))
    (run-with-timer
     0.5 nil
     (lambda ()
       (shell-command
	"xdotool search --sync --onlyvisible --class thunar windowmove 0 0"))))

  (defun xsrv-gh ()
    (interactive)
    (start-process-shell-command "xsrv-gh" nil "gnome-terminal --maximize -- ssh xsrv-GH"))

  (defun my-year ()
    "Open year file and move to near bottom."
    (interactive)
    (find-file (format-time-string "~/Dropbox/GH/year/%Y.txt"))
    (goto-char (point-max))
    (forward-line -10))

  (defun my-year-draft ()
    "Open year draft file."
    (interactive)
    (find-file "~/Dropbox/GH/year/draft.dat")
    (goto-char (point-min))
    (forward-line))

  (defun my-apvoice ()
    "Open apvoice file."
    (interactive)
    (my-open "~/Dropbox/GH/apvoice/apvoice.txt" 'top))

  (defun my-m_kukai ()
    "Open m_select file."
    (interactive)
    (my-open "~/Dropbox/GH/m_select/tex/mkukai.txt" 'top)))


(require 'my-template)

;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 40-hydra-menu.el ends here
