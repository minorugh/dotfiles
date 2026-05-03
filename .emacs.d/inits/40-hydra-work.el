;;; 40-hydra-work.el --- Hydra work-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-work
  ;; :bind (("<henkan>" . hydra-work/body))
  :require (my-template)
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
   ("y" (my-open "~/Dropbox/GH/year/%Y.txt" :pos -10))
   ("," (my-open "~/Dropbox/GH/year/draft.dat" :pos 1))
   ("Y" my-year-new-post)
   ("/" (my-open "~/Dropbox/GH/m_select/tex/mkukai.txt" :pos 'top))
   ("a" (my-open "~/Dropbox/GH/apvoice/apvoice.txt" :pos 'top :emacs))
   ("A" my-apvoice-new-post)
   ("b" (my-make "-k" "~/Dropbox"))
   ("." (my-open "~/Dropbox/GH/w_kukai/info/kendai.csv" :pos 'top :emacs))
   (":" (my-open "~/Dropbox/GH/marquee.dat" :pos 'top :emacs))
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("J" (my-open "~/Dropbox/howm/junk/"))
   ("j" my-junk-new)
   ("d" (my-open "~/Dropbox/GH/dia/diary.txt" :pos 'top))
   ("D" my-diary-new-post)
   ("g" gist-region-or-buffer)
   ("l" open-lepton)
   ("t" (my-open "~/Dropbox/GH/teirei/tex/teirei.txt" :pos 'top))
   ("T" my-teirei-new-post)
   ("s" (my-open "~/Dropbox/GH/s_select/tex/swan.txt" :pos 'top))
   ("S" my-swan-new-post)
   ("K" (my-open "~/Dropbox/GH/kinnei/kinnei.txt" :pos 'top))
   ("k" (my-open "~/Dropbox/GH/kinnei/draft.dat"))
   ("m" (my-open "~/Dropbox/GH/d_select/tex/minoru_sen.txt" :pos 'top))
   ("w" (my-open "~/Dropbox/GH/w_select/tex/minoru_sen.txt" :pos 'top))
   ("f" (my-open "~/Dropbox/GH/d_selext/select.txt" :pos 'top))
   ("F" my-dselext-new-post)
   ("]" my-haiku-note)
   ("[" my-haiku-note-post)
   ("q" top-level)
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))
  :preface
  (defun my-junk-new ()
    "タイムスタンプ付きPerlスクラッチファイルを開く。"
    (interactive)
    (let* ((file    (format-time-string "~/Dropbox/howm/junk/%Y%m%d%H%M.pl"))
           (is-new  (not (file-exists-p file))))
      (find-file file)
      (when is-new
	(insert "#!/usr/bin/perl\nuse strict;\nuse warnings;\n\n")
	(when evil-mode (evil-insert-state))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 40-hydra-work.el ends here
