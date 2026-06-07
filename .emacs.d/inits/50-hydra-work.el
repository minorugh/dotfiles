;;; 50-hydra-work.el --- Hydra work-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-work
  :hook (after-init-hook . (lambda () (require 'my-template)))
  :bind ("<f14>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _/_:月例^^   _p_rint._r_eg  _K_endai^^  yas._n_._v_._i_  _c_aption._u_p.d_o_wn^^
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _y_:年度_,_  _g_ist._l_ept  mutt_.__:_  _e_asy-hugo^^^^  b_@_point^^^^  _+_scale
"
   ("+" text-scale-adjust)
   ("." neomutt)
   (":" neomutt-restart)
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
   ("K" (my-open "~/Dropbox/GH/w_kukai/info/kendai.csv" :pos 'top :emacs))
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("J" (my-open "~/Dropbox/howm/junk/"))
   ("j" my-junk-new)
   ("d" (my-open "~/Dropbox/GH/dia/diary.txt" :pos 'top))
   ("D" my-diary-new-post)
   ("g" gist-region-or-buffer)
   ("G" (browse-url "https://gist.github.com/minorugh"))
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
   ("<f14>" hydra-dired/body)
   ("<henkan>" hydra-dired/body)
   ("<muhenkan>" nil))

  :preface
  (leaf browse-at-remote
    :ensure t
    :doc "Open github page from Emacs"
    :config
    (setq browse-at-remote-prefer-symbolic nil))

  (defun neomutt ()
    "Toggle NeoMutt window."
    (interactive)
    (let ((win (string-trim (shell-command-to-string "wmctrl -l | grep 'NeoMutt Mail'"))))
      (if (string= win "")
          (call-process "setsid" nil 0 nil "neomutt.sh")
	(call-process "wmctrl" nil 0 nil "-a" "NeoMutt Mail"))))

  (defun neomutt-restart ()
    "Kill and restart NeoMutt tmux session."
    (interactive)
    (call-process "tmux" nil 0 nil "kill-session" "-t" "mail")
    (call-process "setsid" nil 0 nil "neomutt.sh"))

  (defun gist-region-or-buffer ()
    "Post region or buffer to Gist."
    (interactive)
    (let ((file (buffer-file-name)))
      (if (not (use-region-p))
          (compile (concat "gist -od '' " file))
        (compile (concat "gist -oPd '' -f " (file-name-nondirectory file)))))
    (delete-other-windows))

  (defun open-lepton ()
    "Specify the full path, disable the sandbox if necessary, and start Lepton."
    (interactive)
    (start-process-shell-command
     "lepton" nil
     "~/Apps/Lepton-1.10.0.AppImage --no-sandbox"))

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
;;; 50-hydra-work.el ends here
