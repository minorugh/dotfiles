;;; 70-hydra-dired.el --- Hydra dired/work-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ============================================================
;;  Hydra Dired  (ファイルナビゲーションランチャー)
;; ============================================================

(defun my-hydra-dired ()
  "Disable mozc if active, then open hydra-dired."
  (interactive)
  (when current-input-method
    (toggle-input-method))
  (hydra-dired/body))

(with-eval-after-load 'mozc
  (define-key mozc-mode-map (kbd "<henkan>") #'my-hydra-dired))

(leaf hydra-dired
  :after evil
  :require (my-tig-bridge)
  :bind (("<henkan>" . my-hydra-dired))
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
 Quick.dired
  _d_ropbox  _e_macs.d^^^^  _i_nits^^  _s_rc  root._/_^^  _._files^  make._k_._b_._m_._u_^  fz._8_._9_._0_  keyp_a_ss  _x_env._l_og
  _r_estart  Git:_[__-__]_  GH._h__j_  _t_ig  xsrv_;__:_  _<home>_^  h_\@_wm_,__v_.Sn_o_te  _g_it._p_jct^^  fl_y_make  _f_ind._1__2__3_
"
   ("p" project-find-regexp)
   ("t" my-open-tig)
   ("y" flymake-show-buffer-diagnostics)
   ("8" (filezilla "s"))
   ("9" (filezilla "g"))
   ("0" (filezilla "m"))
   ("a" keepassxc)
   ("g" counsel-git)
   ("f" counsel-find-file)
   ("o" (browse-url "https://app.simplenote.com/"))
   ("<home>" (my-open "~/" :omit))
   (":" my-open-xsrv-2pane-gh)        ;; 実体は 40-remote.el を参照
   (";" my-open-xsrv-2pane-minorugh)  ;; 実体は 40-remote.el を参照
   ("." (my-open "~/src/github.com/minorugh/dotfiles/"))
   ("d" (my-open "~/Dropbox/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   ("1" (my-open "~/src/github.com/minorugh/dotfiles/Makefile" :pos 'top))
   ("2" (my-open "~/src/github.com/minorugh/dotfiles/.zshrc" :pos 'top))
   ("x" my-reload-xenv)
   ("s" (my-open "~/src/"))
   ("w" (my-open "~/src/github.com/minorugh/"))
   ("/" (my-open "/" :omit))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" restart-emacs)
   ("3" neomutt-restart)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("," my-howm-create-with-category)
   ("_" delete-other-windows)
   ("[" my-git-peek-smart)
   ("-" git-peek-deleted)
   ("]" my-make-git)
   ("l" (my-open "~/Dropbox/CHANGELOG/"))
   ("q" top-level)
   ("<henkan>"  hydra-work/body)
   ("<muhenkan>" nil))
  :init
  (defun my-make (target &optional dir)
    "Run make TARGET in DIR (default: current directory)."
    (interactive "sTarget: ")
    (let ((default-directory (expand-file-name (or dir default-directory))))
      (compile (concat "make " target))))

  (defun my-open (path &rest opts)
    "Open PATH in dired or find-file.
OPTS: :pos 'top | 'bottom | integer  :omit  :emacs
  :pos -10  → point-max then back 10 lines
  :pos  1   → point-min then forward 1 line"
    (find-file (expand-file-name (format-time-string path)))
    (pcase (plist-get opts :pos)
      ('top    (goto-char (point-min)))
      ('bottom (goto-char (point-max)))
      ((pred integerp)
       (let ((n (plist-get opts :pos)))
         (goto-char (if (< n 0) (point-max) (point-min)))
         (forward-line n))))
    (when (memq :omit  opts) (dired-omit-mode 0))
    (when (memq :emacs opts) (evil-emacs-state)))

  (defvar my-2pane-origin-buffer nil
    "Buffer to return to when quitting 2-pane view.")

  (defvar my-2pane-quit-hook nil
    "Hook run at the end of `my-2pane-quit'.
個人用の追加処理（例: my-xsrv.el の divider 解除など）を
ここから差し込めるようにするための空の拡張ポイント。
このファイル単体では何も登録されておらず、デフォルトでは何も起きない。")

  (defun my-2pane-quit ()
    "Close both panes and return to the original buffer."
    (interactive)
    (when (= (length (window-list)) 2)
      (let ((bufs (mapcar #'window-buffer (window-list))))
        (delete-other-windows)
        (mapc #'kill-buffer bufs)
        (when (buffer-live-p my-2pane-origin-buffer)
          (switch-to-buffer my-2pane-origin-buffer)
          (setq my-2pane-origin-buffer nil))))
    (when (fboundp 'my-update-modeline-for-split)
      (my-update-modeline-for-split))
    (run-hooks 'my-2pane-quit-hook))  ; 個人設定からの追加処理用フック

  (defun my-dired-quit ()
    "2ペイン中なら my-2pane-quit、それ以外は quit-window。"
    (interactive)
    (if (buffer-live-p my-2pane-origin-buffer)
        (my-2pane-quit)
      (quit-window)))

  (define-key evil-normal-state-map (kbd "q") #'my-2pane-quit)
  (add-hook 'dired-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "q") #'my-dired-quit)))

  (defun my-reload-xenv ()
    "Reload xmodmap and re-import SSH_AUTH_SOCK from keychain file."
    (interactive)
    (shell-command "xmodmap ~/.Xmodmap > /dev/null 2>&1")
    (let ((keychain-file (expand-file-name
                          (concat "~/.keychain/" (system-name) "-sh"))))
      (when (file-exists-p keychain-file)
	(with-temp-buffer
          (insert-file-contents keychain-file)
          (goto-char (point-min))
          (while (re-search-forward "^\\([^=]+\\)=\\([^;]+\\);" nil t)
            (setenv (match-string 1)
                    (match-string 2))))))
    (message "xmodmap + SSH_AUTH_SOCK reloaded"))

  (defun keepassxc ()
    "Open KeePassXC via keepass.sh, detached from Emacs."
    (interactive)
    (call-process "setsid" nil 0 nil "keepass.sh"))

  (defun filezilla (&optional site)
    "Open FileZilla with SITE, detached from Emacs.
SITE: \"g\" = gospel-haiku.com, \"m\" = minorugh.com, \"s\" = site manager."
    (interactive)
    (let* ((sites '(("g" . "0/gospel-haiku.com")
                    ("m" . "0/minorugh.com")
                    ("s" . "-s")))
           (arg  (or (cdr (assoc site sites)) "-s"))
           (args (if (string= arg "-s")
                     '("-s")
                   (list (format "--site=%s" arg)))))
      (apply #'call-process "setsid" nil 0 nil "filezilla" args))))


;; ============================================================
;;  Hydra Work  (俳句作業メニュー)
;; ============================================================

(leaf hydra-work
  :after evil
  :bind ("<f14>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t :body-pre (require 'my-template))
   "
 Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _M_:月例^^  _p_rint._r_eg  yas._n_._v_._i_  _h_own  _c_aption._u_p.d_o_wn^^
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _Y_:年度^^  _g_ist._l_ept  _e_asy-hugo^^^^  _j_unk  _+_scale
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
   (":" (my-open "~/Dropbox/GH/year/draft.dat" :pos 1))
   ("Y" my-year-new-post)
   ("M" (my-open "~/Dropbox/GH/m_select/tex/mkukai.txt" :pos 'top))
   ("a" (my-open "~/Dropbox/GH/apvoice/apvoice.txt" :pos 'top :emacs))
   ("A" my-apvoice-new-post)
   ("K" (my-open "~/Dropbox/GH/w_kukai/info/kendai.csv" :pos 'top :emacs))
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("j" (my-open "~/Dropbox/howm/junk/"))
   ("h" (my-open "~/Dropbox/howm/"))
   ("d" (my-open "~/Dropbox/GH/dia/diary.txt" :pos 'top))
   ("D" my-diary-new-post)
   ("g" gist-region-or-buffer)
   ("G" (browse-url "https://gist.github.com/minorugh"))
   ("l" open-lepton)
   ("t" (my-open "~/Dropbox/GH/teirei/tex/teirei.txt" :pos 'top))
   ("T" my-teirei-new-post)
   ("s" (my-open "~/Dropbox/GH/s_select/tex/swan.txt" :pos 'top))
   ("S" my-swan-new-post)
   ("k" (my-open "~/Dropbox/GH/kinnei/draft.dat"))
   ("m" (my-open "~/Dropbox/GH/d_select/tex/minoru_sen.txt" :pos 'top))
   ("w" (my-open "~/Dropbox/GH/w_select/tex/minoru_sen.txt" :pos 'top))
   ("f" (my-open "~/Dropbox/GH/d_selext/select.txt" :pos 'top))
   ("F" my-dselext-new-post)
   ("]" my-haiku-note)
   ("[" my-haiku-note-post)
   ("q" top-level)
   ("<f14>"     hydra-dired/body)
   ("<henkan>"  hydra-dired/body)
   ("<muhenkan>" nil)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 70-hydra-dired.el ends here
