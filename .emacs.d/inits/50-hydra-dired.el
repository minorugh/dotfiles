;;; 50-hydra-dired.el --- Hydra dired-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Hydra Dired  (ファイルナビゲーションランチャー)
;;; ============================================================

(leaf hydra-dired
  :after evil
  :require (my-tig-bridge)  ;; tig ↔ git-peek bridge
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
   (":" (my-open-xsrv-2pane "~/src/github.com/minorugh/xsrv-GH/" "~/Dropbox/GH/"))
   (";" (my-open-xsrv-2pane "~/src/github.com/minorugh/xsrv-minorugh/" "~/Dropbox/minorugh.com/"))
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
  (defun my-hydra-dired ()
    "Disable mozc if active, then open hydra-dired."
    (interactive)
    (when current-input-method
      (toggle-input-method))
    (hydra-dired/body))

  ;; mozc-mode-map からも同じキーで起動できるようにする
  (with-eval-after-load 'mozc
    (define-key mozc-mode-map (kbd "<henkan>") #'my-hydra-dired))


;;; ============================================================
;;;  ファイル・ディレクトリオープン
;;; ============================================================

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


;;; ============================================================
;;;  2ペインビュー  (xsrv ローカル/リモート対比表示)
;;; ============================================================

  (defvar my-2pane-origin-buffer nil
    "Buffer to return to when quitting 2-pane view.")

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
      (my-update-modeline-for-split)))

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

  (defun my-open-xsrv-2pane (src-dir pair-dir)
    "Open SRC-DIR and PAIR-DIR side by side."
    (setq my-2pane-origin-buffer (current-buffer))
    (shell-command "~/.emacs.d/elisp/bin/xsrv-backup-smart.sh &")
    (delete-other-windows)
    (dired src-dir)
    (split-window-right)
    (other-window 1)
    (dired pair-dir)
    (other-window 1))


;;; ============================================================
;;;  環境・外部アプリ連携
;;; ============================================================

  (defun my-reload-xenv ()
    "Reload ~/.xprofile and re-import keychain env vars into Emacs."
    (interactive)
    (shell-command "bash ~/.xprofile > /dev/null 2>&1")
    (let ((keychain-file (expand-file-name
                          (concat "~/.keychain/" (system-name) "-sh"))))
      (when (file-exists-p keychain-file)
        (with-temp-buffer
          (insert-file-contents keychain-file)
          (goto-char (point-min))
          (while (re-search-forward "^export \\([^=]+\\)=\\(.*\\)$" nil t)
            (setenv (match-string 1)
                    (replace-regexp-in-string "^\"\\|\"$" "" (match-string 2)))))))
    (message "xprofile + keychain reloaded"))

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


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 50-hydra-dired.el ends here
