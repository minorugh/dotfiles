;;; 50-hydra-dired.el --- Hydra dired-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-dired
  :after evil
  :bind (("<henkan>" . my-hydra-dired))
  :init

  ;; ---------------------------------------------------------------
  ;; hydra entry point: mozc を無効化してから起動
  ;; ---------------------------------------------------------------
  (defun my-hydra-dired ()
    "Disable mozc if active, then open hydra-dired."
    (interactive)
    (when current-input-method
      (toggle-input-method))
    (hydra-dired/body))
  (with-eval-after-load 'mozc
    (define-key mozc-mode-map (kbd "<henkan>") #'my-hydra-dired))

  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
 Quick.dired
  _d_ropbox  _e_macs.d^^^^  _i_nits^^  ~/_s_rc  root._/_^^  _._files^  make._k_._b_._m_._u_^  fz._8_._9_._0_  keyp_a_ss  _x_env._l_og
  _r_estart  Git:_[__-__]_  GH._h__j_  term_,_  xsrv_;__:_  _<home>_^  h_\@_wm_c__v_.Sn_o_te  _g_it._p_jct^^  fl_y_make  _f_ind._1__2__3_
"
   ("p" ivy-git-project-switch)
   ("," my-remote-select)
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
   ;; ("o" (my-open "~/src/github.com/minorugh/minorugh.github.io/docs/"))
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
   ("3"  neomutt-restart)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("c" my-howm-create-with-category)
   ("_" delete-other-windows)
   ("[" my-git-peek-smart)
   ("-" git-peek-deleted)
   ("]" my-make-git)
   ("l" (my-open "~/Dropbox/CHANGELOG/"))
   ("q" top-level)
   ("<henkan>" hydra-work/body)
   ("<muhenkan>" nil))

  :init

  ;; ---------------------------------------------------------------
  ;; dired ナビゲーション: ディレクトリ・ファイルを開くユーティリティ
  ;; ---------------------------------------------------------------
  (defun my-make (target &optional dir)
    "Run make TARGET in DIR (default: current directory)."
    (interactive "sTarget: ")
    (let ((default-directory (expand-file-name (or dir default-directory))))
      (compile (concat "make " target))))

  (defun my-open (path &rest opts)
    "Open PATH.  OPTS: :pos 'top|'bottom|integer, :omit, :emacs.
e.g. :pos -10 => bottom-10  :pos 1 => top+1"
    (find-file (expand-file-name
		(format-time-string path)))
    (pcase (plist-get opts :pos)
      ('top    (goto-char (point-min)))
      ('bottom (goto-char (point-max)))
      ((pred integerp)
       (let ((n (plist-get opts :pos)))
	 (goto-char (if (< n 0) (point-max) (point-min)))
	 (forward-line n))))
    (when (memq :omit opts)  (dired-omit-mode 0))
    (when (memq :emacs opts) (evil-emacs-state)))

  ;; ---------------------------------------------------------------
  ;; 2ペイン: xsrv ローカル/リモート対比表示と Q による復帰
  ;; ---------------------------------------------------------------
  (defvar my-2pane-origin-buffer nil
    "Buffer to return to when quitting 2-pane view.")

  (defun my-2pane-quit ()
    (interactive)
    (when (= (length (window-list)) 2)
      (let ((bufs (mapcar #'window-buffer (window-list))))
	(delete-other-windows)
	(mapc #'kill-buffer bufs)
	(when (buffer-live-p my-2pane-origin-buffer)
          (switch-to-buffer my-2pane-origin-buffer)
          (setq my-2pane-origin-buffer nil)))))

  (define-key evil-normal-state-map (kbd "Q") #'my-2pane-quit)
  (add-hook 'dired-mode-hook
            (lambda () (local-set-key (kbd "Q") #'my-2pane-quit)))

  (defun my-open-xsrv-2pane (src-dir pair-dir)
    "Open SRC-DIR in `dired' and line up PAIR-DIR in two panes."
    (setq my-2pane-origin-buffer (current-buffer))
    (delete-other-windows)
    (dired src-dir)
    (split-window-right)
    (other-window 1)
    (dired pair-dir)
    (other-window 1))

  ;; ---------------------------------------------------------------
  ;; 環境・外部アプリ連携
  ;; ---------------------------------------------------------------
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

  (defun my-remote-select ()
    "Select remote dir and open gnome-terminal with SSH."
    (interactive)
    (let* ((home-root "/home/minorugh/")
           (gh-root   (concat home-root "gospel-haiku.com/public_html/"))
           (mn-root   (concat home-root "minorugh.com/public_html/"))
           (dirs `(("home-root"     . ("ls"     . ,home-root))
                   ("gospel-haiku"  . ("ls"     . ,gh-root))
                   ("minorugh.com"  . ("ls"     . ,mn-root))
                   ("docker/httpd"  . ("docker" . "docker exec -it httpd /bin/bash"))
                   ("passwd"        . ("vim"    . ,(concat home-root "gospel-haiku.com/passwd/")))
                   ("d_kukai/data"  . ("vim"    . ,(concat gh-root "d_kukai/data/")))
                   ("w_kukai/data"  . ("vim"    . ,(concat gh-root "w_kukai/data/")))
                   ("s_kukai/data"  . ("vim"    . ,(concat gh-root "s_kukai/data/")))
                   ("m_kukai/data"  . ("vim"    . ,(concat gh-root "m_kukai/data/")))))
           (choice (completing-read "remote: " (mapcar #'car dirs) nil t "^"))
           (entry  (cdr (assoc choice dirs)))
           (action (car entry))
           (dir    (cdr entry))
           (cmd (cond
		 ((string= action "docker")
                  (format "gnome-terminal --maximize -- %s" dir))
		 ((string= action "vim")
                  (format "gnome-terminal --maximize -- ssh -t xsrv 'exec vim %s'" dir))
		 (t
                  (format "gnome-terminal --maximize -- ssh -t xsrv 'cd %s && bash -il'" dir)))))
      (start-process-shell-command "ssh-cd" nil cmd)))

  (defun keepassxc ()
    "Open keepassxc with auto passwd input and detach it from Emacs."
    (interactive)
    (call-process "setsid" nil 0 nil "keepass.sh"))

  (defun filezilla (&optional site)
    "Open FileZilla with SITE and detach it from Emacs."
    (interactive)
    (let* ((sites '(("g" . "0/gospel-haiku.com")
                    ("m" . "0/minorugh.com")
                    ("s" . "-s")))
           (arg (or (cdr (assoc site sites)) "-s"))
           (args (if (string= arg "-s")
                     '("-s")
                   (list (format "--site=%s" arg)))))
      (apply #'call-process "setsid" nil 0 nil "filezilla" args))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 50-hydra-dired.el ends here
