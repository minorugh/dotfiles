;;; 40-hydra-dired.el --- Hydra dired-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Hydra menu bound to `M-.' for quick `dired' navigation and external tool launch.
;; filezilla function accepts site key (g/m/s) for direct launch via hydra keys.
;;
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-dired
  :bind (("<henkan>" . my-hydra-dired))
  :init
  ;; `<henkan>' キーは mozc アクティブ時に `mozc-mode-map' の `<t>' バインディングに
  ;; 横取りされ hydra が起動できない。そのため `my-hydra-dired' を `mozc-mode-map' に
  ;; 直接登録し、mozc を deactivate してから hydra-dired を起動するようにしている。
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
  _d_ropbox  _e_macs.d^^^^  _i_nits^^  ~/_s_rc  root_/_  _._files^  make._k_._b_._m_._u_  _l_og._w_  ftp_9_._0_._-_  xsrv._:__;_
  _r_estart  Git:_[__-__]_  GH._h__j_  term_,_  _x_.env  _<home>_^  h_@_wm_c_._v_.no_t_e  _p_assX^^  _g_it-repo^^^^  _f_lymake^^
"
   ("a" counsel-git-grep)
   ("," my-remote-select)
   ("f" flymake-show-buffer-diagnostics)
   ("8" (filezilla "s"))
   ("9" (filezilla "g"))
   ("0" (filezilla "m"))
   ("p" keepassxc)
   ("g" ivy-git-project-switch)
   ("n" neomutt)
   ("t" (browse-url "https://app.simplenote.com/"))
   ("<home>" (my-open "~/" :omit))
   (":" (my-open "~/src/github.com/minorugh/xsrv-GH/"))
   (";" (my-open "~/src/github.com/minorugh/xsrv-minorugh/"))
   ("o" (my-open "~/src/github.com/minorugh/minorugh.github.io/docs/"))
   ("." (my-open "~/src/github.com/minorugh/dotfiles/"))
   ("d" (my-open "~/Dropbox/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   ("w" (my-open "~/src/github.com/minorugh/dotfiles/Makefile" :pos 'top))
   ("x" my-reload-xenv)
   ("s" (my-open "~/src/"))
   ("/" (my-open "/" :omit))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("c" my-howm-create-with-category)
   ("_" delete-other-windows)
   ("[" my-git-peek-smart)
   ("-" git-peek-deleted)
   ("]" my-make-git)
   ("l" (my-open "~/Dropbox/Changelog/"))
   ("q" top-level)
   ("<henkan>" hydra-work/body)
   ("<muhenkan>" nil))
  :init
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

  (defun my-reload-xenv ()
    "Reload ~/.xprofile and re-import keychain env vars into Emacs."
    (interactive)
    (shell-command "bash ~/.xprofile > /dev/null 2>&1")
    ;; keychain の環境変数を Emacs プロセスに反映
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
    "Open keepassxc with auto passwd input."
    (interactive)
    (start-process-shell-command
     "keepass" nil "keepass.sh"))

  (defun filezilla (&optional site)
    "Open FileZilla with SITE."
    (interactive)
    (let* ((sites '(("g" . "0/gospel-haiku.com")
                    ("m" . "0/minorugh.com")
                    ("s" . "-s")))
           (arg (or (cdr (assoc site sites)) "-s"))
           (cmd (if (string= arg "-s")
                    "filezilla -s"
                  (format "filezilla --site='%s'" arg))))
      (start-process-shell-command "filezilla" nil cmd))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 40-hydra-dired.el ends here
