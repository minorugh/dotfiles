;;; 40-hydra-dired.el --- Hydra dired-menu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-dired
  :bind ("M-." . hydra-dired/body)
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^^^  _i_nits  root_/_^^  ~/_s_rc  _._files  make._k_._b_._m_._u_  ._g_._o_._l_  sftp._9__0__-_  meint_:_
  _r_estart  Git:_[__-__]_  _n_otes  GH._h__j_  _f_lych  _<home>_  howm._;__@_._v_iew^^  key_p_assX^^^^  capture_,_^^^^  ke_y_chn
"
   ("f" flycheck-list-errors)
   ("-" fzilla-s)
   ("9" fzilla-GH)
   ("0" fzilla-minoruGH)
   ("g" (browse-url "https://github.com/minorugh"))
   (":" my-open-cron-makefile)
   ("p" keepassxc)
   ("<home>" (my-open-a "~/"))
   ("l" (my-open "~/src/github.com/minorugh"))
   ("o" (my-open "~/src/github.com/minorugh/minorugh.github.io/docs/"))
   ("." (my-open-a "~/src/github.com/minorugh/dotfiles/"))
   ("d" (my-open "~/Dropbox/"))
   ("n" (my-open "~/Dropbox/notes/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("J" (my-open "~/Dropbox/howm/junk/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   ("T" (my-open "~/Dropbox/GH/tselext/select.txt" 'top))
   ("P" (my-open "~/Dropbox/GH/tpdia/dia.txt" 'top))
   ("c" my-open-cron-makefile)
   ("s" (my-open "~/src/"))
   ("t" (my-open "/tmp"))
   ("/" (my-open-a "/"))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("," org-capture)
   (";" my-howm-create-memo)
   ("_" delete-other-windows)
   ("[" git-peek)
   ("-" git-peek-deleted)
   ("]" my-make-git)
   ("y" my-reload-keychain)
   ("M-." hydra-work/body)
   ("q" (top-level))
   ("<muhenkan>" nil))
  :init
  (defun my-reload-keychain ()
    "Reload keychain environment variables in Emacs session for SSH."
    (interactive)
    ;; keychain が書いた SSH_AUTH_SOCK と SSH_AGENT_PID を Emacs 内に設定
    (let ((keychain-file (expand-file-name (concat "~/.keychain/" (system-name) "-sh"))))
      (when (file-exists-p keychain-file)
	(with-temp-buffer
          (insert-file-contents keychain-file)
          ;; export 文を eval して Emacs 内に反映
          (goto-char (point-min))
          (while (re-search-forward "^export \\([^=]+\\)=\\(.*\\)$" nil t)
            (setenv (match-string 1) (replace-regexp-in-string "^\"\\|\"$" "" (match-string 2))))))
      (message "Keychain reloaded in Emacs!")))

  (add-hook 'after-init-hook #'my-reload-keychain)
  
  (defun my-make-git ()
    "Run 'make git' in the repository root of the current buffer."
    (interactive)
    (let* ((dir (or buffer-file-name default-directory))
           (root (locate-dominating-file dir "Makefile")))
      (if root
          (let ((default-directory root))
            (compile "make git"))
	(message "Makefile not found"))))

  (defun my-make (target &optional dir)
    "Run make TARGET in DIR (default: current directory)."
    (interactive "sTarget: ")
    (let ((default-directory (expand-file-name (or dir default-directory))))
      (compile (concat "make " target))))

  (defun my-open (path &optional pos)
    "Open PATH.  POS options: top, bottom, or nil."
    (find-file (expand-file-name path))
    (cond ((eq pos 'top)    (goto-char (point-min)))
          ((eq pos 'bottom) (goto-char (point-max)))))

  (defun my-open-a (path &optional pos)
    "Open PATH.  POS options: top, bottom, or nil."
    (find-file (expand-file-name path))
    (cond ((eq pos 'top)    (goto-char (point-min)))
          ((eq pos 'bottom) (goto-char (point-max))))
    (dired-omit-mode 0))

  (defun my-open-e (path &optional pos)
    "Open PATH.  POS options: top, bottom, or nil."
    (find-file (expand-file-name path))
    (cond ((eq pos 'top)    (goto-char (point-min)))
          ((eq pos 'bottom) (goto-char (point-max))))
    (evil-emacs-state))

  (defun fzilla-GH ()
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/gospel-haiku.com'"))

  (defun fzilla-minoruGH ()
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/minorugh.com'"))

  (defun fzilla-s ()
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla -s"))

  (defun keepassxc ()
    "Open keepassxc with auto passwd input."
    (interactive)
    (start-process-shell-command "keepass" nil "keepass.sh")))

(require 'my-template)

;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 40-hydra-dired.el ends here
