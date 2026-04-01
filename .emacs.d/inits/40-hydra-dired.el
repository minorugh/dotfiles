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
  _d_ropbox  _e_macs.d^^^^  _i_nits  root_/_^^  _s_ourc  _._dotdir  make._c_._k_._b__m__u_  ._l_ocal  fzilla._9_._0_._-_  capture_,_
  _r_estart  Git:_[__:__]_  _n_mutt  GH._h__j_  .i_o_  _<home>_   howm._;__@_._v_iew^^^^  key_p_assx  myjobs._x_._g_._:_  _f_lyerror
"
   ("f" flycheck-list-errors)
   ("9" fzilla-s)
   ("0" fzilla-GH)
   ("-" fzilla-minoruGH)
   ("G" (browse-url "https://github.com/minorugh"))
   ("," org-capture)
   ("p" keepassxc)
   ("l" (my-open "~/src/github.com/minorugh"))
   ("o" (my-open "~/src/github.com/minorugh/minorugh.github.io/"))
   ("q" (top-level))
   ("<home>" my-open-user)
   ("." my-open-dotfiles)
   ("d" (my-open "~/Dropbox/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("J" (my-open "~/Dropbox/howm/junk/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   (";" my-howm-create-memo)
   ("c" (my-make "clean"))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("/" my-open-root)
   ("_" delete-other-windows)
   ("[" git-peek)
   (":" git-peek-deleted)
   ("]" my-make-git)
   ("s" (my-open "~/src/"))
   ("g" (browse-url "http://localhost:3000/minoru"))
   ("G" git-peek-deleted)
   ("t" my-tig)
   ("n" neomutt)
   ("x" my-reload-keychain)
   ("M-." hydra-work/body)
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
    (let ((root (or (locate-dominating-file (or buffer-file-name default-directory) "Makefile")
                    (locate-dominating-file (or buffer-file-name default-directory) "makefile"))))
      (if root
          (let ((default-directory root))
            (compile "make git"))
	(message "Makefile (or makefile) not found"))))

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

  (defun my-open-dotfiles ()
    (interactive)
    (my-open "~/src/github.com/minorugh/dotfiles/")
    (dired-omit-mode 0))

  (defun my-open-user ()
    (interactive)
    (my-open "~/")
    (dired-omit-mode 0))

  (defun my-open-root ()
    (interactive)
    (my-open "/")
    (dired-omit-mode 0))

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

;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 40-hydra-dired.el ends here
