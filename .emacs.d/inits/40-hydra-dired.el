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
  ;; `<henkan>' key is caught by `mozc-handle-event' via catch-all `<t>' binding
  ;; in `mozc-mode-map' while mozc is active.  Registering the key directly in
  ;; `mozc-mode-map' ensures `my-hydra-dired' takes precedence over mozc.
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
  _d_ropbox  _e_macs.d^^^^  _i_nits  ~/_s_rc^^  roo_t_  _._files  make._k_._b_._m_._u_  ke_y_cahin  ftp_9_._0_._-_  meint_:__;_
  _r_estart  Git:_[__-__]_  _w_rite  GH._h__j_  _x_srv^  _<home>_  h_@_wm_,_._v_._n_ote  key_p_assx  _g_it-proj^^^  _f_lymake
"
   ("a" counsel-git-grep)
   ("x" xsrv-ssh-fzf)
   ("f" flymake-show-buffer-diagnostics)
   ("8" (filezilla "s"))
   ("9" (filezilla "g"))
   ("0" (filezilla "m"))
   ("t" my-reload-xprofile)
   ("p" keepassxc)
   ("g" ivy-git-project-switch)
   ("n" (browse-url "https://app.simplenote.com/"))
   ("<home>" (my-open "~/" :omit))
   ("g" (my-open "~/src/github.com/minorugh"))
   ("o" (my-open "~/src/github.com/minorugh/minorugh.github.io/docs/"))
   ("." (my-open "~/src/github.com/minorugh/dotfiles/"))
   ("d" (my-open "~/Dropbox/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   (";" (my-open "~/src/github.com/minorugh/dotfiles/Makefile" :pos 'top))
   (":" my-reload-xprofile)
   ("s" (my-open "~/src/"))
   ("t" (my-open "/" :omit))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("," my-howm-create-with-category)
   ("/" kill-current-buffer)
   ("_" delete-other-windows)
   ("[" git-peek)
   ("-" git-peek-deleted)
   ("]" my-make-git)
   ("w" my-darkroom-toggle)
   ("y" my-reload-keychain)
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

  (defun my-reload-xprofile ()
    "Reload ~/.xprofile."
    (interactive)
    (shell-command "bash ~/.xprofile > /dev/null 2>&1")
    (message "xprofile reloaded"))

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
