;;; 20_utils.el --- Utility command cofiguration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)
;; User custom functions

(leaf *cus-ut-functions
  :bind	(([f3]  . thunar-open)
		 ([f4]  . terminal-open)
		 ([f5]  . ssh-xsrv)
		 ([f6]  . quickrun)
		 ("C-x C-c"  . restart-emacs)
		 ([muhenkan] . my:muhenkan))
  :init
  (defun thunar-open ()
	"Open thunar with current dir."
	(interactive)
	(shell-command (concat "xdg-open " default-directory)))

  (defun terminal-open ()
	"Open termninal with current dir."
	(interactive)
	(let ((dir (directory-file-name default-directory)))
	  (when (and (eq system-type 'gnu/linux)
				 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
		(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
	  (shell-command (concat "gnome-terminal --working-directory " dir))))

  (defun ssh-xsrv ()
	"Open terminal and ssh to xsrv."
	(interactive)
	(shell-command "gnome-terminal -- ssh xsrv"))

  (defun my:muhenkan ()
	(interactive)
	(if (not (use-region-p))
		(minibuffer-keyboard-quit)
	  (keyboard-quit))))


(leaf quickrun
  :doc "Qick executes editing buffer"
  :url "https://github.com/emacsorphanage/quickrun"
  :ensure t)


(leaf restart-emacs
  :doc "Restart emacs from within emacs"
  :url "https://github.com/iqbalansari/restart-emacs"
  :ensure t)


(leaf key-chord
  :doc "Mapping a pair of simultaneously pressed keys"
  :url "https://github.com/emacsorphanage/key-chord"
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log))
  :custom (key-chord-two-keys-delay . 0.1))


(leaf sequential-command
  :doc "Many commands into one command"
  :url "https://github.com/HKey/sequential-command/blob/master/sequential-command.el"
  :el-get "HKey/sequential-command"
  :config
  (leaf sequential-command-config
	:hook (after-init-hook . sequential-command-setup-keys)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_utils.el ends here
