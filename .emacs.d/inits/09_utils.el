;;; 09_utils.el --- Utility command cofiguration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)
;; User custom functions

(leaf *cus-ut-functions
  :bind	(([f3] . thunar-open)
		 ([f4] . terminal-open)
		 ([f5] . ssh-xsrv)
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


(leaf which-key
  :doc "Displays available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom (which-key-max-description-length . 40))


;; Flymake
(leaf flymake
  :doc "Finding Syntax Errors On The Fly"
  :hook (emacs-lisp-mode-hook . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (leaf flymake-posframe
	:doc "Display flymake diagnostics at point"
	:url "https://github.com/Ladicle/flymake-posframe"
	:el-get Ladicle/flymake-posframe
	:hook (flymake-mode-hook . flymake-posframe-mode)
	:custom
	(flymake-posframe-error-prefix . " ")))


(leaf imenu-list
  :doc "Show imenu entries in a separate buffer"
  :url "https://github.com/bmag/imenu-list"
  :ensure t
  :hook (imenu-list-major-mode-hook . neo-hide-nano-header)
  :bind ([f2] . imenu-list-smart-toggle)
  :custom
  `((imenu-list-size                   . 30)
	(imenu-list-auto-resize            . t)
	(imenu-list-position               . 'left)
	(imenu-list-focus-after-activation . t))
  :preface
  (leaf counsel-css
	:ensure t
	:hook (css-mode-hook . counsel-css-imenu-setup)))


(leaf counsel-tramp
  :doc "Tramp ivy interface for ssh server"
  :url "https://github.com/masasam/emacs-counsel-tramp"
  :ensure t
  :custom
  `((tramp-persistency-file-name . ,"~/.emacs.d/tmp/tramp")
	(tramp-default-method        . "scp")
	(counsel-tramp-custom-connections
	 . '(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))
  :config
  (defun my:tramp-quit ()
	"Quit tramp, if tramp connencted."
	(interactive)
	(when (get-buffer "*tramp/scp xsrv*")
	  (tramp-cleanup-all-connections)
	  (counsel-tramp-quit)
	  (message "Tramp Quit!"))))


(leaf migemo
  :doc "Japanese increment search with 'Romanization of Japanese'"
  :url "https://github.com/emacs-jp/migemo"
  :if (executable-find "cmigemo")
  :ensure t
  :hook (after-init-hook . migemo-init)
  :custom
  `((migemo-command    . "cmigemo")
	(migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")))


(leaf *cus-counsel-ag
  :doc "Fast full-text search"
  :url "https://takaxp.github.io/init.html#org29c7b6b7"
  :init
  (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
	(apply f (or initial-input
				 (and (not (thing-at-point-looking-at "^\\*+"))
					  (ivy-thing-at-point)))
		   (unless current-prefix-arg
			 (or initial-directory default-directory))
		   extra-ag-args ag-prompt caller))
  (with-eval-after-load "counsel"
	(require 'thingatpt nil t)
	(advice-add 'counsel-ag :around #'ad:counsel-ag)
	;; Make search trigger even with 2 characters
	(add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))
	(ivy-add-actions
	 'counsel-ag
	 '(("r" my:counsel-ag-in-dir "search in directory")))

	(defun my:counsel-ag-in-dir (_arg)
	  "Search again with new root directory."
	  (let ((current-prefix-arg '(4)))
		(counsel-ag ivy-text nil "")))))


(leaf *cus-ps-print
  :doc "Print from Emacs via Postscript"
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :init
  (defalias 'ps-mule-header-string-charsets 'ignore)
  (setq ps-multibyte-buffer 'non-latin-printer
		ps-paper-type 'a4
		ps-font-size 9
		;; ps-font-family 'Helvetica
		ps-font-family 'Courier
		ps-line-number-font 'Courier
		ps-printer-name nil
		ps-print-header nil
		ps-show-n-of-n t
		ps-line-number t
		ps-print-footer nil))


(leaf sequential-command
  :doc "Many commands into one command"
  :url "https://github.com/HKey/sequential-command/blob/master/sequential-command.el"
  :el-get "HKey/sequential-command"
  :config
  (leaf sequential-command-config
	:hook (after-init-hook . sequential-command-setup-keys)))


(leaf package-utils
  :doc "Interactive package manager"
  :url "https://github.com/Silex/package-utils"
  :ensure t
  :chord ("p@" . hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
  Package: _i_nstall _r_emove _l_ist up_a_ll    El-get:_u_pdate.re_m_ove
"
   ("i" package-install)
   ("l" package-utils-list-upgrades)
   ("r" package-utils-remove-by-name)
   ("a" package-utils-upgrade-all-and-restart)
   ("u" el-get-update-all)
   ("m" el-get-remove)
   ("<muhenkan>" nil)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 09_utils.el ends here
