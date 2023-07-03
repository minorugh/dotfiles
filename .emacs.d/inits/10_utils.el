;;; 10_utils.el --- Utility command cofiguration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)
;; User custom functions

(leaf *cus-ut-functions
  :bind	(([f3]  . thunar-open)
		 ([f4]  . terminal-open)
		 ([f5]  . ssh-xsrv)
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


(leaf key-chord
  :doc "Mapping a pair of simultaneously pressed keys"
  :url "https://github.com/emacsorphanage/key-chord"
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log))
  :custom (key-chord-two-keys-delay . 0.1))


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


(leaf *cus-counsel-ag
  :doc "Fast full-text search"
  :url "https://takaxp.github.io/init.html#org29c7b6b7"
  :bind ("s-a" . counsel-ag)
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
  :config
  (setq ps-multibyte-buffer 'non-latin-printer
		ps-paper-type       'a4
		ps-font-size        9
		;; ps-font-family      'Helvetica
		ps-font-family      'Courier
		ps-line-number-font 'Courier
		ps-printer-name     nil
		ps-print-header     nil
		ps-show-n-of-n      t
		ps-line-number      t
		ps-print-footer     nil)
  (defalias 'ps-mule-header-string-charsets 'ignore))


(leaf counsel-web
  :doc "Search the Web using Ivy"
  :url "https://github.com/mnewt/counsel-web"
  :ensure t
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))


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
;;; 10_utils.el ends here
