;;; 20_ut.el --- Counsel ut configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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


(leaf which-key
  :doc "Displays available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom ((which-key-max-description-length . 40)))


(leaf counsel-web
  :doc "Search the Web using Ivy"
  :url "https://github.com/mnewt/counsel-web"
  :ensure t
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))


(leaf sequential-command
  :doc "Many commands into one command"
  :url "https://github.com/HKey/sequential-command/blob/master/sequential-command.el"
  :el-get "HKey/sequential-command"
  :config
  (leaf sequential-command-config
	:hook (after-init-hook . sequential-command-setup-keys)))


(leaf key-chord
  :doc "Mapping a pair of simultaneously pressed keys"
  :url "https://github.com/emacsorphanage/key-chord"
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log))
  :custom (key-chord-two-keys-delay . 0.1))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_ut.el ends here
