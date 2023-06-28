;;; 08_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :hook (emacs-startup-hook . (lambda () (load-theme 'doom-dracula t)))
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold   . nil)
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :custom-face
  (region  . '((t (:background "#6272a4" :extend t))))
  (hl-line . '((t (:background "#3B4252" :extend t)))))


(leaf doom-modeline
  :doc "Fast mode-line inspired by minimalism design."
  :url "https://github.com/seagle0128/doom-modeline"
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-icon            . t)
  (doom-modeline-major-mode-icon . nil)
  (doom-modeline-minor-modes     . nil)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (leaf nerd-icons :ensure t)
  (leaf hide-mode-line
	:ensure t
	:after doom-modeline
	:hook ((imenu-list-major-mode-hook neotree-mode-hook) . hide-mode-line-mode))
  (leaf nyan-mode
	:ensure t
	:if (display-graphic-p)
	:after doom-modeline
	:config
	(nyan-mode 1)
	(nyan-start-animation)))


(leaf display-line-numbers
  :doc "Show line numbers"
  :hook ((after-init-hook . global-display-line-numbers-mode)
		 ((dired-mode-hook
		   neotree-mode-hook
		   lisp-interaction-mode-hook
		   eshell-mode-hook) . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


(leaf darkroom
  :doc "Remove visual distractions and focus on writing"
  :url "https://github.com/joaotavora/darkroom"
  :ensure t
  :bind (([f12] . my:darkroom-in)
		 (:darkroom-mode-map
		  ([f12] . my:darkroom-out)))
  :init
  (defun my:darkroom-in ()
	"Enter to the `darkroom-mode'."
	(interactive)
	(diff-hl-mode 0)
	(display-line-numbers-mode 0)
	(darkroom-mode 1)
	(setq-local line-spacing .5))

  (defun my:darkroom-out ()
	"Returns from `darkroom-mode' to the previous state."
	(interactive)
	(darkroom-mode 0)
	(display-line-numbers-mode 1)
	(setq-local line-spacing .1)))


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


(leaf counsel-web
  :doc "Search the Web using Ivy"
  :url "https://github.com/mnewt/counsel-web"
  :ensure t
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 08_ui.el ends here
