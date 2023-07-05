;;; 13_edit-ut.el --- Editing support interface configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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


(leaf iedit
  :doc "Edit multiple regions in the same way simultaneously"
  :url "https://github.com/victorhge/iedit"
  :ensure t
  :bind ([insert] . iedit-mode))


(leaf sudo-edit
  :doc "Open root parmission files as sudo user"
  :url "https://github.com/nflath/sudo-edit"
  :ensure t)
										;

(leaf expand-region
  :doc "Extension for region"
  :url "https://github.com/magnars/expand-region.el"
  :ensure t
  :bind ("C-@" . er/expand-region))


(leaf smartparens
  :doc "Minor mode for dealing with pairs"
  :url "https://github.com/Fuco1/smartparens"
  :ensure t
  :hook (after-init-hook . smartparens-global-mode)
  :config
  (leaf smartparens-config :require t))


(leaf atomic-chrome
  :doc "Edit Chrome text area with Emacs"
  :url "https://github.com/alpha22jp/atomic-chrome"
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom ((atomic-chrome-buffer-open-style . 'full)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 13_edit-ut.el ends here
