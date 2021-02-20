;;; 40_neotree.el --- Neotree configurations. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf neotree
  :ensure t
  :bind (("<f10>" . neotree-find)
		 (:neotree-mode-map
		  ("RET" . neotree-enter-hide)
		  ("a" . neotree-hidden-file-toggle)
		  ("<left>" . neotree-select-up-node)
		  ("<right>" . neotree-change-root)
		  ("<f10>" . neotree-hide)))
  :config
  (setq-default neo-keymap-style 'concise)
  (setq neo-create-file-auto-open t)
  (doom-themes-neotree-config)
  (setq neo-window-width 30)

  ;; Change neotree's font size
  ;; Tips from https://github.com/jaypei/emacs-neotree/issues/218
  (defun neotree-text-scale ()
	"Text scale for neotree."
	(interactive)
	(text-scale-adjust 0)
	(text-scale-decrease .5)
	(message nil))
  (add-hook 'neo-after-create-hook
			(lambda (_)
			  (call-interactively 'neotree-text-scale)))

  ;; neotree enter hide
  ;; Tips from https://github.com/jaypei/emacs-neotree/issues/77
  (defun neo-open-file-hide (full-path &optional arg)
	"Open file and hiding neotree.
The description of FULL-PATH & ARG is in `neotree-enter'."
	(neo-global--select-mru-window arg)
	(find-file full-path)
	(neotree-hide))

  (defun neotree-enter-hide (&optional arg)
	"Neo-open-file-hide if file, Neo-open-dir if dir.
The description of ARG is in `neo-buffer--execute'."
	(interactive "P")
	(neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 40_neotree.el ends here

