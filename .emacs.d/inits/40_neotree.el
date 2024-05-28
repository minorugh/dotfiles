;;; 40_neotree.el --- Neotree configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf all-the-icons :ensure t
  :doc "utility package to collect various Icon Fonts"
  :after neotree
  :if (display-graphic-p)
  :config
  (setq all-the-icons-dired-monochrome nil)
  (setq all-the-icons-scale-factor 0.8)
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))


(leaf neotree :ensure t
  :doc "Tree plugin like NerdTree for Vim"
  :after projectile
  :commands neotree-find
  :bind (("<f10>" . my:neotree-find)
	 (:neotree-mode-map
	  ("RET"     . neotree-enter-hide)
	  ("a"       . neotree-hidden-file-toggle)
	  ("<left>"  . neotree-select-up-node)
	  ("<right>" . neotree-change-root)
	  ("<f10>"   . neotree-toggle)))
  :config
  (setq neo-mode-line-type nil)
  (setq neo-keymap-style 'concise)
  (setq neo-create-file-auto-open t)
  (defun my:neotree-find ()
    "Neotree-find with dimmer-off."
    (interactive)
    (dimmer-off)
    (neotree-find))

  (defun neotree-text-scale ()
    "Neotree text scale.
see https://github.com/jaypei/emacs-neotree/issues/218"
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 0.8)
    (message nil))
  (add-hook 'neo-after-create-hook
	    (lambda (_)
	      (call-interactively 'neotree-text-scale)))

  (defun neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide)
    (dimmer-on))

  (defun neotree-enter-hide (&optional arg)
    "Enters file and hides neotree directly"
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_neotree.el ends here
