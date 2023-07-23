;;; 40_neotree.el --- Neotree configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf all-the-icons
  :doc "utility package to collect various Icon Fonts"
  :url "https://github.com/domtronn/all-the-icons.el"
  :ensure t
  :if (display-graphic-p)
  :config
  (unless (member "all-the-icons" (font-family-list))
	(all-the-icons-install-fonts t)))


(leaf neotree
  :doc "Tree plugin like NerdTree for Vim"
  :url "https://github.com/jaypei/emacs-neotree"
  :ensure t
  :defun ((neo-global--select-mru-window)(neo-buffer--execute))
  :bind (("<f10>"    . neotree-find)
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
	(neotree-hide))

  (defun neotree-enter-hide (&optional arg)
	"Enters file and hides neotree directly"
	(interactive "P")
	(neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))



;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 40_neotree.el ends here
