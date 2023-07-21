;;; 40_neotree.el --- Neotree configurations. -*- lexical-binding: t -*-
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
  :bind (("<f10>"    . neotree-find)
		 (:neotree-mode-map
		  ("RET"     . neotree-enter-hide)
		  ("a"       . neotree-hidden-file-toggle)
		  ("<left>"  . neotree-select-up-node)
		  ("<right>" . neotree-change-root)
		  ("<f10>"   . neotree-toggle)))
  :custom
  '((neo-mode-line-type        . nil)
	(neo-keymap-style          . 'concise)
	(neo-create-file-auto-open . t))
  :config
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

  (defun neotree-enter-hide ()
	"Enters file and hides neotree directly."
	(interactive)
	(neotree-enter)
	(neotree-hide)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 40_neotree.el ends here
