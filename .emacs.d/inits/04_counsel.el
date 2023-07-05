;;; 04_counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel
  :doc "Various completion functions using Ivy"
  :url "https://github.com/abo-abo/swiper"
  :ensure t
  :hook (after-init-hook . ivy-mode)
  :bind (("C-:"     . counsel-switch-buffer)
		 ("C-x C-b" . counsel-switch-buffer)
		 ("M-x"     . counsel-M-x)
		 ("M-y"     . counsel-yank-pop)
		 ("C-,"     . counsel-mark-ring)
		 ("C-x C-f" . counsel-find-file)
		 ("C-x C-r" . counsel-recentf))
  :custom
  `((search-default-mode             . nil)
	(ivy-use-virtual-buffers         . t)
	(ivy-use-selectable-prompt       . t)
	(enable-recursive-minibuffers    . t)
	(counsel-find-file-ignore-regexp . (regexp-opt completion-ignored-extensions))
	(ivy-format-functions-alist      . '((t . my:ivy-format-function-arrow))))
  :config
  (defun my:ivy-format-function-arrow (cands)
	"Transform into a string for minibuffer with CANDS."
	(ivy--format-function-generic
	 (lambda (str)
	   (concat (if (display-graphic-p)
				   "")
			   (propertize " " 'display `(space :align-to 2))
			   (ivy--add-face str 'ivy-current-match)))
	 (lambda (str)
	   (concat (propertize " " 'display `(space :align-to 2)) str))
	 cands
	 "\n")))


(leaf ivy-rich
  :doc "More friendly display transformer for ivy"
  :url "https://github.com/Yevgnen/ivy-rich"
  :ensure t
  :hook (after-init-hook . ivy-rich-mode))


(leaf amx
  :ensure t
  :custom
  `((amx-save-file . ,"~/.emacs.d/tmp/amx-items")
	(amx-history-length . 20)))


(leaf prescient
  :doc "Better sorting and filtering"
  :url "https://github.com/raxod502/prescient.el"
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  `((prescient-aggressive-file-save . t)
	(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf projectile
  :doc "Project navigation and management library"
  :url "https://github.com/bbatsov/projectile"
  :ensure t
  :hook (after-init-hook . projectile-mode)
  :custom
  (projectile-known-projects-file . "~/.emacs.d/tmp/projectile.eld"))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 04_counsel.el ends here
