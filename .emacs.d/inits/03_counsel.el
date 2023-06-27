;;; 03_counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel
  :doc "Various completion functions using Ivy"
  :url "https://github.com/abo-abo/swiper"
  :ensure t
  :hook (after-init-hook . ivy-mode)
  :bind (([remap isearch-forward]  . swiper-region)
		 ([remap isearch-backward] . swiper-thing-at-point)
		 ("C-:"     . counsel-switch-buffer)
		 ("M-:"     . counsel-switch-buffer)
		 ("M-x"     . counsel-M-x)
		 ("M-y"     . counsel-yank-pop)
		 ("C-x m"   . counsel-mark-ring)
		 ("C-x C-b" . ibuffer)
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
  (defun swiper-region ()
	"If region is selected, `swiper-thing-at-point'.
If the region isn't selected, `swiper'."
	(interactive)
	(if (not (use-region-p))
		(swiper)
	  (swiper-thing-at-point)))

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
	 "\n"))
  :init
  (leaf ivy-rich
	:doc "More friendly display transformer for ivy"
	:url "https://github.com/Yevgnen/ivy-rich"
	:ensure t
	:hook (after-init-hook . ivy-rich-mode))

  (leaf amx
	:ensure t
	:custom `((amx-save-file . ,"~/.emacs.d/tmp/amx-items")
			  (amx-history-length . 20)))

  (leaf swiper-migemo
	:doc "Use ivy/counsel/swiper with migemo"
	:url "https://github.com/tam17aki/swiper-migemo"
	:el-get tam17aki/swiper-migemo
	:after swiper
	:config
	(global-swiper-migemo-mode +1)
	(add-to-list 'swiper-migemo-enable-command 'counsel-rg)
	(setq migemo-options '("--quiet" "--nonewline" "--emacs"))
	(migemo-kill)
	(migemo-init))

  (leaf migemo
	:doc "Japanese increment search with 'Romanization of Japanese'"
	:url "https://github.com/emacs-jp/migemo"
	:if (executable-find "cmigemo")
	:ensure t
	:hook (after-init-hook . migemo-init)
	:custom
	`((migemo-command    . "cmigemo")
	  (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict"))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 03_counsel.el ends here
