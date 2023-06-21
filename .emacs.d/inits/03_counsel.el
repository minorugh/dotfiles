;;; 03_counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel
  :doc "Various completion functions using Ivy"
  :url "https://github.com/abo-abo/swiper"
  :ensure t
  :hook (after-init-hook . ivy-mode)
  :bind (([remap isearch-forward] . swiper-region)
		 ([remap isearch-backward] . swiper-thing-at-point)
		 ("C-:" . counsel-switch-buffer)
		 ("M-:" . counsel-switch-buffer)
		 ("s-a" . counsel-ag)
		 ("M-x" . counsel-M-x)
		 ("M-y" . counsel-yank-pop)
		 ("C-x m" . counsel-mark-ring)
		 ("C-x C-b" . ibuffer)
		 ("C-x C-f" . counsel-find-file)
		 ("C-x C-r" . counsel-recentf))
  :custom
  `((search-default-mode . nil)
	(ivy-use-virtual-buffers . t)
	(ivy-use-selectable-prompt . t)
	(enable-recursive-minibuffers . t)
	(counsel-find-file-ignore-regexp . (regexp-opt completion-ignored-extensions))
	(ivy-format-functions-alist . '((t . my:ivy-format-function-arrow))))
  :init
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

  (leaf ivy-rich
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
	(migemo-init)))


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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 03_counsel.el ends here
