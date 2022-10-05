:;;; 03_counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel
  :ensure t
  :defer-config (ivy-mode)
  :bind (("C-r" . swiper-thing-at-point)
		 ("C-s" . swiper-region)
		 ("C-:" . counsel-switch-buffer)
		 ("s-a" . counsel-ag)
		 ("M-x" . counsel-M-x)
		 ("M-y" . counsel-yank-pop)
		 ("<f6>" . counsel-linux-app)
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
  (leaf ivy-rich
	:ensure t
	:hook (after-init-hook . ivy-rich-mode))
  (leaf amx
	:ensure t
	:custom	`((amx-save-file . ,"~/.emacs.d/tmp/amx-items")
			  (amx-history-length . 20)))
  :preface
  (defun swiper-region ()
	"If region is selected, `swiper-thing-at-point'.
If the region isn't selected, `swiper'."
	(interactive)
	(if (not (use-region-p))
		(swiper)
      (swiper-thing-at-point)))

  ;; Highlight selection candidate with cursor row with icon
  (defun my:ivy-format-function-arrow (cands)
	"Transform into a string for minibuffer with CANDS."
	(ivy--format-function-generic
	 (lambda (str)
	   (concat (if (display-graphic-p)
				   (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
				 ">")
			   (propertize " " 'display `(space :align-to 2))
			   (ivy--add-face str 'ivy-current-match)))
	 (lambda (str)
	   (concat (propertize " " 'display `(space :align-to 2)) str))
	 cands
	 "\n")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 03_counsel.el ends here
