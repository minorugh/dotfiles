;;; 03_swiper.el --- Counsel configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf swiper :ensure t
  :bind (("C-s" . swiper-region)
		 ("C-c r" . swiper-thing-at-point))
  :config
  (defun swiper-region ()
    "If region is selected, `swiper-thing-at-point'.
If the region isn't selected, `swiper'."
    (interactive)
    (if (not (use-region-p))
		(swiper)
      (swiper-thing-at-point)))

  (leaf swiper-migemo
	:el-get "tam17aki/swiper-migemo"
	:hook (after-init-hook . global-swiper-migemo-mode)
	:defun migemo-kill
	:config
	(add-to-list 'swiper-migemo-enable-command 'counsel-rg)
	(setq migemo-options '("--quiet" "--nonewline" "--emacs"))
	(migemo-kill)
	(migemo-init)))


(leaf migemo :ensure t
  :if (executable-find "cmigemo")
  :hook (after-init-hook . migemo-init)
  :custom
  `((migemo-command    . "cmigemo")
    (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 03_swiper.el ends here
