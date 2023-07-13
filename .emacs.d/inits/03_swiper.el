;;; 03_swiper.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf swiper
  :doc "Isearch with an overview. Oh, man!"
  :url "https://github.com/abo-abo/swiper"
  :ensure t
  :bind (("C-s" . swiper-region)
		 ("C-r" . swiper-thing-at-point))
  :config
  (defun swiper-region ()
    "If region is selected, `swiper-thing-at-point'.
If the region isn't selected, `swiper'."
    (interactive)
    (if (not (use-region-p))
		(swiper)
      (swiper-thing-at-point))))


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
    (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 03_swiper.el ends here
