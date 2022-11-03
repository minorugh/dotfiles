;;; 06_migemo.el --- Migemo configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf migemo
  :ensure t
  :hook (after-init-hook . migemo-init)
  :when (executable-find "cmigemo")
  :custom
  `((migemo-command . "cmigemo")
	(migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict"))
  :config
  (leaf swiper-migemo
	:doc "https://github.com/tam17aki/swiper-migemo"
	:el-get tam17aki/swiper-migemo
	:config
	(global-swiper-migemo-mode +1))
  (add-to-list 'swiper-migemo-enable-command 'counsel-rg)
  (setq migemo-options '("--quiet" "--nonewline" "--emacs"))
  (migemo-kill)
  (migemo-init))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 06_migemo.el ends here
