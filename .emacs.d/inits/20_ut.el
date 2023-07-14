;;; 20_ut.el --- Counsel ut configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)


(leaf which-key
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom ((which-key-max-description-length . 40)))


(leaf counsel-tramp
  :ensure t
  :custom `((tramp-persistency-file-name . ,"~/.emacs.d/tmp/tramp")
			(tramp-default-method        . "scp")
			(counsel-tramp-custom-connections
			 . '(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/))))


(leaf sequential-command
  :doc "Many commands into one command"
  :url "https://github.com/HKey/sequential-command/blob/master/sequential-command.el"
  :el-get "HKey/sequential-command"
  :config
  (leaf sequential-command-config
	:hook (after-init-hook . sequential-command-setup-keys)))


(leaf counsel-web
  :ensure t
  :bind ("s-w" . counsel-web-search)
  :custom `((counsel-web-search-action . #'browse-url)
			(counsel-web-engine . 'google)
			(counsel-web-search-dynamic-update . t)))


(leaf quickrun
  :ensure t
  :bind ([f6] . quickrun))


(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :custom (key-chord-two-keys-delay . 0.1)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log)))


(leaf *cus-ps-print
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :config
  (setq ps-multibyte-buffer 'non-latin-printer
		ps-paper-type       'a4
		ps-printer-name      nil
		ps-print-header      nil
		ps-print-footer      nil
		ps-font-size         9
		ps-font-family      'Courier
		ps-line-number-font 'Courier
		ps-line-number       t
		ps-show-n-of-n       t)
  (defalias 'ps-mule-header-string-charsets 'ignore))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20_ut.el ends here
