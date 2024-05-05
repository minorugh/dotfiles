;;; 20_ut.el --- Counsel ut configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf which-key
  :ensure t
  :config
  (setq which-key-max-description-length 40)
  :hook after-init-hook)

(leaf counsel-tramp
  :ensure t
  :config
  (setq tramp-persistency-file-name ,"~/.emacs.d/tmp/tramp")
  (setq tramp-default-method        "scp")
  (setq counsel-tramp-custom-connections
	'(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))

(leaf sequential-command
  :ensure t
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))

(leaf counsel-web
  :ensure t
  :bind ("s-w" . counsel-web-search)
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))

(leaf quickrun
  :ensure t
  :bind ([f6] . quickrun))

(leaf key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.1)
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log))
  :hook after-init-hook)

(leaf iedit
  :ensure t
  :bind ([insert] . iedit-mode))

(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

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
;; no-byte-compile: t
;; End:
;;; 20_ut.el ends here
