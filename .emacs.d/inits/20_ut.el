;;; 20_ut.el --- Utilities configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel-tramp :ensure t
  :config
  (setq tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
  (setq tramp-default-method "scp")
  (setq counsel-tramp-custom-connections
	'(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))

(leaf sequential-command
  :vc (:url "https://github.com/HKey/sequential-command")
  :doc "Move to first and last line of buffer"
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))

(leaf imenu-list :ensure t
  :doc "Show imenu entries in a separate buffer"
  :bind ([f2]  . imenu-list-smart-toggle)
  :config
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position    'left)
  :init
  (leaf counsel-css :ensure t
    :doc "stylesheet-selector-aware swiper"
    :hook (css-mode-hook . counsel-css-imenu-setup)))

(leaf web-mode :ensure t
  :doc "Web template editing mode for emacs"
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset   2))

(leaf ediff
  :doc "Edit while viewing the difference"
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-split-window-function 'split-window-horizontally
	ediff-diff-options "-twB"))

(leaf ps-mule
  :doc "provide multi-byte character facility to ps-print"
  :tag "Builtin"
  :if (executable-find "lpr")
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :config
  (setq ps-multibyte-buffer 'non-latin-printer)
  (setq ps-paper-type       'a4)
  (setq ps-printer-name      nil)
  (setq ps-print-header      nil)
  (setq ps-print-footer      nil)
  (setq ps-font-size         9)
  (setq ps-font-family      'Courier)
  (setq ps-line-number-font 'Courier)
  (setq ps-line-number       t)
  (setq ps-show-n-of-n       t)
  (defalias 'ps-mule-header-string-charsets 'ignore))


;;; 20_ut.el ends here
