;;; 20_ut.el --- Counsel ut configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf which-key :ensure t
  :config
  (setq which-key-max-description-length 40)
  :hook after-init-hook)


(leaf counsel-tramp :ensure t
  :config
  (setq tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
  (setq tramp-default-method "scp")
  (setq counsel-tramp-custom-connections
	'(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))


(leaf sequential-command :ensure t
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))


(leaf counsel-web :ensure t
  :bind ("s-w" . counsel-web-search)
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))


(leaf quickrun :ensure t
  :bind ([f6] . quickrun))


(leaf key-chord :ensure t
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log))
  :config
  (setq key-chord-two-keys-delay 0.1)
  :hook after-init-hook)


(leaf iedit :ensure t
  :bind ([insert] . iedit-mode))


(leaf expand-region :ensure t
  :bind ("C-@" . er/expand-region))


(leaf undo-fu :ensure t
  :doc "Undo helper with redo"
  :bind (("C-_" . undo-fu-only-undo)
	 ("C-/" . undo-fu-only-redo)))


(leaf undohist :ensure t
  :doc "Persistent undo history"
  :hook (after-init-hook . undohist-initialize)
  :config
  (setq undohist-directory     "~/.emacs.d/tmp/undohist")
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))


(leaf smartparens :ensure t
  :doc "minor mode for dealing with pairs"
  :hook (after-init-hook . smartparens-global-mode)
  :config (leaf smartparens-config :require t))


(leaf darkroom :ensure t
  :doc "Remove visual distractions and focus on writing"
  :bind (([f8] . my:darkroom-in)
	 (:darkroom-mode-map
	  ([f8] . my:darkroom-out)))
  :config
  (defun my:darkroom-in ()
    "Enter to the `darkroom-mode'."
    (interactive)
    (diff-hl-mode 0)
    (display-line-numbers-mode 0)
    (darkroom-tentative-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing .2)
    (evil-emacs-state))

  (defun my:darkroom-out ()
    "Returns from `darkroom-mode' to the previous state."
    (interactive)
    (darkroom-tentative-mode 0)
    (display-line-numbers-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing 0)
    (evil-normal-state)))


(leaf mail-mode
  :doc "Using mail-mode for eml files for Thunderbird plugin support"
  :mode ("\\.eml\\'" . mail-mode)
  :hook (mail-mode-hook . darkroom-mode))


(leaf *cus-ps-print
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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_ut.el ends here
