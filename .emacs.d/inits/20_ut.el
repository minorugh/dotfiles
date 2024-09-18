;;; 20_ut.el --- Counsel ut configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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

(leaf quickrun :ensure t
  :bind ([f6] . quickrun))

(leaf key-chord :ensure t
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log))
  :config
  (setq key-chord-two-keys-delay 0.1)
  :hook after-init-hook)

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

(leaf elec-pair
  :doc "Automatic parenthesis pairing"
  :tag "Builtin"
  :hook (after-init-hook . electric-pair-mode)
  :config
  (defadvice electric-pair-post-self-insert-function
      (around electric-pair-post-self-insert-function-around activate)
    "Don't insert the closing pair in comments or strings"
    (unless (nth 8 (save-excursion (syntax-ppss (1- (point)))))
      ad-do-it)))
					;
(leaf ps-mule
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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_ut.el ends here
