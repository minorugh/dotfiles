;;; 20_ut.el --- Utilities configurations.
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

(leaf which-key :ensure t
  :doc "Display available keybindings in popup"
  :config
  (setq which-key-max-description-length 40)
  :hook after-init-hook)


(leaf projectile :ensure t
  :doc "Manage and navigate projects in Emacs"
  :config
  (setq projectile-known-projects-file "~/.emacs.d/tmp/projectile.eld")
  :hook after-init-hook)


(leaf prescient :ensure t
  :doc "Better sorting and filtering"
  :hook (after-init-hook . prescient-persist-mode)
  :config
  (setq prescient-aggressive-file-save t)
  (setq prescient-save-file "~/.emacs.d/tmp/prescient-save")
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf popwin :ensure t
  :doc "popup window manager for Emacs"
  :hook after-init-hook)


(leaf aggressive-indent :ensure t
  :doc "Minor mode to aggressively keep your code always indented"
  :hook (emacs-lisp-mode-hook css-mode-hook))


(leaf iedit :ensure t
  :doc "Edit multiple occurrences in the same way simultaneously"
  :bind ("<insert>" . iedit-mode))


(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :hook ((prog-mode-hook)
	 (lisp-interaction-mode-hook . (lambda () (flymake-mode 0))))
  :bind ((prog-mode-map
	  ("M-n" . flymake-goto-next-error)
	  ("M-p" . flymake-goto-prev-error)))
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))


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


(provide '20_ut)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_ut.el ends here
