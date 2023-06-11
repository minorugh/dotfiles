;;; 09_ut.el --- Utility command cofiguration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Effective sorting
(leaf prescient
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  `((prescient-aggressive-file-save . t)
	(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))

;; Flymake
(leaf flymake
  :hook (emacs-lisp-mode-hook . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (leaf flymake-posframe
	:el-get Ladicle/flymake-posframe
	:hook (flymake-mode-hook . flymake-posframe-mode)
	:custom
	(flymake-posframe-error-prefix . " ")))

;; imenu-list
(leaf imenu-list
  :ensure t
  :bind ([f2] . imenu-list-smart-toggle)
  :custom
  `((imenu-list-size . 30)
	(imenu-list-auto-resize . t)
	(imenu-list-position . 'left)
	(imenu-list-focus-after-activation . t))
  :config
  (leaf counsel-css
	:ensure t
	:hook (css-mode-hook . counsel-css-imenu-setup)))

;; Html editing
(leaf web-mode
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :custom
  `((web-mode-markup-indent-offset . 2)
	(web-mode-css-indent-offset . 2)
	(web-mode-code-indent-offset . 2)))

;; Key Chord
(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log)
		  ("@@" . howm-list-all))
  :custom
  `((key-chord-two-keys-delay . 0.15)
	(key-chord-safety-interval-backward . 0.1)
	(key-chord-safety-interval-forward  . 0.25)))

;; Sequential-command
(leaf sequential-command
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))

;; PS-printer
(defalias 'ps-mule-header-string-charsets 'ignore)
(setq ps-multibyte-buffer 'non-latin-printer
	  ps-paper-type 'a4
	  ps-font-size 9
	  ;; ps-font-family 'Helvetica
	  ps-font-family 'Courier
	  ps-line-number-font 'Courier
	  ps-printer-name nil
	  ps-print-header nil
	  ps-show-n-of-n t
	  ps-line-number t
	  ps-print-footer nil)

;; Package utils
(leaf package-utils
  :ensure t
  :chord ("p@" . hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
  Package: _i_nstall _r_emove _l_ist up_a_ll    El-get:_u_pdate.re_m_ove
"
   ("i" package-install)
   ("l" package-utils-list-upgrades)
   ("r" package-utils-remove-by-name)
   ("a" package-utils-upgrade-all-and-restart)
   ("u" el-get-update-all)
   ("m" el-get-remove)
   ("<muhenkan>" nil)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 09_ut.el ends here
