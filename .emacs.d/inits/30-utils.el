;;; 30-utils.el --- Initialize utilities.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf which-key
  :tag "builtin"
  :doc "Display available keybindings in popup."
  :hook (after-init-hook . which-key-mode)
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-delay 0.0))

(leaf key-chord
  :doc "map pairs of simultaneously pressed keys to commands."
  :vc (:url "https://github.com/minorugh/key-chord")
  :hook (after-init-hook . key-chord-mode)
  :chord (("l;" . init-loader-show-log))
  :config
  (key-chord-define-global ";;" (lambda () (interactive) (execute-kbd-macro (kbd "ESC")))))

(leaf sequential-command
  :doc "Move to first and last line of buffer."
  :vc (:url "https://github.com/minorugh/sequential-command")
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))

(leaf quickrun
  :ensure t
  :doc "Run commands quickly.  Bound to F5; see 10-funcs.el.")

(leaf ps-print
  :doc "PostScript printing with Japanese support."
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :if (executable-find "lpr")
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

(leaf package-update
  :doc "Package management hydra."
  :chord ("p@" . hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
Package: _i_nstall _d_elete _u_pgrade upgrade-_a_ll _v_c-update-all
  "
   ("i" package-install)
   ("u" package-upgrade)
   ("d" package-delete)
   ("a" package-upgrade-all)
   ("v" package-vc-upgrade-all)
   ("<muhenkan>" nil)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 30-utils.el ends here
