;;; 60-web.el --- Web mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf web-mode
  :ensure t
  :doc "Web template editing mode for emacs."
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.html?\\'" "\\.php\\'")
  :config
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 60-web.el ends here
