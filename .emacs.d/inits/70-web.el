;;; 70-web.el --- Web mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Simple web-mode setup for color checking and manual formatting.
;;; Toggle between web-mode and default mode with s-w.
;;; Code:
;; (setq debug-on-error t)

(leaf web-mode
  :ensure t
  ;; :mode ("\\.html?\\'" "\\.css\\'")
  :mode ("\\.html?\\'")
  :bind (("s-w"    . my-toggle-web-mode))
  :config
  (setq web-mode-enable-auto-indentation nil)  ;; 勝手な整形を防止
  (setq web-mode-enable-css-colorization t)    ;; 色を表示
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :init
  (defun my-toggle-web-mode ()
    "Simple switch between `web-mode' and original mode."
    (interactive)
    (if (derived-mode-p 'web-mode)
	(set-auto-mode)
      (web-mode))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 70-web.el ends here
