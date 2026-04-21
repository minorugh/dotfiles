;;; 08-highlight.el --- Display highlight configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf paren
  :tag "builtin"
  :doc "Highlight matching parens."
  :hook (after-init-hook . show-paren-mode)
  :config
  (custom-set-faces
   '(show-paren-match ((t (:background "#6272a4" :foreground "#f1fa8c" :weight bold)))))
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(leaf rainbow-delimiters
  :ensure t
  :doc "Display brackets in rainbow."
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf whitespace
  :tag "builtin"
  :doc "Minor mode to visualize whitespace characters."
  :hook (after-init-hook . global-whitespace-mode)
  :bind ("C-c s" . my-cleanup-for-spaces-safe)
  :config
  (setq whitespace-style '(face trailing)) ;; 行末スペースを赤くハイライト
  (defun my-cleanup-for-spaces-safe ()
    "Perform safe whitespace processing on buffer contents.
This is intended for use in the before-save-hook (before-save-hook),
indentation (auto-formatting) is not performed."
    (interactive)
    (delete-trailing-whitespace)           ;; 行末の空白を削除
    (set-buffer-file-coding-system 'utf-8))) ;; 文字コードをUTF-8に設定


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 08-highlight.el ends here
