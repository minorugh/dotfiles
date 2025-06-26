;;; 07-highlight.el --- Display highlight configulation. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf goggles :ensure t
  :doc "Highlights the modified region using pulse"
  :hook prog-mode-hook text-mode-hook
  :config
  (setq-default goggles-pulse t)
  :custom-face
  (goggles-added    . '((t (:background "#c3fabb"))))
  (goggles-changed  . '((t (:background "#fae8bb"))))
  (goggles-removed  . '((t (:background "#fabfbb")))))

(leaf paren :tag "builtin"
  :doc "Highlight matching parens"
  :hook (after-init-hook . show-paren-mode)
  :custom-face
  (show-paren-match . '((t (:background "#6272a4" :foreground "#f1fa8c" :weight bold))))
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(leaf blink-cursor :tag "Builtin"
  :doc "Blinking cursor mode for GNU Emacs"
  :config
  (setq blink-cursor-blinks   0)
  (setq blink-cursor-interval 0.3)
  (setq blink-cursor-delay    10))

(leaf rainbow-delimiters :ensure t
  :doc "Display brackets in rainbow"
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :hook prog-mode-hook)

(leaf rainbow-mode :ensure t
  :doc "Color letter that indicate the color"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :bind ("C-c r" . rainbow-mode)
  :hook after-init-hook)

(leaf web-mode :ensure t
  :doc "Web template editing mode for emacs"
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :hook (web-mode-hook . my-web-mode-hook)
  :config
  (eval-and-compile
    (setq web-mode-comment-style 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-style-padding 0)
    (setq web-mode-script-padding 0)
    (setq web-mode-block-padding 0)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-expanding t)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-enable-heredoc-fontification t)
    (setq web-mode-enable-current-column-highlight t)))

;;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 07-highlight.el ends here
