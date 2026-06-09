;;; 20-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Theme
;;; ============================================================

(leaf doom-themes
  :ensure t
  :doc "Pack of modern color-themes."
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :config
  (setq doom-themes-enable-italic nil))


;;; ============================================================
;;;  Cursor & Line Highlight
;;; ============================================================

(leaf hl-line
  :tag "builtin"
  :doc "Highlight the current line."
  :hook ((after-init-hook . global-hl-line-mode)
         (dashboard-mode-hook
          . (lambda () (setq-local global-hl-line-mode nil))))
  :config
  (custom-set-faces
   '(region  ((t (:background "#6272a4" :extend t))))
   '(hl-line ((t (:background "#3B4252" :extend t))))))

(leaf blink-cursor
  :tag "builtin"
  :doc "Blinking cursor settings."
  :config
  (setq blink-cursor-blinks   0)    ; blink forever
  (setq blink-cursor-interval 0.3)
  (setq blink-cursor-delay    10))


;;; ============================================================
;;;  Icons
;;; ============================================================

(leaf nerd-icons
  :ensure t
  :if (display-graphic-p))

(leaf nerd-icons-dired
  :ensure t
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :config (setq nerd-icons-scale-factor 0.8))


;;; ============================================================
;;;  Line Numbers & Fill Column
;;; ============================================================

(leaf display-line-numbers
  :tag "builtin"
  :doc "Line numbers.  Toggle with F9; see 07-functions.el."
  :hook ((prog-mode-hook . display-line-numbers-mode)
         (prog-mode-hook . goto-address-prog-mode)
         (text-mode-hook . display-line-numbers-mode)
         (lisp-interaction-mode-hook
          . (lambda () (display-line-numbers-mode 0))))
  :config (setq display-line-numbers-width-start t))

(leaf display-fill-column-indicator
  :tag "builtin"
  :doc "Vertical rule at column 79."
  :hook ((gfm-mode-hook  . display-fill-column-indicator-mode)
         (text-mode-hook . display-fill-column-indicator-mode))
  :config
  (setopt display-fill-column-indicator-column 79)
  (setq-default display-fill-column-indicator-character ?│))


;;; ============================================================
;;;  Parens & Delimiters
;;; ============================================================

(leaf paren
  :tag "builtin"
  :doc "Highlight matching parentheses."
  :hook (after-init-hook . show-paren-mode)
  :config
  (custom-set-faces
   '(show-paren-match ((t (:background "#6272a4" :foreground "#f1fa8c" :weight bold)))))
  (setq show-paren-style                  'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(leaf rainbow-delimiters
  :ensure t
  :doc "Colorize nested brackets by depth."
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :hook (prog-mode-hook . rainbow-delimiters-mode))


;;; ============================================================
;;;  Whitespace
;;; ============================================================

(leaf whitespace
  :tag "builtin"
  :doc "Visualize trailing whitespace and provide safe cleanup."
  :hook (after-init-hook . global-whitespace-mode)
  :bind ("C-c s" . my-cleanup-for-spaces-safe)
  :config
  (setq whitespace-style '(face trailing)) ; 行末スペースを赤くハイライト

  (defun my-cleanup-for-spaces-safe ()
    "Delete trailing whitespace and ensure UTF-8 encoding.
Safe for use in `before-save-hook' — does not auto-indent."
    (interactive)
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 20-ui.el ends here
