;;; 30-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Theme
;; ============================================================

(leaf doom-themes
  :ensure t
  :doc "Pack of modern color-themes."
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :config
  (setq doom-themes-enable-italic nil))


;; ============================================================
;;  Cursor & Line Highlight
;; ============================================================

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
  (setq blink-cursor-delay    30))


;; ============================================================
;;  Icons
;; ============================================================

(leaf nerd-icons
  :ensure t
  :if (display-graphic-p))

(leaf nerd-icons-dired
  :ensure t
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :config (setq nerd-icons-scale-factor 0.8))


;; ============================================================
;;  Line Numbers & Fill Column
;; ============================================================

(leaf display-line-numbers
  :tag "builtin"
  :doc "Line numbers.  Toggle with F9; see 07-functions.el."
  :hook ((prog-mode-hook . display-line-numbers-mode)
         (prog-mode-hook . goto-address-prog-mode)
         (text-mode-hook . display-line-numbers-mode)
         (lisp-interaction-mode-hook
          . (lambda () (display-line-numbers-mode 0))))
  :config
  (setq display-line-numbers-width-start t))

(leaf display-fill-column-indicator
  :tag "builtin"
  :doc "Vertical rule at column 79."
  :hook ((gfm-mode-hook  . display-fill-column-indicator-mode)
         (text-mode-hook . display-fill-column-indicator-mode))
  :config
  (setopt display-fill-column-indicator-column 79)
  (setq-default display-fill-column-indicator-character ?│))


;; ============================================================
;;  Parens & Delimiters
;; ============================================================

(leaf paren
  :tag "builtin"
  :doc "Highlight matching parentheses."
  :hook (after-init-hook . show-paren-mode)
  :config
  (custom-set-faces
   '(show-paren-match ((t (:background "#6272a4" :foreground "#f2fa8c" :weight bold)))))
  (setq show-paren-style                  'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(leaf rainbow-delimiters
  :ensure t
  :doc "Colorize nested brackets by depth."
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :hook (prog-mode-hook . rainbow-delimiters-mode))


;; ============================================================
;;  Whitespace
;; ============================================================

(leaf whitespace
  :tag "builtin"
  :doc "Visualize trailing whitespace and provide safe cleanup."
  :hook (((prog-mode-hook markdown-mode-hook) . whitespace-mode)
         (text-mode-hook . (lambda () (whitespace-mode -2))))
  :bind ("C-c s" . my-cleanup-for-spaces-safe)
  :config
  (setq whitespace-style '(face trailing)) ; 行末スペースを赤くハイライト

  (defun my-cleanup-for-spaces-safe ()
    "Delete trailing whitespace and ensure UTF-8 encoding.
Safe for use in `before-save-hook' — does not auto-indent."
    (interactive)
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8)))


;; ============================================================
;;  Doom Modeline
;; ============================================================

(leaf doom-modeline
  :ensure t
  :doc "A minimal and modern mode-line."
  :hook (after-init-hook . doom-modeline-mode)
  :config
  (setq doom-modeline-icon            t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (setq doom-modeline-percent-position nil)
  (line-number-mode   0)
  (column-number-mode 0))

(leaf nyan-mode
  :ensure t
  :hook (doom-modeline-mode-hook . (lambda () (nyan-mode 2)))
  :config
  (setq nyan-animate-nyancat t)
  (setq nyan-bar-length 26))


;; ============================================================
;;  Active Window Highlight  (2ペイン時にアクティブ側を強調)
;;  ポップアップ扱いのウィンドウ（minibuffer / hydra / lv /
;;  Flymake / Compilation / which-key）は実ウィンドウ数から除外。
;;  実ウィンドウが 2 枚以上あるときだけ mode-line を紫ボーダーで強調。
;; ============================================================

(leaf my-active-modeline
  :after doom-modeline
  :doc "Highlight active mode-line with purple border when 2+ windows are visible."
  :config
  (defvar my-modeline-default-bg  nil
    "Default mode-line background captured after theme load.")
  (defvar my-modeline-default-box nil
    "Default mode-line box attribute captured after theme load.")
  (defvar my-modeline-default-bar-bg nil
    "Default doom-modeline-bar background captured after theme load.")

  (defun my-modeline-capture-defaults ()
    "Capture default mode-line face attributes after theme initialization."
    (setq my-modeline-default-bg
          (or (face-background 'mode-line nil t) "unspecified-bg"))
    (setq my-modeline-default-box
          (or (face-attribute 'mode-line :box nil t) 'unspecified))
    (setq my-modeline-default-bar-bg
          (or (face-background 'doom-modeline-bar nil t) "unspecified-bg")))

  (defun my-modeline-popup-window-p (w)
    "Return non-nil if W is a popup that should not count as a real split."
    (or (window-minibuffer-p w)
        (string-match-p
         (rx (or "*hydra"
                 "lv"
                 "*Flycheck errors"
                 "*Compilation"
                 "which-key"
                 "*evil-cheat*"
                 "*Permission Help*"))
         (buffer-name (window-buffer w)))))

  (defvar my-modeline-update-timer nil
    "Pending idle timer for `my-update-modeline-for-split'.")

  (defun my-update-modeline-for-split ()
    "Highlight active mode-line when 2 or more real windows are visible."
    (when (timerp my-modeline-update-timer)
      (cancel-timer my-modeline-update-timer))
    (setq my-modeline-update-timer
          (run-with-idle-timer
           0.2 nil
           (lambda ()
             (setq my-modeline-update-timer nil)
             (let ((wins (cl-count-if-not #'my-modeline-popup-window-p (window-list))))
               (if (>= wins 2)
                   (progn
                     (set-face-attribute 'mode-line nil
                                         :background "#44475a"
                                         :box '(:line-width 2 :color "#bd93f9"))
                     (set-face-attribute 'doom-modeline-bar nil
                                         :background "#bd93f9"))
                 ;; 1ペインに戻ったら確実に復元する
                 (when my-modeline-default-bg
                   (set-face-attribute 'mode-line nil
                                       :background my-modeline-default-bg
                                       :box (or my-modeline-default-box 'unspecified))
                   (set-face-attribute 'doom-modeline-bar nil
                                       :background (or my-modeline-default-bar-bg
                                                       my-modeline-default-bg)))))))))

  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (run-with-idle-timer 2 nil #'my-modeline-capture-defaults)))
  (add-hook 'window-configuration-change-hook #'my-update-modeline-for-split)
  ;; quit-window 等、ウィンドウ「構成」自体は変わらずバッファだけ差し替わる
  ;; ケースを window-configuration-change-hook が拾えないことがあるため、
  ;; より頻繁に発火する buffer-list-update-hook でも再計算する。
  (add-hook 'buffer-list-update-hook #'my-update-modeline-for-split))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 30-ui.el ends here
