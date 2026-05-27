;;; 30-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :ensure t
  :doc "Pack of modern color-themes."
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :config
  (setq doom-themes-enable-italic nil)
  (doom-themes-org-config)
  (with-eval-after-load 'doom-dracula
    (doom-themes-vertico-config)))

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
  :doc "Blinking cursor mode for GNU Emacs."
  :config
  (setq blink-cursor-blinks   0)
  (setq blink-cursor-interval 0.3)
  (setq blink-cursor-delay    10))

(leaf nerd-icons
  :ensure t
  :if (display-graphic-p))

(leaf nerd-icons-dired
  :ensure t
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :config (setq nerd-icons-scale-factor 0.8))

(leaf display-line-numbers
  :tag "builtin"
  :doc "Interface for display-line-numbers."
  ;; Toogle display-line-numbers-mode
  ;; Bound to F9; see 10-functions.el
  :hook ((prog-mode-hook . display-line-numbers-mode)
         (prog-mode-hook . goto-address-prog-mode)
         (text-mode-hook . display-line-numbers-mode)
         (lisp-interaction-mode-hook
          . (lambda () (display-line-numbers-mode 0))))
  :config (setq display-line-numbers-width-start t))

(leaf display-fill-column-indicator
  :tag "builtin"
  :doc "Indicate maximum column."
  :hook ((gfm-mode-hook  . display-fill-column-indicator-mode)
         (text-mode-hook . display-fill-column-indicator-mode))
  :config
  (setopt display-fill-column-indicator-column 79)
  (setq-default display-fill-column-indicator-character ?│))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-modeline
;; Active mode-line highlight for 2-pane layout (doom-dracula)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf doom-modeline
  :ensure t
  :doc "A minimal and modern mode-line."
  :hook (after-init-hook . doom-modeline-mode)
  :config
  (setq doom-modeline-icon            t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (line-number-mode   0)
  (column-number-mode 0)
  (defvar my-modeline-default-bg nil
    "Default mode-line background color captured after theme load.")

  (defvar my-modeline-default-box nil
    "Default mode-line box attribute captured after theme load.")

  (defun my-modeline-capture-defaults ()
    "Capture default mode-line face attributes after theme initialization."
    (setq my-modeline-default-bg  (face-background 'mode-line nil t))
    (setq my-modeline-default-box (face-attribute  'mode-line :box nil t)))

  (defun my-modeline-popup-window-p (w)
    "Return non-nil if W is a popup that should not count as a real split.
Excludes minibuffer, hydra, lv, and Flymake diagnostics windows."
    (or (window-minibuffer-p w)
	(string-match-p "\\*hydra\\|lv\\|\\*Flymake\\|\\*compilation\\|which-key"
			(buffer-name (window-buffer w)))))

  (defun my-update-modeline-for-split ()
    "Highlight active mode-line when 2 or more real windows are shown.
Popup windows such as minibuffer, hydra, lv, and Flymake diagnostics
are excluded from the window count."
    (run-with-idle-timer 0.1 nil
			 (lambda ()
			   (let ((wins (cl-count-if-not #'my-modeline-popup-window-p
							(window-list))))
			     (if (> wins 1)
				 (progn
				   (set-face-attribute 'mode-line nil
						       :background "#44475a"
						       :box '(:line-width 2 :color "#bd93f9"))
				   (set-face-attribute 'doom-modeline-bar nil
						       :background "#bd93f9"))
			       (when (and my-modeline-default-bg my-modeline-default-box)
				 (set-face-attribute 'mode-line nil
						     :background my-modeline-default-bg
						     :box my-modeline-default-box)
				 (set-face-attribute 'doom-modeline-bar nil
						     :background "#bd93f9")))))))

  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (run-with-idle-timer 1 nil #'my-modeline-capture-defaults)))
  (add-hook 'window-configuration-change-hook #'my-update-modeline-for-split))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 30-ui.el ends here
