;;; 30-modeline.el --- Modeline functions. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)


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
    "Default mode-line  color captured after theme load.")

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
	(string-match-p "\\*hydra\\|lv\\|\\*Flymake\\|\\*Compilation\\|which-key"
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
;;; 30-modeline.el ends here
