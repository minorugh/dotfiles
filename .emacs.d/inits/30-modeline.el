;;; 30-modeline.el --- Modeline configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Doom Modeline
;;; ============================================================

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


;;; ============================================================
;;;  Active Window Highlight  (2ペイン時にアクティブ側を強調)
;;;
;;;  ポップアップ扱いのウィンドウ（minibuffer / hydra / lv /
;;;  Flymake / Compilation / which-key）は実ウィンドウ数から除外。
;;;  実ウィンドウが 2 枚以上あるときだけ mode-line を紫ボーダーで強調。
;;; ============================================================

  (defvar my-modeline-default-bg  nil
    "Default mode-line background captured after theme load.")
  (defvar my-modeline-default-box nil
    "Default mode-line box attribute captured after theme load.")

  (defun my-modeline-capture-defaults ()
    "Capture default mode-line face attributes after theme initialization."
    (setq my-modeline-default-bg  (face-background 'mode-line nil t))
    (setq my-modeline-default-box (face-attribute  'mode-line :box nil t)))

  (defun my-modeline-popup-window-p (w)
    "Return non-nil if W is a popup that should not count as a real split."
    (or (window-minibuffer-p w)
        (string-match-p "\\*hydra\\|lv\\|\\*Flymake\\|\\*Compilation\\|which-key"
                        (buffer-name (window-buffer w)))))

  (defun my-update-modeline-for-split ()
    "Highlight active mode-line when 2 or more real windows are visible."
    (run-with-idle-timer
     0.1 nil
     (lambda ()
       (let ((wins (cl-count-if-not #'my-modeline-popup-window-p (window-list))))
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
