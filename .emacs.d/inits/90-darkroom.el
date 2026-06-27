;;; 90-darkroom.el --- Distraction-free writing mode. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; A lightweight distraction-free writing mode without the darkroom package.
;; Toggle with F8 to enter or leave.  F1-F12 bindings are centrally managed
;; in 07-functions.el via `leaf external-functions'.
;;
;; Customizable variables:
;;   my-darkroom-margin       -- side margin ratio  (default 0.15)
;;   my-darkroom-text-scale   -- text zoom level    (default 2)
;;   my-darkroom-line-spacing -- line height        (default 0.2)
;;
;;; Code:

;; ============================================================
;;  Darkroom Minor Mode (darkroom package alternative)
;; ============================================================

(defvar-local my-darkroom--saved nil
  "Saved state before entering distraction-free mode.")

(defcustom my-darkroom-margin 0.15
  "Side margin ratio for distraction-free mode.
A float between 0.0 and 0.5 representing the fraction of window width.
Example: 0.15 = 15% margin on each side."
  :type 'float
  :group 'convenience)

(defcustom my-darkroom-text-scale 2
  "Text scale level for distraction-free mode.
Passed to `text-scale-increase'. Default 2 = approx 20% larger."
  :type 'integer
  :group 'convenience)

(defcustom my-darkroom-line-spacing 0.2
  "Line spacing for distraction-free mode.
Passed to `line-spacing'. Default 0.2 = 20% extra spacing."
  :type 'float
  :group 'convenience)


;; ============================================================
;;  Margin Helpers
;; ============================================================

(defun my-darkroom--margin-cols ()
  "Calculate margin width in columns from `my-darkroom-margin'."
  (round (* (window-total-width) my-darkroom-margin)))

(defun my-darkroom--set-margins ()
  "Apply side margins to the current window."
  (let ((m (my-darkroom--margin-cols)))
    (set-window-margins (selected-window) m m)))

(defun my-darkroom--reset-margins ()
  "Reset window margins to zero."
  (set-window-margins (selected-window) 0 0))


;; ============================================================
;;  Mode Definition & Toggle
;; ============================================================

(define-minor-mode my-darkroom-mode
  "Minor mode for distraction-free writing."
  :lighter " Dark"
  (if my-darkroom-mode
      (progn
        (setq my-darkroom--saved
              (list (cons 'mode-line-format   mode-line-format)
                    (cons 'header-line-format header-line-format)))
        (setq-local mode-line-format nil header-line-format nil)
        (text-scale-increase my-darkroom-text-scale)
        (my-darkroom--set-margins)
        (add-hook 'window-configuration-change-hook
                  #'my-darkroom--set-margins nil t))
    (dolist (pair my-darkroom--saved)
      (set (make-local-variable (car pair)) (cdr pair)))
    (setq my-darkroom--saved nil)
    (text-scale-mode -1)
    (my-darkroom--reset-margins)
    (remove-hook 'window-configuration-change-hook
                 #'my-darkroom--set-margins t)))

(defun my-darkroom-in ()
  "Enter distraction-free mode, saving current state."
  (interactive)
  (setq-local my-dark-old-state
              (list :line-num display-line-numbers-mode
                    :spacing  line-spacing))
  (display-line-numbers-mode 0)
  (whitespace-mode -1)
  (setq-local line-spacing my-darkroom-line-spacing)
  (my-darkroom-mode 1)
  (toggle-frame-fullscreen)
  ;; Switch between "evil-normal-state" and "evil-emacs-state" while keeping the IME enabled
  (when (null current-input-method)
    (toggle-input-method))
  (run-with-timer 0.3 nil #'evil-normal-state))

(defun my-darkroom-out ()
  "Leave distraction-free mode and restore previous state."
  (interactive)
  (my-darkroom-mode 0)
  (toggle-frame-fullscreen)
  (whitespace-mode 1)
  (display-line-numbers-mode (if (plist-get my-dark-old-state :line-num) 1 0))
  (setq-local line-spacing (plist-get my-dark-old-state :spacing))
  (deactivate-input-method)
  (setq evil-input-method nil)
  (evil-normal-state))

(defun my-darkroom-toggle ()
  "Toggle distraction-free mode.  Bound to F8; see 07-functions.el."
  (interactive)
  (if my-darkroom-mode
      (my-darkroom-out)
    (my-darkroom-in)))


;; ============================================================
;;  NeoMutt の外部エディタとして Emacs を利用するための設定。
;;
;;  - Compose バッファでは自動的に darkroom を有効化
;;  - Evil は Emacs state に切り替えて入力しやすくする
;;  - 編集終了時に darkroom を解除
;;  - `server-edit' 後は Emacs を最小化し、NeoMutt へフォーカスを戻す
;; ============================================================

(leaf my-neomutt
  :doc "NeoMutt integration with emacsclient."
  :bind (("C-x C-c" . my-server-edit-and-iconify))
  :hook (server-visit-hook . my-neomutt-setup)
  :init
  (defun my-neomutt-setup ()
    "Prepare a NeoMutt compose buffer."
    (when (string-match "neomutt-" (buffer-name))
      (my-darkroom-in)))

  (defun my-server-edit-and-iconify ()
    "Finish NeoMutt edit and iconify frame."
    (interactive)
    (when my-darkroom-mode
      (my-darkroom-out))
    (server-edit)
    (kill-buffer)
    (iconify-frame)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved make-local)
;; End:
;;; 90-darkroom.el ends here
