;;; 90-darkroom.el --- Distraction-free writing mode. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; A lightweight distraction-free writing mode without the darkroom package.
;; Toggle with F8 to enter or leave.  F1-F12 bindings are centrally managed
;; in 10-funcs.el via `leaf external-functions'.
;;
;; Customizable variables:
;;   my-darkroom-margin       -- side margin ratio (default 0.15)
;;   my-darkroom-text-scale   -- text zoom level  (default 2)
;;   my-darkroom-line-spacing -- line height       (default 0.2)
;;
;;; Code:

(leaf my-darkroom
  :doc "Distraction-free writing mode (darkroom package alternative)."
  :init
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

  (defun my-darkroom--margin-cols ()
    "Calculate margin width in columns from `my-darkroom-margin'."
    (round (* (window-total-width) my-darkroom-margin)))

  (defun my-darkroom--set-margins ()
    "Apply margins to the current window."
    (let ((m (my-darkroom--margin-cols)))
      (set-window-margins (selected-window) m m)))

  (defun my-darkroom--reset-margins ()
    "Reset window margins to zero."
    (set-window-margins (selected-window) 0 0))

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
    "Save current settings and enter distraction-free mode."
    (interactive)
    (setq-local my-dark-old-state
                (list :line-num display-line-numbers-mode
                      :spacing line-spacing))
    (display-line-numbers-mode 0)
    (setq-local line-spacing my-darkroom-line-spacing)
    (my-darkroom-mode 1)
    (toggle-frame-fullscreen))

  (defun my-darkroom-out ()
    "Leave distraction-free mode and restore previous settings."
    (interactive)
    (my-darkroom-mode 0)
    (toggle-frame-fullscreen)
    (display-line-numbers-mode (if (plist-get my-dark-old-state :line-num) 1 0))
    (setq-local line-spacing (plist-get my-dark-old-state :spacing)))

  (defun my-darkroom-toggle ()
    "Toggle distraction-free mode.
Bound to F8; see 10-functions.el."
    (interactive)
    (if my-darkroom-mode
        (my-darkroom-out)
      (my-darkroom-in))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved make-local)
;; End:
;;; 90-darkroom.el ends here
