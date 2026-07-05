;;; 90-darkroom.el --- Distraction-free writing mode (built on darkroom.el) -*- lexical-binding: t -*-
;;; Commentary:
;;
;; mutt/markdown/howmなどtext-mode系の日本語文章作成バッファ専用.
;; darkroom-outではline-num/spacingをtext-mode-hookの既定値に固定復元するため、
;; prog-mode系バッファでの使用は想定していない.
;;
;;; Code:
;; (setq debug-on-error t)

(defvar-local my-darkroom-saved-state nil
  "UI state saved by `my-darkroom-in', restored by `my-darkroom-out'.")

(leaf darkroom
  :ensure t
  :doc "Remove visual distractions and focus on writing."
  :config
  (defun my-darkroom-in ()
    "Enter distraction-free mode, remembering the current UI state."
    (interactive)
    (setq-local my-darkroom-saved-state
                (list :line-num (bound-and-true-p display-line-numbers-mode)
                      :whitespace (bound-and-true-p whitespace-mode)
                      :spacing line-spacing))
    (display-line-numbers-mode 0)
    (whitespace-mode -1)
    (setq-local line-spacing 0.2)
    (setq-local darkroom-margins 'darkroom-guess-margins)
    (setq-local darkroom-text-scale-increase 2)
    (darkroom-mode 1)
    (evil-normal-state))

  (defun my-darkroom-out ()
    "Leave distraction-free mode, restoring the UI state saved by `my-darkroom-in'."
    (interactive)
    (darkroom-mode 0)
    (setq text-scale-mode-amount 0)
    (display-line-numbers-mode (if (plist-get my-darkroom-saved-state :line-num) 1 0))
    (whitespace-mode (if (plist-get my-darkroom-saved-state :whitespace) 1 0))
    (setq-local line-spacing (plist-get my-darkroom-saved-state :spacing))
    (deactivate-input-method)
    (setq evil-input-method nil)
    (evil-normal-state))

  (defun my-darkroom-toggle ()
    "Toggle distraction-free mode. Bound to F8 in 07-functions.el."
    (interactive)
    (if (bound-and-true-p darkroom-mode)
        (my-darkroom-out)
      (my-darkroom-in))))


;; (leaf my-neomutt
;;   :doc "NeoMutt integration with emacsclient."
;;   :bind (("C-x C-c" . my-server-edit-and-iconify))
;;   :init
;;   (defun my-server-edit-and-iconify ()
;;     "Finish NeoMutt edit and iconify frame."
;;     (interactive)
;;     (when (bound-and-true-p darkroom-mode)
;;       (my-darkroom-out))
;;     (server-edit)
;;     (kill-buffer)
;;     (iconify-frame)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved make-local)
;; End:
;;; 90-darkroom.el ends here
