;;; 90-darkroom.el --- Distraction-free writing mode (built on darkroom.el) -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

(leaf darkroom
  :ensure t
  :doc "Remove visual distractions and focus on writing."
  :config
  (defun my-darkroom-in ()
    "Enter distraction-free mode, saving current state."
    (interactive)
    (setq-local my-darkroom--old-state
                (list :line-num display-line-numbers-mode
                      :spacing  line-spacing
                      :input-method current-input-method))
    (display-line-numbers-mode 0)
    (whitespace-mode -1)
    (setq-local line-spacing 0.2)
    (setq-local darkroom-margins 'darkroom-guess-margins)
    (setq-local darkroom-text-scale-increase 2)
    (darkroom-mode 1)
    (toggle-frame-fullscreen)
    ;; mutt から emacsclient(server-visit) 経由で起動した場合、
    ;; 即時に evil-normal-state を呼んでも反映されないため遅延させる.
    (run-with-timer 0.3 nil #'evil-normal-state))

(defun my-darkroom-out ()
    "Leave distraction-free mode."
    (interactive)
    (darkroom-mode 0)
    (toggle-frame-fullscreen)
    (whitespace-mode 1)
    (display-line-numbers-mode 1)   ;; text-mode-hook の既定値に固定復元
    (setq-local line-spacing nil)
    (when current-input-method
      (deactivate-input-method))
    (setq evil-input-method nil)
    (evil-normal-state))

  (defun my-darkroom-toggle ()
    "Toggle distraction-free mode. Bound to F8 in 07-functions.el."
    (interactive)
    (if (bound-and-true-p darkroom-mode)
        (my-darkroom-out)
      (my-darkroom-in))))


;; ============================================================
;;  NeoMutt の外部エディタとして Emacs を利用するための設定.
;;  Compose バッファで自動的に darkroom を有効化し、
;;  編集終了時に解除して NeoMutt へフォーカスを戻す.
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
    (when darkroom-mode
      (my-darkroom-out))
    (server-edit)
    (kill-buffer)
    (iconify-frame)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved make-local)
;; End:
;;; 90-darkroom.el ends here

