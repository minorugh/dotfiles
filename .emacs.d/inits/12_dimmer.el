;;; 12_dimmer.el --- Dimmer configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer
  :ensure t
  :hook ((after-init-hook . dimmer-mode)
		 (focus-out-hook . dimmer-off)
		 (focus-in-hook . dimmer-on))
  :bind ("C-q" . other-window-or-split)
  :config
  (setq dimmer-buffer-exclusion-regexps '("^ \\*which-key\\|^ \\*LV\\|^ \\*.*posframe.*buffer.*\\*$"))
  (setq dimmer-fraction 0.25)
  (with-eval-after-load "dimmer"
	(defun dimmer-off ()
      (dimmer-mode -1)
      (dimmer-process-all))
	(defun dimmer-on ()
      (dimmer-mode 1)
      (dimmer-process-all)))
  :init
  ;; Split window configuration with dimmer control
  (defun other-window-or-split ()
	"If there is one window, open split window.
If there are two or more windows, it will go to another window."
	(interactive)
	(when (one-window-p)
	  (split-window-horizontally))
	(other-window 1)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 12_dimmer.el ends here
