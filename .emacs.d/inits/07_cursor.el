;;; 07_cursor.el --- Cursor Utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Sequential-command
(leaf sequential-command
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))


;; Extension for region
(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


;; point-history
;; (leaf point-history
;;   :el-get blue0513/point-history
;;   :hook (after-init-hook . point-history-mode)
;;   :chord ("gg" . point-history-show) ;; Since it disappears with `g'
;;   :bind ((:point-history-show-mode-map
;; 		  ("<SPC>" . point-history-next-line)
;; 		  ("b" . point-history-prev-line)))
;;   :custom
;;   `((point-history-show-buffer-height . 15)
;; 	(point-history-ignore-buffer . "^ \\*Minibuf\\|^*\\|^ \\*point-history-show*\\|^magit\\|\]$")))


;; Like as '%' of vim. Suitable for use in view mode
(defun my:jump-brace ()
  "Jump to the corresponding parenthesis."
  (interactive)
  (let ((c (following-char))
		(p (preceding-char)))
	(if (eq (char-syntax c) 40) (forward-list)
	  (if (eq (char-syntax p) 41) (backward-list)
		(backward-up-list)))))


;; Return to last edit point
(leaf *exchange-point-and-mark
  :bind ("C-x C-x" . my:exchange-point-and-mark)
  :init
  (defun my:exchange-point-and-mark ()
	"No mark active `exchange-point-and-mark'."
	(interactive)
	(exchange-point-and-mark)
	(deactivate-mark)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 07_cursor.el ends here
