;;; 90_selected.el --- Selected configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf selected :ensure t
  :doc "Keymap for when region is active"
  :url "http://github.com/Kungsgeten/selected.el"
  :hook (after-init-hook . selected-global-mode)
  :bind (:selected-keymap
	 (";" . comment-dwim)
	 ("c" . clipboard-kill-ring-save)
	 ("s" . swiper-thing-at-point)
	 ("d" . deepl-translate)
	 ("t" . google-translate-auto)
	 ("w" . my:weblio)
	 ("g" . my:google-this))
  :config
  (defvar my:ime-flag nil)
  (add-hook 'activate-mark-hook 'my:activate-selected)
  (add-hook 'activate-mark-hook #'(lambda () (setq my:ime-flag current-input-method) (my:ime-off)))
  (add-hook 'deactivate-mark-hook #'(lambda () (unless (null my:ime-flag) (my:ime-on))))
  (leaf google-this :ensure t)
  :init
  (defun my:activate-selected ()
    (selected-global-mode 1)
    (selected--on)
    (remove-hook 'activate-mark-hook 'my:activate-selected))

  (defun my:ime-on ()
    (interactive)
    (when (null current-input-method)
      (toggle-input-method)))

  (defun my:ime-off ()
    (interactive)
    (deactivate-input-method))

  (defun my:google-this ()
    (interactive)
    (google-this (current-word) t))

  (defun my:weblio (str)
    "Search weblio."
    (interactive (list (region-or-read-string nil)))
    (browse-url (format "https://www.weblio.jp/content/%s"
			(upcase (url-hexify-string str)))))

  (defun region-or-read-string (prompt &optional initial history default inherit)
    "If region is specified, get the string, otherwise call `read-string'."
    (if (not (region-active-p))
	(read-string prompt initial history default inherit)
      (prog1
	  (buffer-substring-no-properties (region-beginning) (region-end))
	(deactivate-mark)
	(message "")))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 90_selected.el ends here
