;;; 09-selected.el --- Selected configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf selected :ensure t
  :doc "Keymap for when region is active"
  :url "http://github.com/Kungsgeten/selected.el"
  :defun my:ime-on my:ime-off selected--on region-or-read-string
  :hook (after-init-hook . selected-global-mode)
  :bind (("C-c g" . my:google-this)
	 (:selected-keymap
	  (";" . comment-dwim)
	  ("c" . clipboard-kill-ring-save)
	  ("y" . clipboard-kill-ring-save)
 	  ("s" . swiper-thing-at-point)
	  ("d" . deepl-translate)
 	  ("t" . google-translate-auto)
	  ("w" . my:weblio)
	  ("g" . my:google-this)))
  :config
  (defvar my:ime-flag nil)
  (add-hook 'activate-mark-hook 'my:activate-selected)
  (add-hook 'activate-mark-hook #'(lambda () (setq my:ime-flag current-input-method) (my:ime-off)))
  (add-hook 'deactivate-mark-hook #'(lambda () (unless (null my:ime-flag) (my:ime-on))))
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
	(message ""))))

  (leaf google-this :ensure t
    :doc "Google search at region or under point"
    :config
    (defun my:google-this ()
      "Run without confirmation"
      (interactive)
      (google-this (current-word) t))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 09-selected.el ends here
