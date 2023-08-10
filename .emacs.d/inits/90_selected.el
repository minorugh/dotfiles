;;; 90_selected.el --- Selected configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf selected
  :doc "Keymap for when region is active"
  :url "http://github.com/Kungsgeten/selected.el"
  :ensure t
  :defun (selected--on region-or-read-string my:ime-off my:ime-on)
  :hook (after-init-hook . selected-global-mode)
  :bind (:selected-keymap
		 (";" . comment-dwim)
		 ("c" . clipboard-kill-ring-save)
		 ("s" . swiper-thing-at-point)
		 ("d" . deepl-translate)
		 ("t" . gts-do-translate)
		 ("W" . my:weblio)
		 ("k" . my:koujien)
		 ("j" . my:eijiro)
		 ("g" . my:google))
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

  (defun my:google (str)
	(interactive (list (region-or-read-string nil)))
	(browse-url (format "https://www.google.com/search?hl=ja&q=%s"
						(upcase (url-hexify-string str)))))

  (defun my:koujien (str)
	"Search koujien."
	(interactive (list (region-or-read-string nil)))
	(browse-url (format "https://sakura-paris.org/dict/広辞苑/prefix/%s"
						(upcase (url-hexify-string str)))))

  (defun my:weblio (str)
	"Search weblio."
	(interactive (list (region-or-read-string nil)))
	(browse-url (format "https://www.weblio.jp/content/%s"
						(upcase (url-hexify-string str)))))

  (defun my:eijiro (str)
	"Search eijiro."
	(interactive (list (region-or-read-string nil)))
	(browse-url (format "https://eow.alc.co.jp/%s/UTF-8/"
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
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 90_selected.el ends here
