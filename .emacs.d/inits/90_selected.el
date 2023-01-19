;;; 90_selected.el --- Selected configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf selected
  :ensure t
  :hook (after-init-hook . selected-global-mode)
  :bind (:selected-keymap
		 (";" . comment-dwim)
		 ("c" . clipboard-kill-ring-save)
		 ("s" . swiper-thing-at-point)
		 ("d" . gts-do-translate)
		 ("t" . deepl-translate)
		 ("W" . my:weblio)
		 ("k" . my:koujien)
		 ("j" . my:eijiro)
		 ("g" . my:google))
  :init
  (defvar my:ime-flag nil)
  (add-hook 'activate-mark-hook 'my:activate-selected)
  (add-hook 'activate-mark-hook '(lambda () (setq my:ime-flag current-input-method) (my:ime-off)))
  (add-hook 'deactivate-mark-hook '(lambda () (unless (null my:ime-flag) (my:ime-on))))

  (defun my:activate-selected ()
	"Active selected."
	(selected-global-mode 1)
	(selected--on)
	(remove-hook 'activate-mark-hook #'my:activate-selected))

  (defun my:ime-on ()
	"IME on."
	(interactive)
	(when (null current-input-method) (toggle-input-method)))

  (defun my:ime-off ()
	"IME off."
	(interactive)
	(deactivate-input-method))

  (defun my:google (str)
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://www.google.com/search?hl=ja&q=%s"
						(upcase (url-hexify-string str)))))

  (defun my:koujien (str)
	"Open koujien with STR."
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://sakura-paris.org/dict/広辞苑/prefix/%s"
						(upcase (url-hexify-string str)))))

  (defun my:weblio (str)
	"Open weblio with STR."
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://www.weblio.jp/content/%s"
						(upcase (url-hexify-string str)))))

  (defun my:eijiro (str)
	"Open eijiro with STR."
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://eow.alc.co.jp/%s/UTF-8/"
						(upcase (url-hexify-string str)))))

  (defun my:get-region (r)
	"Get search word from region with R."
	(buffer-substring-no-properties (region-beginning) (region-end))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 90_selected.el ends here
