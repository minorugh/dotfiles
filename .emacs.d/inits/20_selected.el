;;; 20_selected.el --- Select configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf selected
  :ensure t
  :global-minor-mode selected-global-mode
  :config
  (bind-key ";" 'comment-dwim selected-keymap)
  (bind-key "d" 'clipboard-kill-region selected-keymap)
  (bind-key "f" 'describe-function selected-keymap)
  (bind-key "v" 'describe-variable selected-keymap)
  (bind-key "c" 'cool-copy selected-keymap)
  (bind-key "i" 'iedit-mode selected-keymap)
  (bind-key "s" 'swiper-thing-at-point selected-keymap)
  (bind-key "k" 'my:koujien selected-keymap)
  (bind-key "e" 'my:eijiro selected-keymap)
  (bind-key "w" 'my:weblio selected-keymap)
  (bind-key "t" 'google-translate-auto selected-keymap)
  (bind-key "T" 'chromium-translate selected-keymap)
  (bind-key "g" 'my:google selected-keymap))


(leaf user-function-selected
  :init
  ;; Control mozc when seleceted
  (defun my:activate-selected ()
	(selected-global-mode 1)
	(selected--on)
	(remove-hook 'activate-mark-hook #'my:activate-selected))
  (add-hook 'activate-mark-hook #'my:activate-selected)
  (defun my:ime-on ()
	(interactive)
	(when (null current-input-method) (toggle-input-method)))
  (defun my:ime-off ()
	(interactive)
	(inactivate-input-method))

  (defvar my:ime-flag nil)
  (add-hook
   'activate-mark-hook
   #'(lambda ()
	   (setq my:ime-flag current-input-method) (my:ime-off)))
  (add-hook
   'deactivate-mark-hook
   #'(lambda ()
	   (unless (null my:ime-flag) (my:ime-on))))

  (defun my:koujien (str)
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://sakura-paris.org/dict/広辞苑/prefix/%s"
						(upcase (url-hexify-string str)))))

  (defun my:weblio (str)
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://www.weblio.jp/content/%s"
						(upcase (url-hexify-string str)))))

  (defun my:eijiro (str)
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://eow.alc.co.jp/%s/UTF-8/"
						(upcase (url-hexify-string str)))))

  (defun my:google (str)
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://www.google.com/search?hl=ja&q=%s"
						(upcase (url-hexify-string str)))))

  (defun my:get-region (r)
	"Get search word from region."
	(buffer-substring-no-properties (region-beginning) (region-end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20_selected.el ends here
