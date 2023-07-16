;;; 09_mozc.el --- Japanese mozc configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf mozc
  :doc "Japanese Input Method Editor"
  :url "https://github.com/google/mozc"
  :ensure t
  :hook (after-init-hook . mozc-mode)
  :bind (("<hiragana-katakana>" . my:toggle-input-method)
		 ("s-d" . my:mozc-word-regist)
		 (:mozc-mode-map
		  ("," . (lambda () (interactive) (mozc-insert-str "、")))
		  ("." . (lambda () (interactive) (mozc-insert-str "。")))))
  :custom
  (default-input-method     . "japanese-mozc")
  (mozc-helper-program-name . "mozc_emacs_helper")
  (mozc-leim-title          . "あ")
  :config
  (leaf mozc-cursor-color
	:el-get iRi-E/mozc-el-extensions
	:require t
	:config
	(setq mozc-cursor-color-alist '((direct . "#50fa7b") (hiragana . "#ff5555"))))
  (leaf mozc-cand-posframe
	:ensure t
	;; :after posframe
	:hook (mozc-cand-posframe-hook . (lambda () (interactive) (dimmer-mode -1)))
	:require t
	:custom (mozc-candidate-style . 'posframe )
	:custom-face
	(mozc-cand-posframe-normal-face  . '((t (:background "#1E2029" :foreground "#C7C9D1"))))
	(mozc-cand-posframe-focused-face . '((t (:background "#393F60" :foreground "#C7C9D1"))))
	(mozc-cand-posframe-footer-face  . '((t (:background "#1E2029" :foreground "#454D73")))))

  ;; Define custom
  (defadvice toggle-input-method (around toggle-input-method-around activate)
	"Input method function in key-chord.el not to be nil."
	(let ((input-method-function-save input-method-function))
	  ad-do-it
	  (setq input-method-function input-method-function-save)))

  (defun my:toggle-input-method ()
	"If `evil-mode' enabled, set to` emacs-state'."
	(interactive)
	(if (boundp 'evil-mode)
		(evil-emacs-state))
	(toggle-input-method)
	(if (null current-input-method)
		(dimmer-on)
	  (dimmer-off)))

  (defun mozc-insert-str (str)
	"STR Immediately confirmed by punctuation."
	(interactive)
	(mozc-handle-event 'enter)
	(insert str))

  (defun my:mozc-word-regist ()
	"Open `mozc-word-regist'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")
	(delete-other-windows))

  (defun my:mozc-copy ()
	"Copy mozc to submachines for avoid conflicts."
	(interactive)
	(unless (string-match "e590" (shell-command-to-string "uname -n"))
	  (compile "cp -rf ~/Dropbox/backup/mozc/.mozc ~/"))
	(add-hook 'emacs-startup-hook 'my:mozc-copy)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 09_mozc.el ends here
