;;; 09_mozc.el --- Japanese mozc configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf mozc :ensure t
  :doc "minor mode to input Japanese with Mozc"
  :hook after-init-hook
  :bind (("<hiragana-katakana>" . my:toggle-input-method)
	 ("s-d" . my:mozc-word-regist)
	 (:mozc-mode-map
	  ("," . (lambda () (interactive) (mozc-insert-str "、")c))
	  ("." . (lambda () (interactive) (mozc-insert-str "。")))))
  :config
  (setq default-input-method     "japanese-mozc")
  (setq mozc-helper-program-name "mozc_emacs_helper")
  (setq mozc-leim-title          "あ")

  (leaf mozc-cursor-color
    :vc (:url "https://github.com/iRi-E/mozc-el-extensions")
    :doc "Set cursor color corresponding to mozc's input state"
    :require t
    :config
    (setq mozc-cursor-color-alist '((direct . "#50fa7b") (hiragana . "#ff5555"))))

  (leaf mozc-popup :ensure t
    :doc "Mozc with popup."
    :hook after-init-hook
    :config
    (setq mozc-candidate-style 'popup))

  (defadvice toggle-input-method (around toggle-input-method-around activate)
    "Input method function in key-chord.el not to be nil."
    (let ((input-method-function-save input-method-function))
      ad-do-it
      (setq input-method-function input-method-function-save)))

  (defun my:toggle-input-method ()
    "If `evil-mode' enabled, set to` emacs-state'."
    (interactive)
    (if (boundp 'evil-mode)
	(evil-insert-state))
    (toggle-input-method))

  (defun mozc-insert-str (str)
    "Immediately confirmed by punctuation."
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
    (unless (string-match "P1" (shell-command-to-string "uname -n"))
      (compile "cp -rf ~/Dropbox/backup/mozc/.mozc ~/"))
    (add-hook 'emacs-startup-hook 'my:mozc-copy)))


;;; 09_mozc.el ends here
