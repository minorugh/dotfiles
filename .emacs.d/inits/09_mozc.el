;;; 09_mozc.el --- Japanese mozc configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf mozc :ensure t
  :doc "minor mode to input Japanese with Mozc"
  :hook after-init-hook
  :bind* ("<hiragana-katakana>" . my:toggle-input-method)
  :bind (("s-m" . my:mozc-config)
	 ("s-d" . my:mozc-word-regist)
	 (:mozc-mode-map
	  ("," . (lambda () (interactive) (mozc-insert-str "、")))
	  ("." . (lambda () (interactive) (mozc-insert-str "。")))))
  :config
  (setq default-input-method     "japanese-mozc")
  (setq mozc-helper-program-name "mozc_emacs_helper")
  (setq mozc-leim-title          "あ")

  (leaf mozc-cursor-color
    :vc (:url "https://github.com/minorugh/mozc-cursor-color")
    :doc "Set cursor color corresponding to mozc's input state"
    :require t)

  (leaf mozc-popup :ensure t
    :doc "Mozc with popup."
    :require t
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

  (defun my:mozc-config ()
    "Open `mozc-word-regist'."
    (interactive)
    (compile "/usr/lib/mozc/mozc_tool --mode=config_dialog")
    (delete-other-windows))

  (defun my:mozc-word-regist ()
    "Open `mozc-word-regist'."
    (interactive)
    (compile "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")
    (delete-other-windows)))


;; (leaf pangu-spacing :ensure t
;;   :doc "Put a space between Japanese and English"
;;   :url "http://github.com/coldnew/pangu-spacing"
;;   :hook ((markdown-mode-hook text-mode-hook) . pangu-spacing-mode)
;;   :config
;;   (setq pangu-spacing-real-insert-separtor t)
;;   (setq pangu-spacing-include-regexp ;; alphabet only
;; 	(rx (or (and (or (group-n 3 (any "。，！？；：「」（）、"))
;; 			 (group-n 1 (or (category japanese))))))
;; 	    (group-n 2 (in "a-zA-Z")))))


;;; 09_mozc.el ends here
