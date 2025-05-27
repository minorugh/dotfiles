;;; 07_mozc.el --- Japanese mozc configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf mozc :ensure t
  :doc "minor mode to input Japanese with Mozc"
  :hook emacs-startup-hook
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

  (defadvice toggle-input-method (around toggle-input-method-around activate)
    "Input method function in key-chord.el not to be nil."
    (let ((input-method-function-save input-method-function))
      ad-do-it
      (setq input-method-function input-method-function-save)))

  (defun my:toggle-input-method ()
    "If `evil-mode' enabled, set to` evil-insert-state'."
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
    (delete-other-windows))

  ;; <2025/05/04 追記>
  ;; 仕様変更前にコンパイルした mozc_emacs_helper を新しい mozc.el で利用した場合、
  ;; 日本語入力時の候補メニューが表示されない問題が発生します。
  ;; この対策のため下記の advice を設定に追加しました。
  (advice-add 'mozc-protobuf-get
	      :around (lambda (orig-fun &rest args)
			(when (eq (nth 1 args) 'candidate-window)
			  (setf (nth 1 args) 'candidates))
			(apply orig-fun args))))


;; mozc extensions
(leaf mozc-cursor-color
  :vc (:url "https://github.com/minorugh/mozc-cursor-color")
  :doc "Set cursor color corresponding to mozc's input state"
  :after mozc
  :require t)

(leaf mozc-popup :ensure t
  :doc "Mozc with popup."
  :after mozc
  :require t
  :config  (setq mozc-candidate-style 'popup))


;;; 07_mozc.el ends here
