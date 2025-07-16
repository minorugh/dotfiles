;;; 06-mozc.el --- Japanese mozc configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf mozc :ensure t
  :defun evil-insert-state mozc-handle-event
  :bind* ("<hiragana-katakana>" . my:toggle-input-method)
  :bind (("s-m" . my:mozc-config)
	 ("s-d" . my:mozc-word-regist)
	 (:mozc-mode-map
	  ("," . (lambda () (interactive) (mozc-insert-str "、")))
	  ("." . (lambda () (interactive) (mozc-insert-str "。")))))
  :init
  (leaf mozc-cursor-color
    :vc (:url "https://github.com/minorugh/mozc-cursor-color")
    :doc "Set cursor color corresponding to mozc's input state."
    :hook (after-init-hook . mozc-cursor-color-setup))

  (leaf mozc-popup :ensure t
    :doc "Mozc with popup."
    :after mozc
    :require t
    :config  (setq mozc-candidate-style 'popup))

  :config
  (setq default-input-method     "japanese-mozc")
  (setq mozc-leim-title          "あ")

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

  ;; The specifications of mozc_helper_emacs and mozc.el have changed.
  ;; If you use mozc_emacs_helper compiled before the specification change with the new mozc.el,
  ;; there is a problem that the suggestion menu is not displayed when you type Japanese.
  ;; Here is some advice for this measure.
  ;; https://w.atwiki.jp/ntemacs/pages/48.html
  ;; ---------------------------------------------------------------------
  (advice-add 'mozc-protobuf-get
	      :around (lambda (orig-fun &rest args)
			(when (eq (nth 1 args) 'candidate-window)
			  (setf (nth 1 args) 'candidates))
			(apply orig-fun args))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 06-mozc.el ends here
