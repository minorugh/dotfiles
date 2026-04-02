;;; 06-mozc.el --- Japanese mozc configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf mozc
  :ensure t
  :bind* ("<hiragana-katakana>" . my-toggle-input-method)
  :bind (("s-m" . my-mozc-config)
	 ("s-d" . my-mozc-word-regist)
	 (:mozc-mode-map
	  ("," . (lambda () (interactive) (mozc-insert-str "、")))
	  ("." . (lambda () (interactive) (mozc-insert-str "。")))))
  :init
  (leaf mozc-cursor-color
    :vc (:url "https://github.com/minorugh/mozc-cursor-color")
    :doc "Set cursor color corresponding to mozc's input state."
    :hook (after-init-hook . (lambda () (require 'mozc-cursor-color))))

  (leaf mozc-popup :ensure t
    :doc "Mozc with popup."
    :hook (mozc-mode-hook . (lambda () (require 'mozc-popup)))
    :config  (setq mozc-candidate-style 'popup))

  :config
  (setq default-input-method     "japanese-mozc")
  (setq mozc-leim-title          "あ")

  (defun my-toggle-input-method ()
    "If `evil-mode' is enabled, set to `evil-insert-state'."
    (interactive)
    (when (and (boundp 'evil-mode) evil-mode)
      (evil-insert-state))
    (toggle-input-method))

  (defun mozc-insert-str (str)
    "Immediately confirmed by punctuation."
    (interactive)
    (mozc-handle-event 'enter)
    (insert str))

  (defun my-mozc-config ()
    "Open mozc config dialog."
    (interactive)
    (start-process "mozc-config" nil "/usr/lib/mozc/mozc_tool" "--mode=config_dialog")
    (delete-other-windows))

  (defun my-mozc-dictionary-tool ()
    "Open mozc dictionary tool."
    (interactive)
    (start-process "mozc-dict" nil "/usr/lib/mozc/mozc_tool" "--mode=dictionary_tool")
    (delete-other-windows))

  (defun my-mozc-word-regist ()
    "Open mozc word register dialog."
    (interactive)
    (start-process "mozc-word" nil "/usr/lib/mozc/mozc_tool" "--mode=word_register_dialog")
    (delete-other-windows)))

;; ---------------------------------------------------------------------
;; The specifications of mozc_helper_emacs and mozc.el have changed.
;; Advice for using mozc_emacs_helper compiled before the spec change with the new mozc.el
;; https://w.atwiki.jp/ntemacs/pages/48.html

(advice-add 'mozc-protobuf-get
	    :around (lambda (orig-fun &rest args)
		      (when (eq (nth 1 args) 'candidate-window)
			(setf (nth 1 args) 'candidates))
		      (apply orig-fun args)))
;; ---------------------------------------------------------------------

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 06-mozc.el ends here
