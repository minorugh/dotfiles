;;; 07-mozc.el --- Japanese mozc configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf mozc
  :ensure t
  :hook (after-init-hook . mozc-mode)
  :bind* ("<hiragana-katakana>" . my-toggle-input-method)
  :bind (("s-m" . my-mozc-config)
	 ("s-d" . my-mozc-word-regist)
	 (:mozc-mode-map
	  ("," . (lambda () (interactive) (mozc-insert-str "、")))
	  ("." . (lambda () (interactive) (mozc-insert-str "。")))))
  :config
  (setq default-input-method     "japanese-mozc")
  (setq mozc-leim-title          "あ")
  (custom-set-faces
   '(mozc-preedit-selected-face ((t (:background "#1E2029" :foreground "#bd93f9" :weight bold)))))

  (leaf mozc-cand-posframe
    :ensure t
    :after mozc
    :require t
    :init
    (setq mozc-candidate-style 'posframe)
    :config
    (custom-set-faces
     '(mozc-cand-posframe-normal-face ((t (:background "#1E2029" :foreground "#F8F8F2" :weight normal))))
     '(mozc-cand-posframe-focused-face ((t (:background "#393F60" :foreground "#C7C9D1" :weight bold))))
     '(mozc-cand-posframe-footer-face  ((t (:background "#262626" :foreground "#454D73" :height 0.9))))))

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
    (delete-other-windows))

  ;;; --------------------------------
  ;;; mozc-cusor-color
  ;;; --------------------------------
  (setq my-mozc-cursor-color-alist
	'((direct    . "#50fa7b")
          (read-only . "#50fa7b")
          (hiragana  . "#ff9580")))

  (setq-default my-mozc-current-input-mode 'hiragana)

  (advice-add 'mozc-session-execute-command :after
              (lambda (return-value &rest _)
		(when return-value
                  (let ((mode (mozc-protobuf-get return-value 'mode)))
                    (when mode
                      (setq my-mozc-current-input-mode mode))))))

  ;; Periodic update with idle timer (0.1 sec)
  (run-with-idle-timer 0.1 t #'my-mozc-cursor-color-update)

  (defun my-mozc-cursor-color-update ()
    (set-cursor-color
     (or (cdr (assq (cond
                     ((and buffer-read-only (not inhibit-read-only)) 'read-only)
                     ((not mozc-mode) 'direct)
                     (t my-mozc-current-input-mode))
                    my-mozc-cursor-color-alist))
	 (frame-parameter nil 'foreground-color)))))

;;; ---------------------------------------------------------------------
;;; The specifications of mozc_helper_emacs and mozc.el have changed.
;;; Advice for using mozc_emacs_helper compiled before the spec change with the new mozc.el
;;; https://w.atwiki.jp/ntemacs/pages/48.html

;; (advice-add 'mozc-protobuf-get
;; 	    :around (lambda (orig-fun &rest args)
;; 		      (when (eq (nth 1 args) 'candidate-window)
;; 			(setf (nth 1 args) 'candidates))
;; 		      (apply orig-fun args)))
;;; ---------------------------------------------------------------------



(defun my-mozc-temp--on-complete (&rest _)
  "Complete handler: switch to `mozc-mode' after mozc-temp session."
  (advice-remove 'mozc-temp--complete #'my-mozc-temp--on-complete)
  (run-at-time 0 nil #'my-toggle-input-method))

(defun my-mozc-temp ()
  "Run `mozc-temp-convert' then enter `mozc-mode' on completion."
  (interactive)
  (advice-add 'mozc-temp--complete :after #'my-mozc-temp--on-complete)
  (mozc-temp-convert))



;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 07-mozc.el ends here
