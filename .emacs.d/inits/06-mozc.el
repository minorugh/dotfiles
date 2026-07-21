;;; 06-mozc.el --- Japanese mozc configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Mozc Core
;; ============================================================

(leaf mozc
  :ensure t
  :hook (after-init-hook . mozc-mode)
  :bind* (("<hiragana-katakana>" . my-toggle-input-method)
          ("<f13>"               . my-toggle-input-method))
  :bind (("s-m" . my-mozc-config)
         ("s-d" . my-mozc-word-regist)
         (:mozc-mode-map
          ("," . (lambda () (interactive) (mozc-insert-str "、")))
          ("." . (lambda () (interactive) (mozc-insert-str "。")))))
  :config
  (setq default-input-method "japanese-mozc")
  (setq mozc-leim-title       "あ")
  (custom-set-faces
   '(mozc-preedit-selected-face
     ((t (:background "#1E2029" :foreground "#bd93f9" :weight bold)))))

  :preface
  ;; Mozc Helper Commands
  ;; ---------------------------------------------------
  (defun my-toggle-input-method ()
    "Toggle input method only when in Emacs state."
    (interactive)
    (when (evil-emacs-state-p)
      (toggle-input-method)))

  (defun mozc-insert-str (str)
    "Commit current preedit and insert STR immediately."
    (interactive)
    (mozc-handle-event 'enter)
    (insert str))

  (defun my-mozc-config ()
    "Open Mozc config dialog."
    (interactive)
    (start-process "mozc-config" nil "/usr/lib/mozc/mozc_tool" "--mode=config_dialog")
    (delete-other-windows))

  (defun my-mozc-dictionary-tool ()
    "Open Mozc dictionary tool."
    (interactive)
    (start-process "mozc-dict" nil "/usr/lib/mozc/mozc_tool" "--mode=dictionary_tool")
    (delete-other-windows))

  (defun my-mozc-word-regist ()
    "Open Mozc word register dialog."
    (interactive)
    (start-process "mozc-word" nil "/usr/lib/mozc/mozc_tool" "--mode=word_register_dialog")
    (delete-other-windows)))


;; ============================================================
;;  Mozc Candidate Posframe
;; ============================================================

(leaf mozc-cand-posframe
  :ensure t
  :after mozc evil
  :require t
  :init
 :init
  (setq mozc-candidate-style (if (display-graphic-p) 'posframe 'overlay))
  :config
  (custom-set-faces
   '(mozc-cand-posframe-normal-face
     ((t (:background "#1E2029" :foreground "#F8F8F2" :weight normal))))
   '(mozc-cand-posframe-focused-face
     ((t (:background "#393F60" :foreground "#C7C9D1" :weight bold))))
   '(mozc-cand-posframe-footer-face
     ((t (:background "#262626" :foreground "#454D73" :height 0.9))))))


;; ============================================================
;;  Cursor Color by Evil State / Mozc Mode
;; ============================================================

(leaf *my-mozc-cursor
  :after (mozc evil)
  :config
  (setq my-mozc-cursor-color-alist
        '((normal    . "#50fa7b")
          (direct    . "#ffb86c")
          (read-only . "#6272A4")
          (hiragana  . "#B33A3A")
          (visual    . "#F1FA8C")))
  (setq-default my-mozc-current-input-mode 'hiragana)

  ;; Track mozc input mode changes
  (advice-add 'mozc-session-execute-command :after
              (lambda (return-value &rest _)
                (when return-value
                  (let ((mode (mozc-protobuf-get return-value 'mode)))
                    (when mode
                      (setq my-mozc-current-input-mode mode))))))

  ;; Update cursor color every 0.1 sec via idle timer
  (run-with-idle-timer 0.1 t #'my-mozc-cursor-color-update)

  :preface
  (defun my-mozc-cursor-color-update ()
    "Set cursor color according to current Evil state and Mozc mode."
    (set-cursor-color
     (or (cdr (assq (cond
                     ((evil-visual-state-p) 'visual)   ; 最初に評価
                     ((evil-normal-state-p) 'normal)
                     ((and buffer-read-only
                           (not inhibit-read-only)) 'read-only)
                     ((not mozc-mode) 'direct)
                     (t my-mozc-current-input-mode))
                    my-mozc-cursor-color-alist))
         (frame-parameter nil 'foreground-color)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 06-mozc.el ends here
