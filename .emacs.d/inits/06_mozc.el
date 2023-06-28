;;; 06_mozc.el --- Japanese mozc configurations. -*- lexical-binding: t -*-
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
	:url "https://github.com/iRi-E/mozc-el-extensions"
	:el-get iRi-E/mozc-el-extensions
	:require t
	:config
	(setq mozc-cursor-color-alist '((direct . "#50fa7b") (hiragana . "#ff5555"))))
  (leaf posframe
	:ensure t
	:when window-system
	:config
	(leaf mozc-cand-posframe
	  :doc "Posframe Frontend for Mozc.el"
	  :url "https://github.com/akirak/mozc-posframe"
	  :ensure t
	  :require t
	  :custom (mozc-candidate-style . 'posframe)
	  :custom-face
	  (mozc-cand-posframe-normal-face  . '((t (:background "#1E2029" :foreground "#C7C9D1"))))
	  (mozc-cand-posframe-focused-face . '((t (:background "#393F60" :foreground "#C7C9D1"))))
	  (mozc-cand-posframe-footer-face  . '((t (:background "#1E2029" :foreground "#454D73"))))))

  ;; (leaf pangu-spacing
  ;; 	:doc "Put a space between Japanese and English"
  ;; 	:url "http://github.com/coldnew/pangu-spacing"
  ;; 	:ensure t
  ;; 	:hook ((markdown-mode-hook text-mode-hook) . pangu-spacing-mode)
  ;; 	:config
  ;; 	(setq pangu-spacing-real-insert-separtor t)
  ;; 	(setq pangu-spacing-include-regexp ;; alphabet only
  ;; 		  (rx (or (and (or (group-n 3 (any "。，！？；：「」（）、"))
  ;; 						   (group-n 1 (or (category japanese))))))
  ;; 			  (group-n 2 (in "a-zA-Z")))))

  (defadvice toggle-input-method (around toggle-input-method-around activate)
	"Input method function in key-chord.el not to be nil."
	(let ((input-method-function-save input-method-function))
	  ad-do-it
	  (setq input-method-function input-method-function-save)
	  (dimmer-mode -1)))

  (defun my:toggle-input-method ()
	"If `evil-mode' enabled, set to` emacs-state'."
	(interactive)
	(if (boundp 'evil-mode)
		(evil-emacs-state))
	(toggle-input-method))

  (defun mozc-insert-str (str)
	"STR Immediately confirmed by punctuation."
	(interactive)
	(mozc-handle-event 'enter)
	(insert str))

  (defun my:mozc-word-regist ()
	"Open `mozc-word-regist'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")
	(delete-other-windows)))


;; --------------------------------------------------------------------
;; Sharring Mozc with submachine
;; --------------------------------------------------------------------
(leaf *sharring-mozc
  :doc "Copy main latest mozc at submachine startup"
  :hook (emacs-startup-hook . my:mozc-copy)
  :init
  (defun my:mozc-copy ()
	"Copy mozc to submachines for avoid conflicts."
	(interactive)
	(unless (string-match "e590" (shell-command-to-string "uname -n"))
	  (compile "cp -rf ~/Dropbox/backup/mozc/.mozc ~/"))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 06_mozc.el ends here
