;;; 05_evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf evil
  :ensure t
  :hook ((after-init-hook . evil-mode)
		 (find-file-hook . my:evil-insert-state))
  :bind (("<zenkaku-hankaku>" . toggle-evil-mode)
		 (:evil-normal-state-map
		  ("?" . chromium-vim-chert)
		  ("C-e" . seq-end)
		  ("SPC" . evil-insert-state)
		  ("M-." . nil)	;; Use with other settings
		  ("<hiragana-katakana>" . my:evil-normal-state)
		  ([home] . open-dashboard))
		 (:evil-visual-state-map
		  ("c" . clipboard-kill-ring-save)
		  ("k" . my:koujien)
		  ("g" . my:google)
		  ("d" . gts-do-translate)))
  :init
  ;; options for Evil, must be written bfore (require 'evil)
  (setq evil-insert-state-cursor '(bar . 4))
  (setq evil-want-C-u-scroll t)	;; Enable scrolling with C-u
  (setq evil-cross-lines t)
  (setq evil-ex-search-vim-style-regexp nil)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Use emacs key bindings in insert state
  (setcdr evil-insert-state-map nil)

  ;; Go back to normal state with ESC
  (define-key evil-insert-state-map [escape] 'my:evil-normal-state)

  ;; Use muhenkan key as ESC
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muhenkan] 'evil-escape-or-quit)

  ;; Set the initial state for major mode
  (evil-set-initial-state 'easy-hugo-mode 'insert)

  ;; Set the initial state for minor mode
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'howm-create-mode-hook 'evil-insert-state)

  ;; Customized functions
  (defun evil-swap-key (map key1 key2)
	"Swap KEY1 and KEY2 in MAP."
	(let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")

  (defun toggle-evil-mode ()
	"Toggle on and off evil mode."
	(interactive)
	(if evil-mode (evil-mode 0)
	  (evil-mode 1)))

  (defun my:evil-normal-state ()
	"Turn off input-method then return to normal-state."
	(interactive)
	(if current-input-method (deactivate-input-method))
	(evil-normal-state))

  (defun evil-escape-or-quit (&optional prompt)
	"If in evil state to ESC, else muhenkan key."
	(interactive)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 (t [muhenkan])))

  (defun my:evil-insert-state ()
	"New files are opened with insert-state."
	(interactive)
	(unless (file-exists-p buffer-file-name)
	  (evil-insert-state)))

  (defvar my:auto-insert-state-buffers '("COMMIT_EDITMSG"))
  (defun ad:switch-to-buffer (&rest _arg)
	"Set buffer for automatic inser-state"
	(when (member (buffer-name) my:auto-insert-state-buffers))
	(evil-insert-state))
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer))


(leaf evil-plugins
  :doc "Plugin for Evil modeline"
  :el-get tarao/evil-plugins
  :after evil
  :require evil-mode-line)


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 05_evil.el ends here
