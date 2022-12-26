;;; 05_evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf evil
  :ensure t
  :hook (after-init-hook  . evil-mode)
  :bind ("<zenkaku-hankaku>" . toggle-evil-mode)
  :bind ((:evil-normal-state-map
		  ("?" . chromium-vim-chert)
		  ("C-e" . seq-end)
		  ("SPC" . evil-insert-state)
		  ("M-." . nil)	;; Use with other settings
		  ([home] . open-dashboard)))
  :init
  ;; options for Evil, must be written before (require 'evil)
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
  ;; Hydra-select in visual-state region
  (define-key evil-visual-state-map (kbd ".") 'hydra-selected/body)

  ;; Use muhenkan key as ESC
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muhenkan] 'evil-escape-or-quit)

  ;; Set the initial state for major mode
  (evil-set-initial-state 'lisp-interaction-mode 'insert)
  (evil-set-initial-state 'fundamental-mode 'insert)
  (evil-set-initial-state 'easy-hugo-mode 'insert)

  ;; Set the major mode to run in emacs-state
  (add-to-list 'evil-emacs-state-modes 'neotree-mode)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)

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

  ;; New files are opened with insert-state
  (add-hook 'find-file-hook
			(lambda ()
			  (unless (file-exists-p buffer-file-name)
				(evil-insert-state))))

  ;; In COMMIT_EDITMSG buffer, make insert-state
  (defun ad:switch-to-buffer (&rest _arg)
	(when (member (buffer-name) my:auto-view-buffers))
	(evil-insert-state))
  (defvar my:auto-view-buffers '("COMMIT_EDITMSG"))
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer)

  ;; Modeline plugin for Evil
  (leaf evil-plugins
	:el-get tarao/evil-plugins
	:require evil-mode-line))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 05_evil.el ends here
