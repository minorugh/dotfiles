;;; 05_evil.el --- Evil mode configurations. -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(leaf evil
  :ensure t
  :hook (after-init-hook . evil-mode)
  :bind ((:evil-normal-state-map
		  ("?" . chromium-vim-chert)
		  ("SPC" . evil-insert)
		  ("M-." . nil))
		 (:evil-emacs-state-map
		  ([muhenkan] . evil-normal-state)))
  :init
  ;; Options for Evil, must be written bfore (require 'evil)
  (setq evil-insert-state-cursor '(bar . 4))
  (setq evil-cross-lines t)
  (setq evil-undo-system 'undo-fu)
  :config
  (defalias 'evil-insert-state 'evil-emacs-state)
  ;; (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Use emacs key bindings in insert state
  (setcdr evil-insert-state-map nil)

  ;; Go back to normal state with ESC
  (define-key evil-insert-state-map [escape] 'my:evil-normal-state)

  ;; Use muhenkan key as ESC
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muhenkan] 'evil-escape-or-quit)

  ;; Force evil-emacs-state-modes into major mode
  (dolist (mode '(lisp-interaction-mode
				  fundamental-mode
				  dashboard-mode
				  dired-mode
				  neotree-mode
				  howm-mode
				  git-timemachine-mode
				  easy-hugo-mode))
	(add-to-list 'evil-emacs-state-modes mode))

  ;; Force evil-emacs-state into minor mode
  (add-hook 'org-capture-mode-hook 'evil-emacs-state)
  (add-hook 'magit-blame-mode-hook 'evil-emacs-state)
  (add-hook 'find-file-hook 'evil-emacs-state)

  (defun evil-escape-or-quit (&optional prompt)
	"If in evil state to ESC, else muhenkan key."
	(interactive)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p)
		  (evil-visual-state-p) (evil-replace-state-p))
	  [escape])
	 (t [muhenkan])))

  ;; User custom functions
  (defun my:evil-normal-state ()
	"Turn off input-method then return to normal-state."
	(interactive)
	(if current-input-method (deactivate-input-method))
	(evil-normal-state)
	(if (use-region-p) (keyboard-quit)))

  (defun evil-swap-key (map key1 key2)
	"Swap KEY1 and KEY2 in MAP."
	(let ((def1 (lookup-key map key1))
		  (def2 (lookup-key map key2)))
	  (define-key map key1 def2)
	  (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")

  (defun ad:switch-to-buffer (&rest _arg)
	"Set buffer for automatic insert-state."
	(when (member (buffer-name) '("COMMIT_EDITMSG"))
	  (evil-emacs-state)))
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer)

  (defun chromium-vim-chert ()
	"Chromium vim chert sheet."
	(interactive)
	(browse-url "https://minorugh.github.io/emacs.d/vim-cheat.html")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 05_evil.el ends here
