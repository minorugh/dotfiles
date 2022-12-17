;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf evil
  :ensure t
  :hook (after-init-hook  . evil-mode)
  :chord ("::" . toggle-evil-mode)
  :bind ((:evil-normal-state-map
		  ("?" . chromium-vim-chert)
		  ("." . hydra-evil-selected/body)
		  ("o" . other-window-or-split)
		  ("C-e" . seq-end)
		  ("M-." . hydra-quick/body)
		  ([home] . open-dashboard)
		  ([muhenkan] . keyboard-quit)))
  :hydra
  (hydra-evil-selected
   (:color red :hint nil)
   "
: _s_wiper  _d_eepl  _w_eblio  _k_oujien  _e_ijoro  _g_oogle
"
   ("s" swiper-thing-at-point)
   ("d" gts-do-translate)
   ("w" my:weblio)
   ("k" my:koujien)
   ("e" my:eijiro)
   ("g" my:google))
  :init
  (setq evil-cross-lines t)
  (setq evil-undo-system 'undo-fu)
  :config
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'my:evil-normal-state)
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muhenkan] 'evil-escape-or-quit)

  ;; Set the initial state for major mode
  (evil-set-initial-state 'lisp-interaction-mode 'insert)
  (evil-set-initial-state 'fundamental-mode 'insert)
  (evil-set-initial-state 'text-mode 'insert)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)

  (defun toggle-evil-mode ()
	"Toggle on and off evil mode."
	(interactive)
	(if evil-mode (evil-mode 0)
	  (evil-mode 1)))

  (defun turn-off-input-method ()
	"If input-method is on, turn it off."
	(interactive)
	(if current-input-method (deactivate-input-method)))

  (defun my:evil-normal-state ()
	"Turn off input-method and return to normal-state."
	(interactive)
	(turn-off-input-method)
	(evil-normal-state))

  (defun evil-escape-or-quit (&optional prompt)
	"If in evil any state to escape key, else muhenkan key."
	(interactive)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 (t [muhenkan])))

  (defun chromium-vim-chert ()
	"Chromium vim chert sheet."
	(interactive)
	(browse-url "https://vim.rtorr.com/lang/ja")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
