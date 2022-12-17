;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf evil
  :ensure t
  :hook (after-init-hook  . evil-mode)
  :chord ("::" . toggle-evil-mode)
  :bind ((:evil-normal-state-map
		  ("." . hydra-evil-pinky/body)
		  ("C-e" . seq-end)
		  ("M-." . hydra-quick/body)
		  ([home] . open-dashboard)
		  ([muhenkan] . keyboard-quit)))
  :hydra
  (hydra-evil-pinky
   (:color red :hint nil)
   "
  :: _SPC_._b_._a_._e_  :_0_._1_._2_._o_._s_  :_-__.__+_  _d_eepl  _w_eblio  _k_oujien  _g_oogle  _c_hert
   "
   ;; web serch
   ("d" gts-do-translate)
   ("w" my:weblio)
   ("k" my:koujien)
   ("g" my:google)
   ("c" chromium-vim-chert)
   ;; move line
   ("SPC" scroll-up-command)
   ("b" scroll-down-command)
   ("a" seq-home)
   ("e" seq-end)
   ;; window
   ("+" text-scale-increase)
   ("-" text-scale-decrease)
   ("." (text-scale-set 0))
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("s" window-swap-states)
   ("o" other-window-or-split))
  :init
  (setq evil-cross-lines t)
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Insert State applies all Emacs settings
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
