;;; 50_view-mode.el --- View-mode configurations. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

;; Change modeline color in view-mode
(leaf viewer
  :ensure t
  :commands viewer-change-modeline-color-setup
  :hook (emacs-startup-hook . viewer-change-modeline-color-setup)
  :init
  (setq viewer-modeline-color-view "#852941")
  (bind-key "C-c v" 'view-mode))

;; Setting to utilize view-mode
(key-chord-define-global "::" 'view-mode)
(add-hook 'view-mode-hook
	  (lambda ()
	    (define-key view-mode-map "i" 'View-exit)
	    (define-key view-mode-map ":" 'View-exit)
	    (define-key view-mode-map "g" 'beginning-of-buffer)
	    (define-key view-mode-map "G" 'end-of-buffer)
	    (define-key view-mode-map "e" 'end-of-line)
	    (define-key view-mode-map "a" 'beginning-of-line)
	    (define-key view-mode-map "c" 'avy-goto-char-2)
	    (define-key view-mode-map "b" 'scroll-down-command)
	    (define-key view-mode-map "D" 'my:view-kill-whole-line)
	    (define-key view-mode-map "u" 'my:view-undo)
	    (define-key view-mode-map "X" 'my:view-del-char)
	    (define-key view-mode-map "w" 'my:view-forward-word+1)
	    (define-key view-mode-map "W" 'backward-word)
	    (define-key view-mode-map "s" 'swiper-isearch-region)
	    (define-key view-mode-map "[" 'forward-list)
	    (define-key view-mode-map "]" 'backward-list)
	    (define-key view-mode-map "l" 'avy-goto-line)
	    (define-key view-mode-map ";" 'recenter-top-bottom)
	    (define-key view-mode-map "t" 'neotree-toggle)
	    (define-key view-mode-map "o" 'other-window-or-split)
	    (define-key view-mode-map "." 'hydra-view-mode/body)))


;; Function to edit in view-mode
(defun my:view-forward-word+1 ()
  "Forward word+1 in view mode."
  (interactive)
  (forward-word)
  (forward-char))
(defun my:view-kill-whole-line ()
  "Kill whole line in view mode."
  (interactive)
  (view-mode 0)
  (kill-whole-line)
  (save-buffer)
  (view-mode 1)
  (message "kill-whole-line and save!"))
(defun my:view-del-char ()
  "Delete character in view mode."
  (interactive)
  (view-mode 0)
  (delete-char 1)
  (save-buffer)
  (view-mode 1)
  (message "delete-char"))
(defun my:view-undo ()
  "Undo in view mode."
  (interactive)
  (view-mode 0)
  (undo)
  (save-buffer)
  (view-mode 1)
  (message "undo and save!"))


;; hydra-view-mode
(defhydra hydra-view-mode (:color pink :hint nil)
  "
╭─────────────────────────────────────────────────────────────────────────^^^^^^^^^^────────┐
│ 🐳 View-mode Function:
  _SPC_: next page       _a_: top of line      _u_: view undo          _o_: other-window
    _b_: prev page       _e_: end of line      _w_: forward word       _t_: neotree
    _g_: page top        _l_: goto line        _W_: backward word      _i_: view exit
    _G_: page end        _D_: delete line      _[_: forward pair       _q_: view quit
    _;_: top-bottom      _X_: delete char      _]_: backward pair      _._: close
└───────────────────────────────────────────────────────────────────^^^^^^^^^^───────────────┘"
  ;; Move page
  ("SPC" scroll-up-command)
  ("b" scroll-down-command)
  ("c" avy-goto-char-2)
  ("g" beginning-of-buffer)
  ("G" end-of-buffer)
  ;; Move line
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("w" my:view-forward-word+1)
  ("W" backward-word)
  ("D" my:view-kill-whole-line)
  ("X" my:view-del-char)
  ("u" my:view-undo)
  ;; Misc
  ("i" View-exit :exit t)
  ("q" View-quit :exit t)
  (":" View-exit :exit t)
  ("[" forward-list)
  ("]" backward-lis)
  ("l" avy-goto-line)
  (";" recenter-top-bottom)
  ;; Others
  ("o" other-window-or-split :exit t)
  ("t" neotree-toggle :exit t)
  ("s" swiper-isearch-region)
  ("." nil :color blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 50_view-mode.el ends here
