;;; 10_pinky.el --- view-mode & pinky settings  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;;(setq debug-on-error t)

(leaf viewer
  :ensure t
  :require t
  :config
  (viewer-change-modeline-color-setup)
  (setq viewer-modeline-color-view "#852941")
  (bind-key "C-c v" 'view-mode))


(add-hook
 'view-mode-hook
 (lambda ()
   (define-key view-mode-map "<down>" 'next-line)
   (define-key view-mode-map "<up>" 'previous-line)
   (define-key view-mode-map "<right>" 'forward-char)
   (define-key view-mode-map "<left>" 'backward-char)
   (define-key view-mode-map "i" 'View-exit)
   (define-key view-mode-map "," 'View-exit)
   (define-key view-mode-map "g" 'beginning-of-buffer)
   (define-key view-mode-map "G" 'end-of-buffer)
   (define-key view-mode-map "e" 'seq-end)
   (define-key view-mode-map "a" 'seq-home)
   (define-key view-mode-map "b" 'scroll-down-command)
   (define-key view-mode-map "w" 'avy-goto-word-1)
   (define-key view-mode-map "l" 'avy-goto-line)
   (define-key view-mode-map "j" 'goto-line)
   (define-key view-mode-map ";" 'recenter-top-bottom)
   (define-key view-mode-map "o" 'other-window-or-split)
   (define-key view-mode-map "+" 'text-scale-increase)
   (define-key view-mode-map "-" 'text-scale-decrease)
   (define-key view-mode-map "/" '(text-scale-set 0))
   (define-key view-mode-map "0" 'delete-window)
   (define-key view-mode-map "1" 'delete-other-windows)
   (define-key view-mode-map "d" 'vc-diff)
   (define-key view-mode-map "n" 'diff-hl-next-hunk)
   (define-key view-mode-map "p" 'diff-hl-previous-hunk)
   (define-key view-mode-map "s" 'swiper)
   (define-key view-mode-map ":" 'switch-buffer-in-view-mode)
   (define-key view-mode-map "[" 'iflipb-previous-buffer)
   (define-key view-mode-map "]" 'iflipb-next-buffer)
   (define-key view-mode-map "." 'hydra-pinky/body)))


(key-chord-define-global
 "::"
 (defhydra hydra-pinky
   (:color red :hint nil)
   "
  🐳 page:_SPC_:_b_:_;_  goto:_a_:_e_._j_._l_._w_  window:_o_:_0_:___  _d_iff:_n_:_p_  zoom:_-__/__+_  buffer:_[__:__]_  _s_wiper  _v_iew  exit:_,_"
   ;; move page
   ("SPC" scroll-up-command)
   ("f" scroll-up-command)
   ("b" scroll-down-command)
   ("g" beginning-of-buffer)
   ("G" end-of-buffer)
   ;; move line
   ("a" seq-home)
   ("e" seq-end)
   ("j" goto-line)
   ("<down>" next-line)
   ("<up>" previous-line)
   ("<right>" forward-char)
   ("<left>" backward-char)
   ;; misc
   ("i" View-exit :exit t)
   ("," View-exit :exit t)
   (";" recenter-top-bottom)
   ;; window
   ("+" text-scale-increase)
   ("-" text-scale-decrease)
   ("/" (text-scale-set 0))
   ("0" delete-window)
   ("_" delete-other-windows)
   ("d" vc-diff)
   ("n" diff-hl-next-hunk)
   ("p" diff-hl-previous-hunk)
   ;; buffer
   (":" counsel-switch-buffer)
   ("[" iflipb-previous-buffer)
   ("]" iflipb-next-buffer)
   ;; avy
   ("l" avy-goto-line)
   ("w" avy-goto-word-1)
   ;; Others
   ("o" other-window-or-split)
   ("t" direx:jump-to-project-directory)
   ("s" swiper)
   ("v" view-mode)
   ("." nil :color blue)))


(bind-key
 "C-q"
 (defun other-window-or-split ()
   "If there is one window, open split window.
If there are two or more windows, it will go to another window."
   (interactive)
   (when (one-window-p)
	 ;; (split-window-horizontally))
	 (follow-delete-other-windows-and-split))
   (other-window 1)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_pinky.el ends here
