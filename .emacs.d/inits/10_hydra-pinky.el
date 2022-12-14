;;; 10_hydra-pinky.el --- Hydra confuguration for pinky. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

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


(key-chord-define-global
 "::"
 (defhydra hydra-pinky
   (:color red :hint nil)
   "
  :_SPC_:_b_  :_h_._l_._j_._k_  :_0_:_1_:_o_:_x_  :_-__.__+_  _d_iff:_n_:_p_  buffer:_[__:__]_  _f_ile  _s_wiper"
   ;; move page
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("SPC" scroll-up-command)
   ("f" scroll-up-command)
   ("b" scroll-down-command)
   ("<next>" scroll-up-command)
   ("<prior>" scroll-down-command)
   ("g" beginning-of-buffer)
   ("G" end-of-buffer)
   ;; move line
   ("<down>" next-line)
   ("<up>" previous-line)
   ("<right>" forward-char)
   ("<left>" backward-char)
   ("a" seq-home)
   ("e" seq-end)
   ;; ("j" goto-line)
   ("<down>" next-line)
   ("<up>" previous-line)
   ("<right>" forward-char)
   ("<left>" backward-char)
   ;; window
   ("+" text-scale-increase)
   ("-" text-scale-decrease)
   ("." (text-scale-set 0))
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right)
   ("x" window-swap-states)
   ("o" other-window-or-split)
   ;; diff-hl
   ("d" vc-diff)
   ("n" diff-hl-next-hunk)
   ("p" diff-hl-previous-hunk)
   ;; buffer
   (":" counsel-switch-buffer)
   ("[" winner-undo)
   ("]" winner-redo)
   ;; Others
   ("f" counsel-find-file)
   ("s" swiper)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10_hydra-pinky.el ends here
