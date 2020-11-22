;;; 10_pinky.el --- view-mode & pinky settings  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;;(setq debug-on-error t)

(key-chord-define-global
 "::"
 (defhydra hydra-pinky
   (:color red :hint nil)
   "
  🐳 page:_SPC_:_b_:_l_  goto:_j_._w_  window:_0_:_1_:_2_:_3_:_o_:_x_  _d_iff:_n_:_p_  zoom:_-__.__+_  buffer:_[__:__]_  _s_wiper"
   ;; move page
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
   ("j" goto-line)
   ("<down>" next-line)
   ("<up>" previous-line)
   ("<right>" forward-char)
   ("<left>" backward-char)
   ;; misc
   ("l" recenter-top-bottom)
   ;; window
   ("+" text-scale-increase)
   ("-" text-scale-decrease)
   ("." (text-scale-set 0))
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right)
   ("x" window-swap-states)
   ("d" vc-diff)
   ("n" diff-hl-next-hunk)
   ("p" diff-hl-previous-hunk)
   ;; buffer
   (":" counsel-switch-buffer)
   ("[" previous-buffer)
   ("]" next-buffer)
   ;; avy
   ("w" avy-goto-word-1)
   ;; Others
   ("o" other-window-or-split)
   ("t" direx:jump-to-project-directory)
   ("s" swiper)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 10_hydra-pinky.el ends here
