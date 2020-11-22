;;; 10_pinky.el --- view-mode & pinky settings  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;;(setq debug-on-error t)

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
   ("[" previous-buffer)
   ("]" next-buffer)
   ;; avy
   ("l" avy-goto-line)
   ("w" avy-goto-word-1)
   ;; Others
   ("o" other-window-or-split)
   ("t" direx:jump-to-project-directory)
   ("s" swiper)
   ("v" view-mode)
   ("." nil :color blue)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_pinky.el ends here
