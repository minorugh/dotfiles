;;; 30_hydra-pinky.el --- Hydra confuguration for pinky.
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf *pinky
  :chord ("::" . hydra-pinky/body)
  :hydra
  (hydra-pinky
   (:color red :hint nil)
   "
  :_0_._1_._o_._S_  :_-__.__+_  _d_iff:_n_._p_  buffer:_[__:__]_  _f_ind-file  _s_wiper"
   ;; window
   ("+" text-scale-increase)
   ("-" text-scale-decrease)
   ("." (text-scale-set 0))
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("S" window-swap-states)
   ("o" other-window-or-split)
   ;; diff-hl
   ("d" diff-hl-show-hunk)
   ("n" diff-hl-next-hunk)
   ("p" diff-hl-previous-hunk)
   ;; buffer
   (":" counsel-switch-buffer)
   ("[" previous-buffer)
   ("]" next-buffer)
   ;; Others
   ("f" counsel-find-file)
   ("s" swiper-thing-at-point)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 30_hydra-pinky.el ends here
