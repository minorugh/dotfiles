;;; 20_hydra-pinky.el --- Hydra pinky configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(defhydra hydra-pinky (:color red :hint nil)
  "
  :_SPC_._b_._a_._e_  :_0_._1_._2_._o_._x_  :_[__:__]_  :_-__.__+_  :_s_wiper  :_d_eepl _w_eblio _k_oujien _g_oogle _c_hert
   "
  ;; web serch
  ("d" gts-do-translate)
  ("w" my:weblio)
  ("k" my:koujien)
  ("g" my:google)
  ("c" chromium-vim-chert)
  ("s" swiper)
  ;; move line
  ("SPC" scroll-up-command)
  ("b" scroll-down-command)
  ("a" seq-home)
  ("e" seq-end)
  ;; buffer
  (":" counsel-switch-buffer)
  ("[" winner-undo)
  ("]" winner-redo)
  ;; window
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("." (text-scale-set 0))
  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("x" window-swap-states)
  ("o" other-window-or-split))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_hydra-pinky.el ends here
