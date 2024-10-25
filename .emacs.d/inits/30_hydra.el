;;; 30_hydra.el --- Hydra misc configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf package-update
  :chord ("@@" . hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
    Package: _i_nstall _d_elete _u_pgrade upgrade-_a_ll _v_c-update-all
  "
   ("i" package-install)
   ("u" package-upgrade)
   ("d" package-delete)
   ("a" package-upgrade-all)
   ("v" package-vc-upgrade-all)
   ("<muhenkan>" nil)))


(leaf *hydra-markdown
  :hydra
  (hydra-markdown
   (:color red :hint nil)
   "
    Markdown: _i_talic  消線:_x_  ft_n_ote  _t_able  _m_arkup  _v_iew._e_xp._p_df._d_ocx"
   ("i" markdown-insert-italic)
   ("x" markdown-insert-strike-through)
   ("t" markdown-insert-table)
   ("n" markdown-insert-footnote)
   ("m" markdown-toggle-markup-hiding)
   ("v" markdown-preview)
   ("e" markdown-export)
   ("p" md2pdf)
   ("d" md2docx)
   ("<muhenkan>" nil)))


(leaf *pinky
  :chord ("::" . hydra-pinky/body)
  :hydra
  (hydra-pinky
   (:color red :hint nil)
   "
  :_0_._1_._o_._S_._5_._/_  :_-__.__+_  _d_iff:_n_._p_  buffer:_[__:__]_  _f_ind-file  _s_wiper"
   ;; window
   ("+" text-scale-increase)
   ("-" text-scale-decrease)
   ("." (text-scale-set 0))
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("5" make-frame-command)
   ("/" delete-frame)
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


;;; 30_hydra.el ends here
