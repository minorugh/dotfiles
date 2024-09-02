;;; 30_hydra-misc.el --- hydra misc mode configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf package-utils :ensure t
  :chord ("p@" . hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
    Package: _i_nstall _r_emove _l_ist up_a_ll    el-get:_u_pdate.re_m_ove
  "
   ("i" package-install)
   ("l" package-utils-list-upgrades)
   ("r" package-utils-remove-by-name)
   ("a" package-utils-upgrade-all-and-restart)
   ("u" el-get-update-all)
   ("m" el-get-remove)
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
;;; 30_hydra-misc.el ends here
