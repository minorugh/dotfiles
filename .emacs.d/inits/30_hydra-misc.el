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


(leaf *hydra-selected
  :hydra
  (hydra-selected
   (:color red :hint nil)
   "
   _g_oogle  _k_oujien  _w_eblio  _e_ijiro  _t_ranslate  _c_lipboard  _s_wiper  comment_;_
"
   (";" comment-dwim)
   ("c" clipboard-kill-ring-save)
   ("s" swiper-thing-at-point)
   ("d" deeple-translate)
   ("t" gts-do-translate)
   ("w" my:weblio)
   ("k" my:koujien)
   ("e" my:eijiro)
   ("g" my:google)
   ("<muhenkan>" nil)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 30_hydra-misc.el ends here
