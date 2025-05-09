;;; 30_hydra-misc.el --- Hydra misc configurations. -*- no-byte-compile: t; -*-
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


;;; 30_hydra-misc.el ends here
