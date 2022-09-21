;;; 20_hydra-pkgut.el --- Hydra packege utilis configuration for misc utils. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf package-utils
  :ensure t
  :chord ("p@" . hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
 📦 Package: _m_elpa  _i_nstall  upgrade:_l_ist._n_ame._a_ll  _r_emove  _e_l-get
"
   ("i" package-install)
   ("l" package-utils-list-upgrades)
   ("n" package-utils-upgrade-by-name)
   ("r" package-utils-remove-by-name)
   ("a" package-utils-upgrade-all-and-restart)
   ("m" package-list-packages)
   ("e" select-elget-command)
   ("<muhenkan>" nil))
  :preface
  (defun select-elget-command ()
    "Narrow the only el-get command in `M-x'."
    (interactive)
    (counsel-M-x "^el-get ")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_hydra-package.el ends here
