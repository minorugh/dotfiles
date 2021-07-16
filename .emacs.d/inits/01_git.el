;;; 01_git.el  --- Magit configurations. -*- lexical-binding: t no-byte-compile: t -*-_
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf magit
  :ensure t
  :bind (("s-s" . magit-status)
		 ("M-:" . hydra-magit/body))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :hydra
  (hydra-magit
   (:color red :hint nil)
   "
 📦 Git: _s_tatus  _b_lame  _d_iff  _t_imemachine"
   ("s" magit-status :exit t)
   ("b" magit-blame :exit t)
   ("d" magit-diff-working-tree)
   ("t" git-timemachine)
   ("<muhenkan>" nil))
  :init
  (leaf git-timemachine	:ensure t)
  (leaf diff-hl
	:ensure t
	:config
	(global-diff-hl-mode)
	(diff-hl-margin-mode)
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 01_git.el ends here
