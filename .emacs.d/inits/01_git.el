;;; 01_git.el  --- Git porcelain inside Emacs  -*- lexical-binding: t -*-_
;;; Commentary:

;;; Code:
;; (setq debug-on-erro t)

(leaf magit
  :ensure t
  :config
  (bind-key "C-x g" 'magit-status)
  ;; magit status stops splitting windows
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :hydra
  (hydra-magit
   (:color red :hint nil)
   "
 📦 Git: _s_tatus  _b_lame  _t_imemachine  _d_iff"
   ("s" magit-status :exit t)
   ("b" magit-blame :exit t)
   ("t" git-timemachine)
   ("d" vc-diff)
   ("<muhenkan>" nil))
  :init
  (leaf git-timemachine :ensure t))


(leaf diff-hl
  :ensure t
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :global-minor-mode (global-diff-hl-mode diff-hl-margin-mode))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 01_git.el ends here
