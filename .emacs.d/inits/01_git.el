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


(leaf gist
  :ensure t
  :config
  (bind-key "C-c g" 'gist-region-or-buffer)
  (bind-key "C-c l" 'gist-list)
  (bind-key "." 'hydra-gist-help/body tabulated-list-mode-map)
  :hydra
  (hydra-gist-help ()
				   "
  🎲 Function for gist
     M-x gist-list: Lists your gists in a new buffer
     M-x gist-region-or-buffer: Post either the current region or buffer
    -----------------------------
  🎲 In gist-list buffer
     RET:fetch  e:edit-description  g:list-reload  b:browse current  y:print current url
     +:add file to current  -:remove file from current  k:delete current
    -----------------------------
  🎲 In fetch file buffer
     C-x C-s : save a new version of the gist
     C-x C-w : rename some file
    -----------------------------
  🎲 In dired buffer
     @ : make a gist out of marked files"
				   ("." nil)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 01_git.el ends here
