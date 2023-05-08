;; 02_git.el  --- Git configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

;; Mgit configuration
(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status )
		 ("M-g" . hydra-git/body))
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :hydra
  (hydra-git
   (:color red :hint nil)
   "
  magit: _s_tatus  _b_lame  _c_heckout  _l_og  _g_itk  _t_imemachine
"
   ("s" magit-status)
   ("b" magit-blame-addition)
   ("c" magit-file-checkout)
   ("l" magit-log-buffer-file)
   ("g" gitk-open)
   ("t" git-timemachine-toggle)
   ("<muhenkan>" nil))
  :custom
  (transient-history-file . "~/.emacs.d/tmp/transient-history")
  :preface
  (defun gitk-open ()
	"Open gitk with current dir."
	(interactive)
	(shell-command "gitk &")
	(delete-other-windows))

  (defun git-gui-open ()
	"Open gitk with current dir."
	(interactive)
	(shell-command "git gui &")
	(delete-other-windows))
  :init
  (leaf diff-hl	:ensure t
	:hook ((after-init-hook . global-diff-hl-mode)
		   (after-init-hook . diff-hl-margin-mode)))
  (leaf git-timemachine	:ensure t)
  (leaf browse-at-remote :ensure t
	:custom (browse-at-remote-prefer-symbolic . nil)))


;; Gist configuration
(leaf cus-gist
  :bind (("s-g p" . gist-region-or-buffer)
		 ("s-g c" . my:chromium-gist))
  :init
  (defun gist-description ()
	"Add gist description."
	(shell-quote-argument (read-from-minibuffer "Add gist description: ")))

  (defun gist-filename ()
	"The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer-file-neme."
	(interactive)
	(let ((file (file-name-nondirectory (buffer-file-name (current-buffer)))))
	  (read-from-minibuffer (format "File name (%s): " file) File)))

  (defun gist-region-or-buffer ()
	"If region is selected, post from the region.
If region isn't selected, post from the buffer."
	(interactive)
	(let ((file (buffer-file-name)))
	  (if (not (use-region-p))
		  (compile (concat "gist -od " (gist-description) " " file))
		(compile (concat "gist -oPd " (gist-description) " -f " (gist-filename)))))
	(delete-other-windows))

  (defun dired-do-gist ()
	"Dired-get-filename do gist and open in browser."
	(interactive)
	(let ((file (dired-get-filename nil t)))
	  (compile (concat "gist -od " (gist-description) " " file)))
	(delete-other-windows)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 02_git.el ends here
