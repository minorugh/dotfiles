;;; 80_eshell.el --- Eshell configurations.
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf eshell
  :after popwin
  :bind ("C-c z" . eshell)
  :custom
  `((eshell-directory-name . ,"~/.emacs.d/tmp/eshell")
	(eshell-cmpl-ignore-case . t)
	(eshell-ask-to-save-history . (quote always))
	(eshell-cmpl-cycle-completions . t)
	(eshell-cmpl-cycle-cutoff-length . 5)
	(eshell-history-file-name . ,"~/Dropbox/backup/zsh/.zsh_history")
	(eshell-hist-ignoredups . t))
  :init
  (setq eshell-command-aliases-list
		(append
		 (list
		  (list "cl" "clear eshell")
		  (list "ll" "ls -ltr")
		  (list "la" "ls -a")
		  (list "ex" "exit"))))
  (push '("*eshell*" :height 0.5) popwin:special-display-config)
  :config
  (custom-set-variables
   '(eshell-prompt-function
	 (lambda ()
       (concat
		"[" (abbreviate-file-name (eshell/pwd)) "]\n"
		(if (= (user-uid) 0) "#" "$")
		" ")))
   '(eshell-prompt-regexp "^\\(\\[[^]\n]+\\]\\|[$#] \\)")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 80_eshell.el ends here
