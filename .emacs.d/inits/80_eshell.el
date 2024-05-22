;;; 80_eshell.el --- Eshell configurations.
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf eshell
  :doc "Emacs command shell"
  :tag "builtin"
  :after popwin
  :bind ("s-z" . eshell)
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
  (setq eshell-directory-name "~/.emacs.d/tmp/eshell")
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-ask-to-save-history (quote always))
  (setq eshell-cmpl-cycle-completions t)
  (setq eshell-cmpl-cycle-cutoff-length 5)
  (setq eshell-history-file-name "~/Dropbox/backup/zsh/.zsh_history")
  (setq eshell-hist-ignoredups t)
  (custom-set-variables
   '(eshell-prompt-function
     (lambda ()
       (concat
	"[" (abbreviate-file-name (eshell/pwd)) "]\n"
	(if (= (user-uid) 0) "#" "$")
	" ")))
   '(eshell-prompt-regexp "^\\(\\[[^]\n]+\\]\\|[$#] \\)")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 80_eshell.el ends here
