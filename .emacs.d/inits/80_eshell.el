;;; 80_eshell.el --- Eshell configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf eshell
  :after popwin
  :bind ("s-z" . eshell)
  :custom
  `((eshell-directory-name . ,"~/.emacs.d/tmp/eshell")
	(eshell-cmpl-ignore-case . t)
	(eshell-ask-to-save-history . (quote always))
	(eshell-cmpl-cycle-completions . t)
	(eshell-cmpl-cycle-cutoff-length . 5)
	(eshell-history-file-name . ,"~/Dropbox/backup/zsh/.zsh_history")
	(eshell-hist-ignoredups . t)
	(eshell-prompt-function . 'my:eshell-prompt)
	(eshell-prompt-regexp . "^[^#$]*[$#] "))
  :config
  (setq eshell-command-aliases-list
		(append
		 (list
		  (list "cl" "clear eshell")
		  (list "ll" "ls -ltr")
		  (list "la" "ls -a")
		  (list "ex" "exit"))))
  (push '("*eshell*" :height 0.6) popwin:special-display-config)
  :init
  (defun my:eshell-prompt ()
	"Prompt change string."
	(interactive)
	(concat
	 (eshell/pwd)
	 (if (= (user-uid) 0) "\n# " "\n$ ")))

  (defun eshell-on-current-buffer ()
	"Set the eshell directory to the current buffer."
	(interactive)
	(let ((path (file-name-directory (or  (buffer-file-name) default-directory))))
	  (with-current-buffer "*eshell*"
		(cd path)
		(eshell-emit-prompt)))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 80_eshell.el ends here
