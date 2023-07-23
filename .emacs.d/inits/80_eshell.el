;;; 80_eshell.el --- Eshell configurations.
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
	;; (eshell-prompt-regexp . "^[^#$]*[$#] ")
	)
  :init
  (setq eshell-command-aliases-list
		(append
		 (list
		  (list "cl" "clear eshell")
		  (list "ll" "ls -ltr")
		  (list "la" "ls -a")
		  (list "ex" "exit"))))
  (push '("*eshell*" :height 0.5) popwin:special-display-config)

  (custom-set-variables
   ;; Specify eshell prompt format
   '(eshell-prompt-function
	 (lambda ()
       (concat
		"[" (abbreviate-file-name (eshell/pwd)) "]\n"
		(if (= (user-uid) 0) "#" "$")
		" ")))
   ;; Specify a regular expression to match the prompt
   ;; If using a multi-line prompt, write each line as a one-line match.
   '(eshell-prompt-regexp "^\\(\\[[^]\n]+\\]\\|[$#] \\)"))

  ;; Additional settings
  (defcustom eshell-prompt-regexp-lastline "^[#$] "
	"Specify a regular expression to match the last line of a multi-line prompt."
	:type 'regexp
	:group 'eshell-prompt)

  ;; Make skipping work correctly at multi-line prompts
  (defadvice eshell-skip-prompt (around eshell-skip-prompt-ext activate)
	(if (looking-at eshell-prompt-regexp)
		(re-search-forward eshell-prompt-regexp-lastline nil t))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 80_eshell.el ends here
