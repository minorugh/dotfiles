;;; 89_eshell.el --- emacs command shell  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf eshell
  :after popwin
  :bind* ("C-z" . eshell)
  :init
  (push '("*eshell*" :height 0.6) popwin:special-display-config)
  :config
  (setq eshell-cmpl-ignore-case t)
  (setq eshell-ask-to-save-history (quote always))
  (setq eshell-cmpl-cycle-completions t)
  (setq eshell-cmpl-cycle-cutoff-length 5)
  (setq eshell-hist-ignoredups t)
  (setq eshell-prompt-function 'my:eshell-prompt)
  (setq eshell-prompt-regexp "^[^#$]*[$#] ")
  (setq eshell-command-aliases-list
  		(append
  		 (list
  		  (list "cl" "clear")
  		  (list "ll" "ls -ltr")
  		  (list "la" "ls -a")
		  (list "d" "dired .")
  		  (list "ex" "exit"))))

  ;; cus functions
  (defun my:eshell-prompt ()
    "Prompt change string."
    (concat
	 (eshell/pwd)
	 (if (= (user-uid) 0) "\n# " "\n$ ")))

  (defun eshell/clear ()
	"Clear the current buffer, leaving one prompt at the top."
	(interactive)
	(let ((inhibit-read-only t))
	  (erase-buffer)))

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

;;; 90_eshell.el ends here
