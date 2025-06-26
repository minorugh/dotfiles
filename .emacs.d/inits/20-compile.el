;;; 10-compile.e,l --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf compile
  :doc "run compiler as inferior of Emacs"
  :tag "Builtin"
  :require my:compile  ;; Load user make functions
  :config
  (add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t)
  (setq compilation-finish-functions 'compile-autoclose)
  :init
  (defun compile-autoclose (buffer string)
    "Automatically close the compilation."
    (cond ((string-match "compilation" (buffer-name buffer))
	   (string-match "finished" string)
	   (delete-other-windows)
	   (message "Compile successful."))
	  (t (message "Compilation exited abnormally: %s" string)))))


(leaf *auto-byte-compile
  :doc "Byte compilation is performed when Emacs exits."
  :hook (kill-emacs-hook . auto-compile-inits)
  :init
  (defun auto-compile-inits ()
    "Byte-compilation of all initial configuration files."
    (interactive)
    (byte-compile-file "~/.emacs.d/early-init.el")
    (byte-compile-file "~/.emacs.d/init.el")
    (byte-recompile-directory (expand-file-name "~/.emacs.d/elisp") 0)
    (byte-recompile-directory (expand-file-name "~/.emacs.d/inits") 0)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 10-compile.el ends here
