;;; 20_compile.el --- Define compile functions.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf compile
  :doc "run compiler as inferior of Emacs"
  :tag "Builtin"
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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_compile.el ends here
