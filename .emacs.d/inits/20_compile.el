;;; 20_compile.el --- Compilation configurations. -*- no-byte-compile: t; -*-
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


(leaf *user-make-command
  :doc "User make command functions"
  :init
  (defun my:make-k ()
    "Make k."
    (interactive)
    (compile "make -k"))

  (defun my:make-upsftp ()
    "Make upfstp."
    (interactive)
    (compile "make up"))

  (defun my:make-move ()
    "Make move."
    (interactive)
    (compile "make mv"))

  (defun my:make-bklog ()
    "Make bklog."
    (interactive)
    (compile "make bk"))

  (defun my:make-git ()
    "Make git."
    (interactive)
    (compile "make git"))

  (defun my:make-clean ()
    "Make clean."
    (interactive)
    (compile "make clean")))


;;; 20_compile.el ends here
