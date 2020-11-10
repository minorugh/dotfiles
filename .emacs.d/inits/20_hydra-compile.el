;;; 20_hydra-compile.el --- hydra for compile command  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf user-make-function
  :mode ("\\.mak\\'" "makefile\\'" . makefile-mode)
  :hydra
  (hydra-compile
   (:color red :hint nil)
   "
   🗿 Compile: make:_k_  _u_pftp  _m_ove  _b_klog  _g_it  _c_lean  _e_rror 🐾 "
   ("k" my:make-k)
   ("u" my:make-upftp)
   ("m" my:make-move)
   ("b" my:make-bklog)
   ("g" my:make-git)
   ("c" my:make-clean)
   ("e" next-error)
   ("<muhenkan>" nil))
  :init
  ;; https://gist.github.com/EricCrosson/fa41233f327403ea2a5a
  (defun close-compile-window-if-successful (buffer string)
    "Close a compilation window if succeeded without warnings."
    (when (and
		   (string-match "compilation" (buffer-name buffer))
		   (string-match "finished" string)
		   (not
			(with-current-buffer buffer
			  (search-forward "warning" nil t))))
      (run-with-timer 1 nil
					  (lambda ()
						(delete-other-windows)))))
  (add-hook 'compilation-finish-functions 'close-compile-window-if-successful)

  ;; Turn off 'Suspicious line XXX of Makefile.' makefile warning
  (add-hook 'makefile-mode-hook
			(lambda ()
			  (fset 'makefile-warn-suspicious-lines 'ignore)))

  ;; User-make-commands
  (defun my:make-k ()
	"Make command default."
	(interactive)
	(compile "make -k"))

  (defun my:make-upftp ()
	"Make command for upftp."
	(interactive)
	(compile "make up"))

  (defun my:make-move ()
	"Make command for move."
	(interactive)
	(compile "make mv"))

  (defun my:make-bklog ()
	"Make command for bklog."
	(interactive)
	(compile "make bk"))

  (defun my:make-git ()
	"Make command for git."
	(interactive)
	(compile "make git"))

  (defun my:make-clean ()
	"Make command for clean."
	(interactive)
	(compile "make clean")))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 20_hydra-compile.el ends here
