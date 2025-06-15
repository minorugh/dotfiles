;;; 10-buffer-ut.el --- Buffer utilities configulation. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf super-save :ensure t
  :doc "Smart auto save buffers"
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration       1)
  (setq super-save-remote-files        nil)
  (setq super-save-exclude             '(".gpg"))
  :hook after-init-hook)

(leaf persistent-scratch :ensure t
  :doc "Save *scratch* buffer state to file and restore from file"
  :hook (after-init-hook . persistent-scratch-autosave-mode)
  :bind ("S-<return>" . toggle-scratch)
  :config
  (setq persistent-scratch-save-file "~/.emacs.d/tmp/scratch")
  (defun toggle-scratch ()
    "Toggle current buffer and *scratch* buffer."
    (interactive)
    (if (not (string= "*scratch*" (buffer-name)))
	(progn
	  (setq toggle-scratch-prev-buffer (buffer-name))
	  (switch-to-buffer "*scratch*"))
      (switch-to-buffer toggle-scratch-prev-buffer))))

(leaf tempbuf
  :doc "https://www.emacswiki.org/emacs/TempbufMode"
  :vc (:url "https://github.com/minorugh/tempbuf")
  ;; `after-change-major-mode-hook' must be written last
  :hook ((emacs-lock-mode-hook . turn-off-tempbuf-mode)
	 (after-change-major-mode-hook . turn-on-tempbuf-mode))
  :config
  (setq tempbuf-kill-message nil))

(leaf emacs-lock-mode :tag "builtin"
  :doc "Set buffer that can not be killed"
  :hook (after-init-hook . my:lock-mode)
  :init
  (defun my:lock-mode ()
    (interactive)
    (with-current-buffer "*scratch*"
      (emacs-lock-mode 'kill))
    (with-current-buffer "*Messages*"
      (emacs-lock-mode 'kill))))

(leaf bs :tag "builtin"
  :doc "Menu for selecting and displaying buffers"
  :bind (("M-]" . bs-cycle-next)
	 ("M-[" . bs-cycle-previous)))

(leaf flycheck :ensure t
  :doc "On-the-fly syntax checking"
  :url "http://www.flycheck.org"
  :hook ((prog-mode-hook . flycheck-mode)
	 (lisp-interaction-mode-hook
 	  . (lambda () (interactive)(flycheck-mode 0))))
  :bind (("M-n" . flycheck-next-error)
	 ("M-p" . flycheck-previous-error))
  :config
  (setq flycheck-emacs-lisp-initialize-packages t)
  ;; Fixing leaf-keywords "Unrecognized keyword" error in flycheck
  (eval-and-compile (require 'flycheck))
  (setq flycheck-emacs-lisp-package-initialize-form
	(flycheck-sexp-to-string
	 '(progn
	    (with-demoted-errors "Error during package initialization: %S"
              (package-initialize))
	    (leaf-keywords-init)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 10-buffer-ut.el ends here
