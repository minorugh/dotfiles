;;; 10-check.e,l --- Syntax checking configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flycheck :ensure t
  :doc "On-the-fly syntax checking"
  :hook (((text-mode-hook prog-mode-hook) . flycheck-mode)
	     (lisp-interaction-mode-hook
	      . (lambda () (interactive)(flycheck-mode 0))))
  :bind ("C-c f" . flycheck-list-errors)
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


(leaf textlint
  :doc "checker for textlint"
  :url "https://qiita.com/mhatta/items/8f2aaa4e27c8f5a4c001?utm_source=pocket_shared"
  :after flycheck
  :config
  (flycheck-define-checker textlint
    "A linter for prose."
    :command ("textlint" "--format" "unix" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode web-mode)))


(leaf ispell :ensure nil
  :tag "Builtin"
  :doc "For hunspell"
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (with-eval-after-load "ispell"
    (setenv "DICTIONARY" "en_US")
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 10-check.el ends here
