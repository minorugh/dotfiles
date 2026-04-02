;;; 10-check.el --- Syntax checking configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flycheck :ensure t
  :doc "On-the-fly syntax checking."
  :hook ((prog-mode-hook . flycheck-mode)
	 (gfm-mode-hook  . flycheck-mode)
	 (lisp-interaction-mode-hook . (lambda () (flycheck-mode 0))))
  :bind ("C-c f" . flycheck-list-errors)
  :config
  ;; add elisp directory to flycheck-emacs-lisp-load-path
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-emacs-lisp-initialize-packages t)
  ;; Fixing leaf-keywords "Unrecognized keyword" error in flycheck
  (require 'flycheck)
  (setq flycheck-emacs-lisp-package-initialize-form
	(flycheck-sexp-to-string
	 '(progn
	    (with-demoted-errors "Error during package initialization: %S"
              (package-initialize))
	    (leaf-keywords-init)))))

;; textlint checker (leafを使わず直接定義)
(with-eval-after-load 'flycheck
  (flycheck-define-checker textlint
    "A linter for prose."
    :command ("textlint" "--format" "unix" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (markdown-mode gfm-mode org-mode web-mode)))


;; (leaf textlint
;;   :ensure nil
;;   :doc "Checker for textlint."
;;   :url "https://qiita.com/mhatta/items/8f2aaa4e27c8f5a4c001?utm_source=pocket_shared"
;;   :after flycheck
;;   :config
;;   (flycheck-define-checker textlint
;;     "A linter for prose."
;;     :command ("textlint" "--format" "unix" source-inplace)
;;     :error-patterns
;;     ((warning line-start (file-name) ":" line ":" column ": "
;;               (id (one-or-more (not (any " "))))
;;               (message (one-or-more not-newline)
;;                        (zero-or-more "\n" (any " ") (one-or-more not-newline)))
;;               line-end))
;;     :modes (markdown-mode gfm-mode org-mode web-mode)))


(leaf ispell :ensure nil
  :tag "builtin"
  :doc "For hunspell."
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)
  (setenv "DICTIONARY" "en_US")
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-check.el ends here
