;;; 06-counsel.el --- Counsel configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel
  :ensure t
  :doc "Various completion functions using Ivy."
  :hook (after-init-hook . ivy-mode)
  :bind (("C-:"     . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x g"   . counsel-git)         ;; プロジェクト内のファイルを検索
         ("s-a"     . counsel-git-grep)    ;; プロジェクト内を全文検索 (agの代わり)
         ("M-x"     . counsel-M-x)
         ("M-y"     . counsel-yank-pop)
         ("C-,"     . counsel-mark-ring))
  :config
  (setq search-default-mode        nil)
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-use-selectable-prompt    t)
  (setq enable-recursive-minibuffers t)
  (setq counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  (setq ivy-format-functions-alist '((t . my-ivy-format-function-arrow)))

  (defun my-ivy-format-function-arrow (cands)
    "Transform into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (display-graphic-p)
		   (nerd-icons-octicon "nf-oct-chevron_right")
		 "")
	       (propertize " " 'display `(space :align-to 2))
	       (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n")))

(leaf ivy-rich
  :ensure t
  :doc "More friendly display transformer for ivy."
  :hook (after-init-hook . ivy-rich-mode))

(leaf amx
  :ensure t
  :doc "Alternative 'M-x' with extra features."
  :config
  (setq amx-save-file (locate-user-emacs-file "tmp/amx-items"))
  (setq amx-history-length 20))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 06-counsel.el ends here
