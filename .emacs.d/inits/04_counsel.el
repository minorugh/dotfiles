;;; 04_counsel.el --- Counsel configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel :ensure t
  :doc "Various completion functions using Ivy"
  :hook (after-init-hook . ivy-mode)
  :bind (("C-:"     . counsel-switch-buffer)
	 ("C-x C-b" . counsel-switch-buffer)
	 ("M-x"     . counsel-M-x)
	 ("M-y"     . counsel-yank-pop)
	 ("M-:"     . counsel-buffer-or-recentf)
	 ("C-,"     . counsel-mark-ring)
	 ("s-a"     . counsel-ag)
	 ("C-x C-a" . counsel-linux-app)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf))
  :init
  (leaf ivy-rich :ensure t
    :doc "More friendly display transformer for ivy"
    :hook (after-init-hook . ivy-rich-mode))
  (leaf amx :ensure t
    :doc "Alternative 'M-x' with extra features"
    :config
    (setq amx-save-file "~/.emacs.d/tmp/amx-items")
    (setq amx-history-length 20))
  :config
  (setq search-default-mode          nil)
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-use-selectable-prompt    t)
  (setq enable-recursive-minibuffers t)
  (setq counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  (setq ivy-format-functions-alist '((t . my:ivy-format-function-arrow)))
  (defun my:ivy-format-function-arrow (cands)
    "Transform into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (display-graphic-p)
		   "")
	       (propertize " " 'display `(space :align-to 2))
	       (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n"))

  (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
    "Fast full-text search. see https://takaxp.github.io/init.html#org29c7b6b7"
    (apply f (or initial-input
		 (and (not (thing-at-point-looking-at "^\\*+"))
		      (ivy-thing-at-point)))
	   (unless current-prefix-arg
	     (or initial-directory default-directory))
	   extra-ag-args ag-prompt caller))
  (advice-add 'counsel-ag :around #'ad:counsel-ag)
  ;; Make search trigger even with 2 characters
  (add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))
  (ivy-add-actions
   'counsel-ag
   '(("r" my:counsel-ag-in-dir "search in directory")))
  (defun my:counsel-ag-in-dir (_arg)
    "Search again with new root directory."
    (let ((current-prefix-arg '(4)))
      (counsel-ag ivy-text nil ""))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 04_counsel.el ends here
