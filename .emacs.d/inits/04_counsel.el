;;; 04_counsel.el --- Counsel configurations. -*- no-byte-compile: t; -*-
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
	 ("C-x a"   . counsel-linux-app)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf))
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
      (counsel-ag ivy-text nil "")))
  :preface
  (leaf avy :ensure t
    :doc "Jump to arbitrary positions quickly."
    :bind ("C-r" . avy-goto-word-1))

  (leaf ivy-rich :ensure t
    :doc "More friendly display transformer for ivy"
    :hook after-init-hook)

  (leaf amx :ensure t
    :doc "Alternative 'M-x' with extra features"
    :config
    (setq amx-save-file "~/.emacs.d/tmp/amx-items")
    (setq amx-history-length 20)))


(leaf swiper :ensure t
  :doc "Isearch with an overview"
  :bind (("C-s" . swiper-region)
	 ("s-s" . swiper-thing-at-point))
  :config
  (defun swiper-region ()
    "If region is selected, `swiper-thing-at-point'.
If the region isn't selected, `swiper'."
    (interactive)
    (if (not (use-region-p))
	(swiper)
      (swiper-thing-at-point)))

  ;; For swiper-migemo
  ;; see "https://www.yewton.net/2020/04/21/migemo-ivy/"
  (defun my:ivy-migemo-re-builder (str)
    "Own function for my:ivy-migemo."
    (let* ((sep " \\|\\^\\|\\.\\|\\*")
	   (splitted (--map (s-join "" it)
			    (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
					    (s-split "" str t)))))
      (s-join "" (--map (cond ((s-equals? it " ") ".*?")
			      ((s-matches? sep it) it)
			      (t (migemo-get-pattern it)))
			splitted))))

  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
				(swiper . my:ivy-migemo-re-builder)))
  :init
  (leaf migemo :ensure t
    :doc "Japanese incremental search through dynamic pattern expansion"
    :if (executable-find "cmigemo")
    :hook (after-init-hook . migemo-init)
    :config
    (setq migemo-command    "cmigemo")
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))


;;; 04_counsel.el ends here
