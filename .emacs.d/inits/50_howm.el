;;; 50_howm.el --- Howm mode configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf howm :ensure t
  :doc "Wiki-like note-taking tool"
  :url "https://howm.osdn.jp"
  :hook ((after-init-hook . howm-mode))
  :bind ((:howm-view-summary-mode-map
	  ([backtab]  . howm-view-summary-previous-section)
	  ("<return>" . howm-view-summary-open)
	  (","        . my:howm-create-memo)
	  (";"        . my:howm-create-tech)))
  :init
  (setq howm-view-title-header "#")
  (setq howm-directory "~/Dropbox/howm")
  (setq howm-file-name-format "%Y/%m/%Y%m%d%H%M.md")
  ;; :config
  ;; (setq howm-dtime-format (concat "<" howm-dtime-body-format ">"))
  ;; (setq howm-insert-date-format "<%s>")
  ;; (setq howm-reminder-regexp-grep-format (concat "<" howm-date-regexp-grep "[ :0-9]*>%s"))
  ;; (setq howm-reminder-regexp-format (concat "\\(<" howm-date-regexp "[ :0-9]*>\\)\\(\\(%s\\)\\([0-9]*\\)\\)"))
  ;; (setq howm-reminder-today-format (format howm-insert-date-format howm-date-format))
  ;; (defadvice howm-action-lock-done-done(after my-org-todo-done () activate) (org-todo))
  :config
  ;; (setq howm-view-use-grep t)
  ;; (setq howm-view-grep-command "rg")
  ;; (setq howm-keyword-case-fold-search t)
  ;; (setq howm-view-grep-option "-nH --no-heading --color never")
  ;; (setq howm-view-grep-extended-option nil)
  ;; (setq howm-view-grep-fixed-option "-F")
  ;; (setq howm-view-grep-expr-option nil)
  ;; (setq howm-view-grep-file-stdin-option nil)
  ;; (setq howm-menu-file "~/Dropbox/howm/howm_menu.txt")
  (setq howm-view-split-horizontally t)
  (setq howm-view-summary-persistent nil)
  (setq howm-normalizer 'howm-sort-items-by-reverse-date)
  (setq howm-user-font-lock-keywords
	'(("memo:" . (0 'compilation-error))
	  ("note:" . (0 'compilation-info))
	  ("tech:" . (0 'compilation-info))))
  (setq howm-template
	'("# %title%cursor\n%date%file"
	  "# memo: %cursor\n%date%file"
	  "# tech: %cursor\n%date%file"))

  (defun my:howm-create-memo ()
    "Create by inserting tags automatically."
    (interactive)
    (howm-create 2 nil)
    (delete-other-windows)
    (evil-insert-state))

  (defun my:howm-create-tech ()
    "Create by inserting tags automatically."
    (interactive)
    (howm-create 3 nil)
    (delete-other-windows)
    (evil-insert-state)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_howm.el ends here
