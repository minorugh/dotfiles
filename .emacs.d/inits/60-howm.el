;;; 60-howm.el --- Howm mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf howm :ensure t
  :doc "Wiki-like note-taking tool."
  :url "https://howm.osdn.jp"
  :defun howm-create my:howm-create-note evil-insert-state my:howm-fix-after-super-save
  :hook (emacs-startup-hook . howm-mode)
  :bind ((:howm-view-summary-mode-map
	  ([backtab]  . howm-view-summary-previous-section)
	  ("<return>" . howm-view-summary-open)
	  (","        . my:howm-create-memo)
	  (";"        . my:howm-create-tech)))
  :init
  (setq howm-use-migemo t)
  (setq howm-migemo-client '((type . cmigemo) (command . "/usr/bin/cmigemo")))
  (setq howm-migemo-client-option '("-q" "-d" "/usr/share/cmigemo/utf-8/migemo-dict"))
  (setq howm-view-title-header "#")
  (setq howm-directory "~/Dropbox/howm")
  (setq howm-file-name-format "%Y/%m/%Y%m%d%H%M.md")
  :config
  (setq howm-view-title-regexp "^# [^#]")   ;; ## 以降を一覧表示から除外
  (setq howm-view-use-grep t)
  (setq howm-view-split-horizontally t)
  (setq howm-view-summary-persistent nil)
  (setq howm-normalizer 'howm-sort-items-by-reverse-date)
  (setq howm-user-font-lock-keywords
	'(("memo:" . (0 'font-lock-comment-face))
	  ("note:" . (0 'compilation-info))
	  ("tech:" . (0 'compilation-info))
	  ("教会:" . (0 'font-lock-keyword-face))
	  ("園芸:" . (0 'font-lock-warning-face))
	  ("日記:" . (0 'font-lock-builtin-face))
	  ("創作:" . (0 'font-lock-constant-face))))

  (setq howm-template
	'("# %title%cursor\n%date%file"
	  "# memo: %cursor\n%date%file"
	  "# tech: %cursor\n%date%file"))

  (defun my:howm-create-note (index)
    "Create howm note with template INDEX, delete other windows, enter insert state."
    (howm-create index nil)
    (delete-other-windows)
    (evil-insert-state))

  (defun my:howm-create-memo ()
    "Create memo note."
    (interactive)
    (my:howm-create-note 2))

  (defun my:howm-create-tech ()
    "Create tech note."
    (interactive)
    (my:howm-create-note 3))

  (defun my:howm-fix-after-super-save (&rest _)
    (when (and (eq major-mode 'howm-mode)
               (buffer-file-name))
      (message "howm-fix running: %s" (buffer-file-name))
      (shell-command
       (format "perl ~/.emacs.d/bin/howm-fix-code-comments.pl %s"
               (shell-quote-argument (buffer-file-name))))))

  (advice-add 'super-save-command :after #'my:howm-fix-after-super-save)

  )

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 60-howm.el ends here
