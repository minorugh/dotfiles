;;; 50_howm.el --- howm mode configurations.
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
  :config
  (setq howm-view-split-horizontally t)
  (setq howm-view-summary-persistent nil)
  (setq howm-normalizer              'howm-sort-items-by-reverse-date)
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
