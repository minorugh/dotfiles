;;; 50_howm.el --- howm mode configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf howm
  :doc "Wiki-like note-taking tool"
  :ensure t
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
  :custom
  `((howm-view-split-horizontally . t)
	(howm-view-summary-persistent . nil)
	(howm-normalizer              . 'howm-sort-items-by-reverse-date)
	(howm-user-font-lock-keywords
	 . '(("memo:" . (0 'compilation-error))
		 ("note:" . (0 'compilation-info))
		 ("tech:" . (0 'compilation-info))))
	(howm-template
	 . '("# %title%cursor\n%date%file"
		 "# memo: %cursor\n%date%file"
		 "# tech: %cursor\n%date%file")))
  :config
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


(defun open-junk-file ()
  "Open junk file without install Package."
  (interactive)
  (let* ((file (expand-file-name
                (format-time-string
                 "%Y-%m-%d-%H%M%S." (current-time))
                "~/Dropbox/howm/junk/"))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file-other-window (read-string "Junk Code: " file))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_howm.el ends here
