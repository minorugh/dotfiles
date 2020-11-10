;;; 60_memo.el --- memo tools  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf howm
  :ensure t
  :hook (emacs-startup-hook . howm-mode)
  :chord ("@@" . howm-list-all)
  :init
  (setq howm-view-title-header "#")
  (defun my:howm-create-file ()
    "Make howm create file with 'org-capture'."
    (interactive)
    (format-time-string "~/Dropbox/howm/%Y/%m/%Y%m%d%H%M.md" (current-time)))
  :config
  (bind-key [backtab] 'howm-view-summary-previous-section howm-view-summary-mode-map)
  (setq howm-directory "~/Dropbox/howm")
  (setq howm-view-split-horizontally t)
  (setq howm-view-summary-persistent nil)
  (setq howm-normalizer 'howm-sort-items-by-reverse-date)
  (setq howm-user-font-lock-keywords
		'(("memo:" . (0 'gnus-group-mail-3))
		  ("note:" . (0 'epa-mark))
		  ("perl:" . (0 'diff-refine-added))
		  ("haiku:" . (0 'compilation-mode-line-exit))
		  ("emacs:" . (0 'compilation-info))
		  ("linux:" . (0 'compilation-error)))))


(leaf org
  :config
  (bind-key "C-c a" 'org-agenda)
  (bind-key "C-c c" 'org-capture)
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("~/Dropbox/howm/org/task.org"))
  (setq org-capture-templates
		'(("t" " Task" entry (file+headline "~/Dropbox/howm/org/task.org" "Task")
		   "** TODO %?\n SCHEDULED: %^t \n" :prepend t)
		  ("m" " Memo" plain (file my:howm-create-file)
		   "# memo: %?\n%U %i")
		  ("n" " Note" plain (file my:howm-create-file)
		   "# note: %?\n%U %i")
		  ("p" "★ Perl" plain (file my:howm-create-file)
		   "# Perl: %?\n%U %i\n\n>>>\n\n```perl\n%i\n```")
		  ("e" "★ Emacs" plain (file my:howm-create-file)
		   "# emacs: %?\n%U %i\n\n```emacs-lisp\n%i\n```")
		  ("l" "★ Linux" plain (file my:howm-create-file)
		   "# linux: %?\n%U %i")))
  :init
  ;; Maximize the org-capture buffer
  (defvar my:org-capture-before-config nil
    "Window configuration before 'org-capture'.")
  (defadvice org-capture (before save-config activate)
    "Save the window configuration before 'org-capture'."
    (setq my:org-capture-before-config (current-window-configuration)))
  (add-hook 'org-capture-mode-hook 'delete-other-windows))


(leaf open-junk-file :ensure t
  :config
  (setq open-junk-file-format "~/Dropbox/howm/junk/%Y%m%d.")
  (setq open-junk-file-find-file-function 'find-file)
  :init
  ;; https://qiita.com/zonkyy/items/eba6bc64f66d278f0032
  (leaf em-glob	:require t
	:config
	(defvar junk-file-dir "~/Dropbox/howm/junk/")
	(defun open-last-junk-file ()
	  "Open last created junk-file."
	  (interactive)
	  (find-file
	   (car
		(last (eshell-extended-glob
			   (concat
				(file-name-as-directory junk-file-dir)
				"*.*.*"))))))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 60_org-howm.el ends here
