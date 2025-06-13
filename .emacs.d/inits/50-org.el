;;; 50-org.el --- Org mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf org
  :chord (",," . org-capture)
  :bind ((("C-c a"  . org-agenda)
	  ("C-c c"  . org-capture)
	  ("C-c k"  . org-capture-kill)
	  ("C-c i"  . org-edit-src-exit)
	  (:org-mode-map
	   ("C-c i" . org-edit-special))))
  :config
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  (setq org-startup-folded 'content)
  (setq org-startup-truncated nil)
  (setq org-agenda-span 'month)
  (setq org-agenda-files '("~/Dropbox/howm/org/task.org"))

  ;; Capture template
  (defun my:howm-create-file ()
    "Make howm create file on `org-capture'."
    (interactive)
    (format-time-string "~/Dropbox/howm/%Y/%m/%Y%m%d%H%M.md" (current-time)))

  (defun my:open-junk-file ()
    "Make create junk file on `org-capture'."
    (interactive)
    (format-time-string "~/Dropbox/howm/junk/%Y%m%d%H%M.pl" (current-time)))

  (setq org-capture-templates
	'(("m" " Memo" plain (file my:howm-create-file)
	   "# memo: %?\n%U %i")
	  ("i" " Idea" plain (file my:howm-create-file)
	   "# idea: %?\n%U %i")
	  ("t" " Tech" plain (file my:howm-create-file)
	   "# tech: %?\n%U %i")
	  ("j" " Junk" plain (file my:open-junk-file)
	   "#!/usr/bin/perl\n## %?\n %i")
	  ("," " Task" entry (file+headline "~/Dropbox/howm/org/task.org" "TASK")
	   "** TODO %?\n SCHEDULED: %^t \n"))))

(leaf calendar
  :defvar calendar-holidays japanese-holidays
  :bind (("<f7>"  . calendar)
	 (:calendar-mode-map
	  ("<f7>" . calendar-exit)))
  :config
  (with-eval-after-load 'japanese-holidays
    (setq calendar-holidays (append japanese-holidays holiday-local-holidays))))

(leaf japanese-holidays :ensure t
  :after calendar
  :require t
  :hook '((calendar-today-visible-hook   . japanese-holiday-mark-weekend)
	  (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
	  (calendar-today-visible-hook   . calendar-mark-today))
  :config
  (setq calendar-holidays
	(append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq calendar-mark-holidays-flag t))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 50-org.el ends here
