;;; 50_org-mode.el --- Org mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf org
  :hook ((emacs-startup-hook . (lambda () (require 'org-protocol)))
		 (org-mode-hook . (lambda () (electric-indent-local-mode -1)))
		 ((org-capture-mode-hook org-agenda-mode-hook) . delete-other-windows))
  :chord (",," . org-capture)
  :bind ((:org-mode-map
		  ("C-c i" . org-edit-special))
		 ("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ("C-c k" . org-capture-kill)
		 ("C-c i" . org-edit-src-exit))
  :config
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  (setq org-startup-folded 'content)
  (setq org-startup-truncated nil)
  (setq org-agenda-files '("~/Dropbox/howm/org/task.org"))
  (setq org-refile-targets
		(quote (("~/Dropbox/howm/org/archives.org" :level . 1)
				("~/Dropbox/howm/org/remember.org" :level . 1)
				("~/Dropbox/howm/org/memo.org" :level . 1)
				("~/Dropbox/howm/org/task.org" :level . 1))))

  ;; Capture template
  (setq org-capture-templates
		'(("m" " Memo" entry (file+headline "~/Dropbox/howm/org/memo.org" "Memo")
		   "* %? %U %i")
		  ("i" " Idea" entry (file+headline "~/Dropbox/howm/org/idea.org" "Idea")
		   "* %? %U %i")
		  ("r" " Remember" entry (file+headline "~/Dropbox/howm/org/remember.org" "Remember")
		   "* %? %U %i")
		  ("t" " Task" entry (file+headline "~/Dropbox/howm/org/task.org" "TASK")
		   "** TODO %?\n SCHEDULED: %^t \n")
		  ("e" " Experiment" entry (file+headline "~/Dropbox/howm/org/experiment.org" "Experiment")
		   "* %? %i\n#+BEGIN_SRC perl\n\n#+END_SRC\n\n%U")
		  ("p" " Code capture" entry (file+headline "~/Dropbox/howm/org/capture.org" "Code")
		   "* %^{Title} \nSOURCE: %:link\nCAPTURED: %U\n\n#+BEGIN_SRC\n%i\n#+END_SRC\n" :prepend t)
		  ("L" " Link capture" entry (file+headline "~/Dropbox/howm/org/capture.org" "Link")
		   "* [[%:link][%:description]] \nCAPTURED: %U\nREMARKS: %?" :prepend t))))


(leaf calendar
  :leaf-defer t
  :bind (("<f7>" . calendar)
		 (:calendar-mode-map
		  ("<f7>" . calendar-exit)))
  :config
  (leaf japanese-holidays
	:ensure t
	:require t
	:hook ((calendar-today-visible-hook . japanese-holiday-mark-weekend)
		   (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
		   (calendar-today-visible-hook . calendar-mark-today))
	:config
	(setq calendar-holidays
		  (append japanese-holidays holiday-local-holidays holiday-other-holidays))
	(setq calendar-mark-holidays-flag t)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_org-mode.el ends here
