;;; 50_org-mode.el --- Org mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf org
  :hook ((emacs-startup-hook . (lambda () (require 'org-protocol)))
		 ((org-capture-mode-hook org-agenda-mode-hook) . delete-other-windows))
  :chord (",," . org-capture)
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ("C-c k" . org-capture-kill)
		 ("C-c i" . org-edit-src-exit)
		 (:org-mode-map
		  ("C-c i" . org-edit-special)))
  :custom
  `((org-log-done . 'time)
	(timep-use-speed-commands . t)
	(org-src-fontify-natively . t)
	(org-startup-folded . 'content)
	(org-agenda-files . '("~/Dropbox/howm/org/task.org"))
	(org-agenda-span . 30))
  :config
  (setq org-capture-templates
		'(("m" " Memo" entry (file+headline "~/Dropbox/howm/org/memo.org" "Memo")
		   "* %? %U %i")
		  ("i" " Idea" entry (file+headline "~/Dropbox/howm/org/idea.org" "Idea")
		   "* %? %U %i")
		  ("t" " Task" entry (file+headline "~/Dropbox/howm/org/task.org" "TASK")
		   "** TODO %?\n SCHEDULED: %^t \n")
		  ("e" " Experiment" entry (file+headline "~/Dropbox/howm/org/experiment.org" "Experiment")
		   "* %? %i\n#+BEGIN_SRC perl\n\n#+END_SRC\n\n%U")
		  ("p" " Code capture" entry (file+headline "~/Dropbox/howm/org/capture.org" "Code")
		   "* %^{Title} \nSOURCE: %:link\nCAPTURED: %U\n\n#+BEGIN_SRC\n%i\n#+END_SRC\n" :prepend t)
		  ("L" " Link capture" entry (file+headline "~/Dropbox/howm/org/capture.org" "Link")
		   "* [[%:link][%:description]] \nCAPTURED: %U\nREMARKS: %?" :prepend t)))

  (setq org-refile-targets
		(quote (("~/Dropbox/howm/org/archives.org" :level . 1)
				("~/Dropbox/howm/org/remember.org" :level . 1)
				("~/Dropbox/howm/org/memo.org" :level . 1)
				("~/Dropbox/howm/org/task.org" :level . 1)))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_org-mode.el ends here
