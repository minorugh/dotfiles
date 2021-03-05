;;; 60_howm.el --- Howm & Org configurations. -*- lexical-binding: t no-byte-compile: t -*-
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
		  ("code:" . (0 'diff-refine-added))
		  ("haiku:" . (0 'compilation-mode-line-exit))
		  ("emacs:" . (0 'compilation-info))
		  ("linux:" . (0 'compilation-error)))))


(leaf org
  :require org-protocol
  :config
  (bind-key "C-c a" 'org-agenda)
  (bind-key "C-c c" 'org-capture)
  (bind-key "S-<return>" 'org-open-at-point org-mode-map)
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  (setq org-startup-folded 'content)
  ;; Agenda Settings
  (setq org-agenda-files '("~/Dropbox/howm/org/task.org"))
  (setq org-agenda-span 30)
  (defadvice org-agenda (around org-agenda-fullscreen activate)
    "Agenda open fullscreen."
    (interactive)
    (window-configuration-to-register :org-agenda-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice org-agenda-quit (around org-agenda-quit-fullscreen activate)
    "Agenda quit fullscreen."
    (interactive)
    ad-do-it
    (jump-to-register :org-agenda-fullscreen))
  ;; Caputure Settings
  (setq org-capture-templates
		'(("m" " Memo" plain (file my:howm-create-file)
		   i"# memo: %?\n%U %i")
		  ("n" " Note" plain (file my:howm-create-file)
		   "# note: %?\n%U %i")
		  ("t" " Task" entry (file+headline "~/Dropbox/howm/org/task.org" "Task")
		   "** TODO %?\n SCHEDULED: %^t \n" :prepend t)
		  ("e" "💣 Experiment" entry (file+headline "~/Dropbox/howm/org/code.org" "Experiment")
		   "* %? %U %i\n#+BEGIN_SRC\n\n#+END_SRC")
		  ("i" "✌ Idea" entry (file+headline "~/Dropbox/howm/org/idea.org" "Idea")
		   "* %? %U %i")
		  ("r" "🐾 Remember" entry (file+headline "~/Dropbox/howm/org/remember.org" "Remember")
		   "* %? %U %i")
		  ;; ("m" "Memo" entry (file+headline "~/Dropbox/howm/org/memo.org" "Memo")
		  ;;  "* %? %U %i")
		  ("p" "Code capture with Chrome" entry (file+headline "~/Dropbox/howm/org/link.org" "Inbox")
		   "* %^{Title} \nSOURCE: %:link\nCAPTURED: %U\n\n#+BEGIN_SRC\n%i\n#+END_SRC\n" :prepend t)
		  ("L" "Link capture with Chrome" entry (file+headline "~/Dropbox/howm/org/link.org" "Inbox")
		   "* [[%:link][%:description]] \nCAPTURED: %U\nREMARKS: %?" :prepend t)
		  ))
  (setq org-refile-targets
		(quote (("~/Dropbox/howm/org/archives.org" :level . 1)
				("~/Dropbox/howm/org/remember.org" :level . 1)
				("~/Dropbox/howm/org/pocket.org" :level . 1)
				("~/Dropbox/howm/org/idea.org" :level . 1)
				("~/Dropbox/howm/org/task.org" :level . 1))))

  :init
  ;; Maximize the org-capture buffer
  (defvar my:org-capture-before-config nil
	"Window configuration before 'org-capture'.")
  (defadvice org-capture (before save-config activate)
	"Save the window configuration before 'org-capture'."
	(setq my:org-capture-before-config (current-window-configuration)))
  (add-hook 'org-capture-mode-hook 'delete-other-windows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 60_org-howm.el ends here
