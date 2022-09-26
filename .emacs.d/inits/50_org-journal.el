;;; 50_org-journal.el --- Org journal configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf org-journal
  :doc "https://www.emacswiki.org/emacs/OrgJournal"
  :ensure t
  :chord (",," . hydra-journal/body)
  :bind ((:org-journal-mode-map
		  ("<muhenkan>" . org-journal-save-entry-and-exit))
		 ("C-c j" . org-journal-new-entry)
		 ("C-c t" . journal-file-today)
		 ("C-c y" . journal-file-yesterday))
  :custom
  `((org-journal-dir . "~/Dropbox/howm/org/journal/")
	(org-journal-file-format . "%Y%m%d.org")
	(org-journal-date-format . "%Y-%m-%d (%A)")
	(org-journal-find-file . 'find-file))
  :hydra
  (hydra-journal
   (:color red :hint nil)
   "
    Journal…  New_,_  Task_._  Yest.today:_[__]_  TODO._;_  Search_@_  Agenda_:_  Exit_/_"
   ("," org-journal-new-entry)
   ("." org-journal-new-scheduled-entry)
   ("]" org-journal-open-current-journal-file)
   ("[" journal-file-yesterday)
   (":" (org-agenda nil "a"))
   (";" my:org-journal-schedule-view)
   ("@" org-journal-search-forever)
   ("/" kill-this-buffer :exit t))
  :config
  (defun my:org-journal-schedule-view ()
	"Turn on `darkroom', disable to `line-numbers' & `view-mode'."
	(interactive)
	(org-journal-schedule-view)
	(darkroom-mode 1)
	(display-line-numbers-mode 0)
	(view-mode 0))

  (defun get-journal-file-yesterday ()
	"Gets filename for yesterday's journal entry."
	(let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
           (daily-name (format-time-string "%Y%m%d.org" yesterday)))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
	"Creates and load a file based on yesterday's date."
	(interactive)
	(find-file (get-journal-file-yesterday)))

  (defun org-journal-file-header-func (time)
	"Custom function to create journal header."
	(concat
	 (pcase org-journal-file-type
       (`daily "#+STARTUP: content indent inlineimages"))))
  (setq org-journal-file-header 'org-journal-file-header-func)
  :init
  ;; Auto Darkroom for journal dir
  (defvar my:journal-dirs nil)
  (add-to-list 'my:journal-dirs "~/Dropbox/howm/org/journal/")

  (defun my:journal-auto-darkroom ()
	"Enable `darkroom', disable `line-numbers'."
	(interactive)
	(dolist (dir my:journal-dirs)
	  (when (eq 0 (string-match (expand-file-name dir) buffer-file-name))
		(darkroom-mode 1)
		(display-line-numbers-mode 0))))
  (add-hook 'find-file-hook 'my:journal-auto-darkroom))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_org-journal.el ends here
