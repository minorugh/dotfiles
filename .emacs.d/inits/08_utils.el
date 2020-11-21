;;; 08_utils.el --- misc utils  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf utility-for-buffer
  :init
  (leaf auto-save-buffers-enhanced
	:ensure t
	:config
	(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "^/scp:" "/sudo:"))
	(setq auto-save-buffers-enhanced-quiet-save-p t)
	(auto-save-buffers-enhanced t))

  (leaf persistent-scratch
	:ensure t
	:init
	(setq persistent-scratch-save-file "~/Dropbox/emacs/persistent-scratch")
	:config
	(persistent-scratch-setup-default))

  (leaf undohist
	:ensure t
	:hook (emacs-startup-hook . undohist-initialize)
	:config
	(setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

  (leaf undo-tree
  	:ensure t
	:chord ("uu" . undo-tree-visualize)
	:config
	(global-undo-tree-mode t)
	(make-variable-buffer-local 'undo-tree-visualizer-diff)
	(setq-default undo-tree-visualizer-diff t)
	(setq undo-tree-visualizer-timestamps t)
	(setq undo-tree-visualizer-diff t)
	(setq undo-tree-enable-undo-in-region nil)
	(setq undo-tree-auto-save-history nil)))


(leaf utility-for-misc
  :init
  (leaf migemo
	:ensure t
	:if (executable-find "cmigemo")
	:config
	(setq migemo-command (executable-find "cmigemo"))
	(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
	(autoload 'migemo-init "migemo" nil t)
	(migemo-init))

  (leaf imenu-list
	:ensure t
	:bind ("<f2>" . imenu-list-smart-toggle)
	:config
	(setq imenu-list-size 30)
	(setq imenu-list-position 'left)
	(setq imenu-list-focus-after-activation t))

  (leaf sequential-command
	:ensure t
	:config
	(leaf sequential-command-config
	  :hook (emacs-startup-hook . sequential-command-setup-keys)))

  (leaf ps-print-setting
	:init
	(defalias 'ps-mule-header-string-charsets 'ignore)
	(setq ps-multibyte-buffer 'non-latin-printer)
	(setq ps-paper-type 'a4)
	(setq ps-font-size 9)
	(setq ps-printer-name nil)
	(setq ps-print-header nil)
	(setq ps-show-n-of-n t)
	(setq ps-line-number t)
	(setq ps-print-footer nil))

  (leaf pdfout-from-emacs
	:config
	(setq my:pdfout-command-format "nkf -e | e2ps -a4 -p -nh | ps2pdf - %s")
	:init
	(defun pdfout-select ()
	  "PDF out select menu."
	  (interactive)
	  (counsel-M-x "^my:pdfout "))
	(defun my:pdfout-buffer ()
	  "PDF out from buffer."
	  (interactive)
	  (my:pdfout-region (point-min) (point-max)))
	(defun my:pdfout-region (begin end)
	  "PDF out from BEGIN to END of region."
	  (interactive "r")
	  ;; (shell-command-on-region begin end my:pdfout-command-format)))
	  (shell-command-on-region begin end (format my:pdfout-command-format
												 (concat (read-from-minibuffer "File name:") ".pdf"))))))


(leaf utils-user-functions
  :config
  (bind-key "<f3>" 'filer-current-dir-open)
  (bind-key "<f4>" 'term-current-dir-open)
  (bind-key "<S-return>" 'toggle-scratch)
  :init
  (defun toggle-scratch ()
	"Toggle current buffer and *scratch* buffer."
	(interactive)
	(if (not (string= "*scratch*" (buffer-name)))
		(progn
		  (setq toggle-scratch-prev-buffer (buffer-name))
		  (switch-to-buffer "*scratch*"))
	  (switch-to-buffer toggle-scratch-prev-buffer)))

  (defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
	(when (get-buffer "*tramp/scp xsrv*")
	  (counsel-tramp-quit))
	(message "Killed Other Buffers!"))

  (defun my:jpg2png ()
	"Convert jpg to ping with mogrify."
	(interactive)
	(compile "mkdir ~/Desktop/output | mogrify -path ~/Desktop/output  -format png *"))

  (defun filer-current-dir-open ()
	"Open filer in current dir."
	(interactive)
	(compile (concat "nautilus " default-directory)))

  (defun term-current-dir-open ()
	"Open terminal application in current dir."
	(interactive)
	(let ((dir (directory-file-name default-directory)))
	  (compile (concat "gnome-terminal --working-directory " dir))))

  (defun my:delete-file-if-no-contents ()
	"Automatic deletion for empty files (Valid in all modes)."
	(when (and (buffer-file-name (current-buffer))
			   (= (point-min) (point-max)))
	  (delete-file
	   (buffer-file-name (current-buffer)))))
  (if (not (memq 'my:delete-file-if-no-contents after-save-hook))
	  (setq after-save-hook
			(cons 'my:delete-file-if-no-contents after-save-hook)))

  (defun browse-calendar ()
	"Open Google-calendar with chrome."
	(interactive)
	(browse-url "https://calendar.google.com/calendar/r"))

  (defun browse-weather ()
	"Open tenki.jp with chrome."
	(interactive)
	(browse-url "https://tenki.jp/week/6/31/"))

  (defun browse-google-news ()
	"Open Google-news with chrome."
	(interactive)
	(browse-url "https://news.google.com/topstories?hl=ja&gl=JP&ceid=JP:ja"))

  (defun browse-pocket ()
	"Open pocket with chrome."
	(interactive)
	(browse-url "https://getpocket.com/a/queue/"))

  (defun browse-keep ()
	"Open pocket with chrome."
	(interactive)
	(browse-url "https://keep.new/"))

  (defun browse-homepage ()
	"Open my homepage with crome."
	(interactive)
	(browse-url "https://gospel-haiku.com/"))

  (defun browse-gmail ()
	"Open gmail with chrome."
	(interactive)
	(browse-url "https://mail.google.com/mail/"))

  (defun browse-tweetdeck ()
	"Open tweetdeck with chrome."
	(interactive)
	(browse-url "https://tweetdeck.twitter.com/"))

  (defun browse-slack ()
	"Open slack with chrome."
	(interactive)
	(browse-url "https://emacs-jp.slack.com/messages/C1B73BWPJ/")))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 08_utils.el ends here
