;;; 00_base.el --- Basic configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ---------------------------------------------------------------------
;; Generic Configurations
;; ---------------------------------------------------------------------
(leaf *generic-configurations
  :custom
  `(;; No startup screen appears
	(inhibit-splash-screen . t)
	;; Faster rendering by not corresponding to right-to-left language
	(bidi-display-reordering . nil)
	;; Do not make a backup file like *.~
	(make-backup-files . nil)
	;; Do not use auto save
	(auto-save-default . nil)
	(auto-save-list-file-prefix . nil)
	;; Do not create lock file
	(create-lockfiles . nil)
	;; Open symbolic link directly
	(vc-follow-symlinks . t)
	;; Do not distinguish uppercase and lowercase letters on completion
	(completion-ignore-case . t)
	(read-file-name-completion-ignore-case . t)
	;; Point keeps its screen position when scroll
	(scroll-preserve-screen-position . t)
	;; All warning sounds and flash are invalid
	(ring-bell-function . 'ignore)
	;; Turn off warning sound screen flash
	(visible-bell . nil)
	;; Copy text with mouse range selection
	(mouse-drag-copy-region . t)
	;; Deleted files go to the trash
	(delete-by-moving-to-trash . t)
	;; Tab width default
	(tab-width . 4)
	;; Limit the final word to a line break code (automatically correct)
	(require-final-newline . t)
	;; Disallow adding new lines with newline at the end of the buffer
	(next-line-add-newlines . nil)
	;; Make it easy to see when it is the same name file
	(uniquify-buffer-name-style . 'post-forward-angle-brackets)
	;; It keeps going steadily the local mark ...  C-u C-SPC C-SPC
	;; It keeps going steadily the global mark ... C-x C-SPC C-SPC
	(set-mark-command-repeat-pop . t)
	;; Use the X11 clipboard
	(select-enable-clipboard  . t)
	;; change-default-file-location
	(url-configuration-directory . "~/.emacs.d/tmp/url")
	(bookmark-file . "~/.emacs.d/tmp/bookmarks"))
  :config
  (leaf *define-alias
	:doc "Change to short command"
	:config
	(defalias 'yes-or-no-p #'y-or-n-p)
	(defalias 'exit 'save-buffers-kill-emacs))

  (leaf *encoding
	:doc "Save the file specified code with basic utf-8 if it exist"
	:config
	(set-language-environment "Japanese")
	(prefer-coding-system 'utf-8))

  (leaf *fonts
	:doc "Set font for main machine or other"
	:config
	(if (string-match "e590" (shell-command-to-string "uname -n"))
		(add-to-list 'default-frame-alist '(font . "Cica-18"))
	  (add-to-list 'default-frame-alist '(font . "Cica-15"))))

  (leaf *autorevert
	:doc "Revert changes if local file is updated"
	:hook (after-init-hook . global-auto-revert-mode)
	:custom (auto-revert-interval . 0.1))

  (leaf *follow-mode
	:doc "Scroll two windows displaying as one virtual window"
	:bind ([f8] . follow-mode))

  (leaf *display-line-numbers
	:doc "Show line numbers"
	:hook ((after-init-hook . global-display-line-numbers-mode)
		   ((dired-mode-hook
			 neotree-mode-hook
			 lisp-interaction-mode-hook
			 eshell-mode-hook) . (lambda () (display-line-numbers-mode -1))))
	:bind ([f9] . display-line-numbers-mode)
	:custom (display-line-numbers-width-start . t))

  (leaf *goto-address
	:doc "Display URL as link, Open with mouse or 'C-c RET'"
	:hook (prog-mode-hook . goto-address-prog-mode))

  (leaf exec-path-from-shell
	:doc "Share PATH from shell environment variables"
	:url "https://github.com/purcell/exec-path-from-shell"
	:ensure t
	:when (memq window-system '(mac ns x))
	:hook (after-init-hook . exec-path-from-shell-initialize)
	:custom
	(exec-path-from-shell-check-startup-files . nil))

  (leaf *server-start
	:doc "Server start for emacs-client"
	:require server
	:config
	(unless (server-running-p)
	  (add-hook 'after-init-hook 'server-start)))

  (leaf *recovery
	:doc "Save place of cursor"
	:hook (after-init-hook . save-place-mode)
	:custom (save-place-file . "~/.emacs.d/tmp/places"))

  (leaf *savehist
	:doc "Edit remote file via SSH or SCP"
	:hook (after-init-hook . savehist-mode)
	:custom
	`((savehist-file . "~/.emacs.d/tmp/history")
	  (savehist-additional-variables . '(kill-ring))))

  (leaf *recentf
	:doc "Record open files history"
	:hook (after-init-hook . recentf-mode)
	:custom
	`((recentf-auto-cleanup . 'never)
	  (recentf-exclude
	   . '("\\.howm-keys" "Dropbox/backup" ".emacs.d/tmp/" ".emacs.d/elpa/" "/scp:"))
	  (recentf-save-file . "~/.emacs.d/tmp/recentf")))

  (leaf auto-async-byte-compile
	:ensure t
	:require t
	:init
	(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

  (defun ad:emacs-init-time ()
	"Advice `emacs-init-time'."
	(interactive)
	(let ((str
		   (format "%.3f seconds"
				   (float-time
					(time-subtract after-init-time before-init-time)))))
	  (if (called-interactively-p 'interactive)
		  (message "%s" str)
		str)))
  (advice-add 'emacs-init-time :override #'ad:emacs-init-time))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 00_base.el ends here
