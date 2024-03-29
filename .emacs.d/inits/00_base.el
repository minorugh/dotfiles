;;; 00_base.el --- Basic configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *generic-configuration
  :defun (ad:emacs-init-time)
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
	;; Disable warnings at initialization
	(warning-minimum-level . :emergency)
	;; change-default-file-location
	(request-storage-directory . "~/.emacs.d/tmp/request")
	(url-configuration-directory . "~/.emacs.d/tmp/url")
	(bookmark-file . "~/.emacs.d/tmp/bookmarks"))
  :config
  ;; Share PATH from shell environment variables
  (leaf exec-path-from-shell :ensure t
	:when (memq window-system '(mac ns x))
	:hook (emacs-startup-hook . exec-path-from-shell-initialize)
	:custom
	(exec-path-from-shell-check-startup-files . nil))

  ;; Change to short command
  (defalias 'yes-or-no-p #'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs)

  ;; Encoding
  ;; (set-language-environment "Japanese")
  ;; (prefer-coding-system 'utf-8)

  ;; Fonts
  ;; (if (string-match "e590" (shell-command-to-string "uname -n"))
  ;; 	  (add-to-list 'default-frame-alist '(font . "Cica-20"))
  ;; 	(add-to-list 'default-frame-alist '(font . "Cica-15")))

  ;; Recentf
  (setq recentf-auto-cleanup 'never)
  (setq recentf-exclude
		'("\\.howm-keys" "Dropbox/backup" ".emacs.d/tmp/" ".emacs.d/elpa/" "/scp:"))
  (setq recentf-save-file "~/.emacs.d/tmp/recentf")
  (add-hook 'after-init-hook 'recentf-mode)

  ;; Autorevert
  (setq auto-revert-interval 0.1)
  (add-hook 'after-init-hook 'global-auto-revert-mode)

  ;; Goto address
  (add-hook 'prog-mode-hook 'goto-address-prog-mode)

  ;; Recovery
  (setq save-place-file "~/.emacs.d/tmp/places")
  (add-hook 'after-init-hook 'save-place-mode)

  ;; Savehist
  (setq savehist-file "~/.emacs.d/tmp/history")
  (setq savehist-additional-variables '(kill-ring))
  (add-hook 'after-init-hook 'savehist-mode)

  ;;Server
  (defun restart-server ()
	"Server Start."
	(interactive)
	(eval-and-compile (require 'server))
	(unless (server-running-p)
	  (server-start)))
  (add-hook 'emacs-startup-hook 'restart-server)

  ;; Emacs init time
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
