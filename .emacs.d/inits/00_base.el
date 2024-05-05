;;; 00_base.el --- Basic configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *basic-configuration
  :config
  ;; Faster rendering by not corresponding to right-to-left language
  (setq bidi-display-reordering nil)
  ;; Do not make a backup file like *.~
  (setq make-backup-files nil)
  ;; Do not use auto save
  (setq auto-save-default nil)
  (setq auto-save-list-file-prefix nil)
  ;; Do not create lock file
  (setq create-lockfiles nil)
  ;; Open symbolic link directly
  (setq vc-follow-symlinks t)
  ;; Do not distinguish uppercase and lowercase letters on completion
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  ;; Point keeps its screen position when scroll
  (setq scroll-preserve-screen-position t)
  ;; All warning sounds and flash are invalid
  (setq ring-bell-function 'ignore)
  ;; Turn off warning sound screen flash
  (setq visible-bell nil)
  ;; Copy text with mouse range selection
  (setq mouse-drag-copy-region t)
  ;; Deleted files go to the trash
  (setq delete-by-moving-to-trash t)
  ;; Tab width default
  (setq tab-width 4)
  ;; Limit the final word to a line break code (automatically correct)
  (setq require-final-newline t)
  ;; Disallow adding new lines with newline at the end of the buffer
  (setq next-line-add-newlines nil)
  ;; Make it easy to see when it is the same name file
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; Disallow adding new lines with newline at the end of the buffer
  (setq next-line-add-newlines nil)
  ;; Make it easy to see when it is the same name file
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; It keeps going steadily the local mark ...  C-u C-SPC C-SPC
  ;; It keeps going steadily the global mark ... C-x C-SPC C-SPC
  (setq set-mark-command-repeat-pop t)
  ;; Use the X11 clipboard
  (setq select-enable-clipboard  t)
  ;;Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows . nil)
  ;; change-default-file-location
  (setq request-storage-directory "~/.emacs.d/tmp/request")
  (setq url-configuration-directory "~/.emacs.d/tmp/url")
  (setq bookmark-file "~/.emacs.d/tmp/bookmarks")
  ;;Goto address
  (add-hook 'prog-mode-hook 'goto-address-prog-mode)
  ;; Change to short command
  (defalias 'yes-or-no-p #'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs)
  ;; Recovery
  (setq save-place-file "~/.emacs.d/tmp/places")
  (add-hook 'after-init-hook 'save-place-mode)
  ;; Savehist
  (setq savehist-file "~/.emacs.d/tmp/history")
  (setq savehist-additional-variables '(kill-ring))
  (add-hook 'after-init-hook 'savehist-mode)
  ;; Recentf
  (setq recentf-auto-cleanup 'never)
  (setq recentf-exclude
	'("\\.howm-keys" "Dropbox/backup" ".emacs.d/tmp/" ".emacs.d/elpa/" "/scp:"))
  (setq recentf-save-file "~/.emacs.d/tmp/recentf")
  (add-hook 'after-init-hook 'recentf-mode))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 00_base.el ends here
