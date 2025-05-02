;;; 00_base.el --- Better default configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *basic-configurations
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
  ;; Use the X11 clipboard
  (setq select-enable-clipboard  t)
  ;; Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows . nil)
  (setq request-storage-directory "~/.emacs.d/tmp/request")
  (setq url-configuration-directory "~/.emacs.d/tmp/url")
  (setq bookmark-file "~/.emacs.d/tmp/bookmarks")
  ;; Default margin
  (setq-default left-margin-width 1 right-margin-width 1)
  ;; Minimize the fringe
  (set-fringe-mode 1)
  ;;Goto address
  (add-hook 'prog-mode-hook 'goto-address-prog-mode)
  ;;Auto revert
  (add-hook 'after-init-hook 'global-auto-revert-mode)
  ;; Change to short command
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs)

  ;; Overwrite handle-delete-frame
  ;; see "https://tinyurl.com/23rah56r"
  (defadvice handle-delete-frame (around my-handle-delete-frame-advice activate)
    "If it's the last frame, minimize it without deleting it."
    (let ((frame   (posn-window (event-start event)))
          (numfrs  (length (visible-frame-list))))
      (when (or (> numfrs 1) (iconify-frame))
        ad-do-it)))

  ;; Recovery
  (setq save-place-file "~/.emacs.d/tmp/places")
  (add-hook 'after-init-hook 'save-place-mode)

  ;; Savehist
  (setq savehist-file "~/.emacs.d/tmp/history")
  (setq savehist-additional-variables '(kill-ring))
  (add-hook 'after-init-hook 'savehist-mode)

  ;; Recentf
  (setq recentf-exclude
	'("\\.howm-keys" "\\^/session" "task.org"
	  "/.emacs.d/tmp/" "/Dropbox/backup/" "/.emacs.d/elpa/" "/scp:"))
  (setq recentf-save-file "~/.emacs.d/tmp/recentf")
  (add-hook 'after-init-hook 'recentf-mode))

;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;;; 00_base.el ends here
