;;; 00-base.el --- Better default configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *basic-configurations
  :init
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

  ;; Minimize the fringe
  (set-fringe-mode 1)

  ;;Goto address
  (add-hook 'prog-mode-hook 'goto-address-prog-mode)

  ;;Auto revert
  (add-hook 'after-init-hook 'global-auto-revert-mode)

  ;; Change to short command
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs)

  ;; Recovery
  (setq save-place-file "~/.emacs.d/tmp/places")
  (add-hook 'after-init-hook 'save-place-mode)

  ;; Savehist
  (setq savehist-file "~/.emacs.d/tmp/history")
  (setq savehist-additional-variables '(kill-ring))
  (add-hook 'after-init-hook 'savehist-mode)

  ;; Set language & font
  (set-language-environment "UTF-8")
  (add-to-list 'default-frame-alist '(font . "Cica-18"))

  ;; Recentf
  (setq recentf-exclude
	'("\\.howm-keys" "\\^/session" "task.org"
	  "/.emacs.d/tmp/" "/Dropbox/backup/" "/.emacs.d/elpa/" "/scp:"))
  (setq recentf-save-file "~/.emacs.d/tmp/recentf")
  (add-hook 'after-init-hook 'recentf-mode))


(leaf *cus-user-configrations
  :load-path "~/.emacs.d/elisp/"
  :require my:dired my:template my:make-command evil-easy-hugo
  :bind (("C-x C-c"  . server-edit)  ;; Server editing buffers exist. Replace "C-x #"
	 ("C-x b"    . ibuffer)      ;; Overwrite switch-to-buffer
	 ("C-x m"    . neomutt)      ;; Overwrite compose-maile
	 ("M-,"      . xref-find-definitions)
	 ("M-w"      . clipboard-kill-ring-save)
	 ("C-w"      . kill-word-or-region)
	 ("M-/"      . kill-current-buffer)
	 ("C-M-/"    . delete-this-file)
	 ("s-c"      . clipboard-kill-ring-save) ;; Like macOS,eq Win 'C-c'
	 ("s-v"      . clipboard-yank)           ;; Like macOS,eq Win 'C-v'
	 ([muhenkan] . my:keyboard-quit))
  :init
  (defun neomutt ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (compile "neomutt.sh"))
  (setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

  (defun my:keyboard-quit ()
    (interactive)
    (if (not (use-region-p))
	(minibuffer-keyboard-quit)
      (keyboard-quit)))

  (defun delete-this-file ()
    "Delete the current file, and kill the buffer."
    (interactive)
    (unless (buffer-file-name)
      (error "No file is currently being edited"))
    (when (yes-or-no-p (format "Really delete '%s'?"
			       (file-name-nondirectory buffer-file-name)))
      (delete-file (buffer-file-name))
      (kill-current-buffer)))

  (defun kill-word-or-region ()
    "If the region is active, `clipboard-kill-region'.
If the region is inactive, `backward-kill-word'."
    (interactive)
    (if (use-region-p)
	(clipboard-kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))

  (defun emacs-init-time ()
    "Overwrite `emacs-init-time' defined in time.el."
    (interactive)
    (let ((str
	   (format "%.3f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))))
      (if (called-interactively-p 'interactive)
	  (message "%s" str)
	str)))

  (defun handle-delete-frame (event)
    "Overwrite `handle-delete-frame` defined in `frame.el`.
  If it's the last frame, minimize it without deleting it."
    (interactive "e")
    (let ((frame  (posn-window (event-start event)))
	  (numfrs (length (visible-frame-list))))
      (cond ((> numfrs 1) (delete-frame frame t))
	    ((iconify-frame))))))

;; Auto byte-compile for inits files
(defun auto-compile-inits ()
  "Byte-compile Lisp files modified in the directory."
  (interactive)
  (compile "make clean")
  (byte-compile-file "~/.emacs.d/early-init.el")
  (byte-compile-file "~/.emacs.d/init.el")
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elisp") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/inits") 0))
  (add-hook 'kill-emacs-hook 'auto-compile-inits)

;;; 00-base.el ends here
