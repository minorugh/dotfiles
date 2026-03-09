;;; 00-base.el --- Better default configurations.-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *basic-configurations
  :config
  ;; Faster rendering by not corresponding to right-to-left language
  (setq-default bidi-display-reordering nil)
  (setq-default bidi-paragraph-direction 'left-to-right)

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
  (setq visible-bell nil)

  ;; Copy text with mouse range selection
  (setq mouse-drag-copy-region t)

  ;; Deleted files go to the trash
  (setq delete-by-moving-to-trash t)

  ;; Limit the final word to a line break code (automatically correct)
  (setq require-final-newline t)

  ;; Disallow adding new lines with newline at the end of the buffer
  (setq next-line-add-newlines nil)

  ;; Make it easy to see when it is the same name file
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

  ;; Use the X11 clipboard
  (setq select-enable-clipboard t)

  ;; Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows nil)

  ;; Minimize the fringe
  (set-fringe-mode 1)

  ;; Change to short command
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'exit 'save-buffers-kill-emacs)
  ;;savehist
  (setq savehist-additional-variables '(kill-ring))
  (setq history-delete-duplicates t)
  ;; Recentf
  (setq recentf-max-saved-items 200)   ;; 多すぎると起動が重くなる
  (setq recentf-auto-cleanup 'never)   ;; 起動時のクリーンアップを抑制
  (setq recentf-exclude
	'("\\.howm-keys" "\\^/session" "task.org"
	  "/.emacs.d/tmp/" "/Dropbox/backup/" "/.emacs.d/elpa/" "/scp:"))

  ;; Associate shell-related dotfiles with sh-mode
  (dolist (pattern '("\\.z?shrc\\'" "\\.bash_profile\\'" "\\.profile\\'"
                     "\\.zshenv\\'" "\\.xprofile\\'" "\\.bashrc\\'" "\\.Xmodmap\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

  ;; Associate config-style dotfiles with conf-mode
  (dolist (pattern '("\\.tmux\\.conf\\'" "\\.muttrc\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'conf-mode)))

  ;; All history files are stored in `~/.emacs.d/tmp'
  (setq request-storage-directory "~/.emacs.d/tmp/request")
  (setq url-configuration-directory "~/.emacs.d/tmp/url")
  (setq bookmark-default-file "~/.emacs.d/tmp/bookmarks")
  (setq save-place-file "~/.emacs.d/tmp/places")
  (setq savehist-file "~/.emacs.d/tmp/history")
  (setq recentf-save-file "~/.emacs.d/tmp/recentf"))


(leaf *defer-modes
  :hook
  (after-init-hook . global-auto-revert-mode)
  (after-init-hook . save-place-mode)
  (after-init-hook . savehist-mode)
  (after-init-hook . recentf-mode)
  (prog-mode-hook  . goto-address-prog-mode))


(leaf *user-configurations
  :defun minibuffer-keyboard-quit my:handle-delete-frame
  :load-path "~/.emacs.d/elisp"
  ;; :require my:template
  :bind (("C-x C-c" . server-edit)
	 ("C-x b"   . ibuffer)
	 ("C-x m"   . neomutt)
	 ("M-,"     . xref-find-definitions)
	 ("M-w"     . clipboard-kill-ring-save)
	 ("C-w"     . my:clipboard-kill-region)
	 ("M-/"     . kill-current-buffer)
	 ("C-x /"   . delete-this-file)
	 ("s-c"     . clipboard-kill-ring-save)
	 ("s-v"     . clipboard-yank)
	 ("C-q"     . other-window-or-split)
	 ([muhenkan] . my:keyboard-quit))
  :init
  (defun my:upcase-word (arg)
    "Convert previous word (or ARG words) to upper case."
    (interactive "p")
    (upcase-word (- arg)))

  (defun my:downcase-word (arg)
    "Convert previous word (or ARG words) to down case."
    (interactive "p")
    (downcase-word (- arg)))

  (defun my:capitalize-word (arg)
    "Convert previous word (or ARG words) to capitalize."
    (interactive "p")
    (capitalize-word (- arg)))

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

  (defun my:clipboard-kill-region ()
    "If the region is active, `clipboard-kill-region'.
If the region is inactive, `backward-kill-word'."
    (interactive)
    (if (use-region-p)
	(clipboard-kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))

  (defun other-window-or-split ()
    "If there is one window, open split window.
If there are two or more windows, it will go to another window."
    (interactive)
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1))

  (defun handle-delete-frame (event)
    "Overwrite `handle-delete-frame` defined in `frame.el'.
If it's the last frame, minimize it without deleting it."
    (interactive "e")
    (let ((frame  (posn-window (event-start event)))
	  (numfrs (length (visible-frame-list))))
      (cond ((> numfrs 1) (delete-frame frame t))
	    ((iconify-frame))))))

;; Autoload my:template functions (lazy load)
(autoload 'my:diary-new-post    "my:template" nil t)
(autoload 'my:tpdia-new-post    "my:template" nil t)
(autoload 'my:teirei-new-post   "my:template" nil t)
(autoload 'my:swan-new-post     "my:template" nil t)
(autoload 'my:m_kukai-new-post  "my:template" nil t)
(autoload 'my:minoru_sen        "my:template" nil t)
(autoload 'my:ap-new-post       "my:template" nil t)
(autoload 'my:apvoice-new-post  "my:template" nil t)
(autoload 'my:tselext-new-post  "my:template" nil t)
(autoload 'my:dselext-new-post  "my:template" nil t)
(autoload 'my:year-new-post     "my:template" nil t)
(autoload 'my:haiku-note        "my:template" nil t)
(autoload 'my:haiku-note-post   "my:template" nil t)


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 00-base.el ends here
