;;; 00_base.el --- Emacs base settings  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf emacs-startup-setting
  :config
  ;; Basic modes
  (savehist-mode)
  (save-place-mode)
  (global-auto-revert-mode)
  (blink-cursor-mode)
  (winner-mode)
  (global-font-lock-mode)
  (global-visual-line-mode)
  ;; Misc
  (setq frame-title-format (concat "%b - emacs@" (system-name)))
  (setq ring-bell-function 'ignore)
  (setq scroll-preserve-screen-position :always)
  (setq visible-bell nil)
  (setq scroll-preserve-screen-position t)
  (setq ad-redefinition-action 'accept)
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq mouse-drag-copy-region t)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)
  (setq vc-follow-symlinks t)
  (setq-default bidi-display-reordering nil)
  (setq-default tab-width 4)
  (add-to-list 'default-frame-alist '(alpha . (1.0 0.9)))

  ;; Change the location of the save files
  (setq bookmark-file "~/Dropbox/emacs/bookmarks")
  (setq savehist-file "~/Dropbox/emacs/history")
  (setq recentf-save-file "~/Dropbox/emacs/recentf")
  (setq tramp-persistency-file-name "~/Dropbox/emacs/tram")
  (setq savehist-file "~/Dropbox/emacs/savehist")
  (setq undohist-directory "~/Dropbox/emacs/undohist")
  (setq transient-history-file "~/Dropbox/emacs/history.el")
  (setq prescient-save-file	"~/Dropbox/emacs/prescient-save.el")
  (setq auto-save-list-file-prefix nil)

  ;; Hack emacs-init-time
  (with-eval-after-load "time"
	(defun ad:emacs-init-time ()
	  "Return a string giving the duration of the Emacs initialization."
	  (interactive)
	  (let ((str
			 (format "%.3f seconds"
					 (float-time
					  (time-subtract after-init-time before-init-time)))))
		(if (called-interactively-p 'interactive)
			(message "%s" str)
		  str)))
	(advice-add 'emacs-init-time :override #'ad:emacs-init-time)))


(leaf emacs-base-setting
  :config
  ;; Basic encoding
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)

  ;; Font
  (add-to-list 'default-frame-alist '(font . "Cica-18"))
  (when (string-match "x250" (shell-command-to-string "uname -n"))
    (add-to-list 'default-frame-alist '(font . "Cica-15")))

  ;; Modifires
  (defalias 'exit 'save-buffers-kill-emacs)
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Set buffer that can not be killed
  (with-current-buffer "*scratch*"
	(emacs-lock-mode 'kill))
  (with-current-buffer "*Messages*"
	(emacs-lock-mode 'kill))

  ;; key modifiers
  (bind-key* "<muhenkan>" 'minibuffer-keyboard-quit ivy-minibuffer-map)
  (bind-key "C-," 'xref-find-references)
  (bind-key "C-." 'xref-find-definitions)
  (bind-key "C-q" 'other-window-or-split)
  (bind-key "C-z" 'nil)	;; Do not use suspend-frame
  (bind-key "M-d" 'my:kill-word-at-point)
  (bind-key "M-/" 'kill-buffer)
  (bind-key "s-c" 'clipboard-kill-ring-save)
  (bind-key "s-v" 'clipboard-yank)
  (bind-key "M-w" 'clipboard-kill-ring-save)
  (bind-key "C-w" 'clipboard-kill-region)
  (setq select-enable-clipboard t)
  (setq select-enable-primary t)

  (defun my:kill-word-at-point ()
	"delete word at under cursor. If spaces was under the cursor, delete horizontal spaces"
	(interactive)
	(let ((char (char-to-string (char-after (point)))))
	  (cond
	   ((string= " " char) (delete-horizontal-space))
	   ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
	   (t (forward-char) (backward-word) (kill-word 1)))))

  (defun other-window-or-split ()
	"If there is one window, open split window.
If there are two or more windows, it will go to another window."
	(interactive)
	(when (one-window-p)
	  ;; (split-window-horizontally))
	  (follow-delete-other-windows-and-split))
	(other-window 1))

  ;; M-x info-emacs-manual (C-h r or F1+r)
  (add-to-list 'Info-directory-list (expand-file-name "info" user-emacs-directory))
  (defun Info-find-node--info-ja (orig-fn filename &rest args)
  	"Info as ORIG-FN FILENAME ARGS."
  	(apply orig-fn
  		   (pcase filename
  			 ("emacs" "emacs-ja.info")
  			 (_ filename))
  		   args))
  (advice-add 'Info-find-node :around 'Info-find-node--info-ja)

  :init
  (leaf server
	:require t
	:config
	(unless (server-running-p)
	  (server-start)))

  (leaf exec-path-from-shell
	:ensure t
	:when (memq window-system '(mac ns x))
	:hook (emacs-startup-hook . exec-path-from-shell-initialize)
	:config
	(setq exec-path-from-shell-check-startup-files nil))

  (leaf browse-at-remote
	:ensure t
	:config
	(defalias 'my:github-show 'browse-at-remote))

  (leaf recentf
  	:global-minor-mode t
  	:config
  	(setq recentf-max-saved-items 200)
  	(setq recentf-auto-cleanup 'never)
  	(setq recentf-exclude
  		  '("recentf" "COMMIT_EDITMSG" "bookmarks" "\\.gitignore"
  			"\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" ".howm-keys" "^/tmp/" "^/scp:"
  			(lambda (file) (file-in-directory-p file package-user-dir)))))

  (leaf display-line-numbers
	:bind ("<f9>" . display-line-numbers-mode)
	:hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode))

  (leaf uniquify
	:config
	(setq uniquify-buffer-name-style 'post-forward-angle-brackets))

  (leaf generic-x :require t))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 00_base.el ends here
