;;; 00_base.el --- Emacs default configurations. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf emacs-default-setting
  :config
  ;; Basic modes
  (savehist-mode)
  (save-place-mode)
  (global-auto-revert-mode)
  (winner-mode)
  (global-font-lock-mode)
  (global-visual-line-mode)

  ;; Misc
  (setq frame-title-format (concat "%b - Emacs@" (system-name)))
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
  (setq auto-save-list-file-prefix nil)
  (setq create-lockfiles nil)
  (setq vc-follow-symlinks t)
  (setq-default bidi-display-reordering nil)
  (setq-default tab-width 4)
  (add-to-list 'default-frame-alist '(alpha . (1.0 0.9)))

  ;; Change the location of the tmp files for maine machine e590
  (setq url-configuration-directory "~/.emacs.d/tmp/url")
  (setq request-storage-directory "~/.emacs.d/tmp/request")
  (setq bookmark-file "~/.emacs.d/tmp/bookmarks")
  (setq savehist-file "~/.emacs.d/tmp/savehist")
  (setq save-place-file "~/.emacs.d/tmp/places")
  (setq transient-history-file "~/.emacs.d/tmp/transient-history")
  (setq recentf-save-file "~/.emacs.d/tmp/recentf")

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
  (add-to-list 'default-frame-alist '(font . "Cica-19.5"))
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
  (bind-key "C-z" 'nil)	;; Do not use suspend-frame
  (bind-key "M-/" 'kill-buffer)
  (bind-key "s-c" 'clipboard-kill-ring-save)
  (bind-key "s-v" 'clipboard-yank)
  (bind-key "M-w" 'clipboard-kill-ring-save)
  (bind-key "C-w" 'clipboard-kill-region)
  (setq select-enable-clipboard t)
  (setq select-enable-primary t)

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
  (defun Info-find-node--elisp-ja (orig-fn filename &rest args)
  	"Info as ORIG-FN FILENAME ARGS."
	(apply orig-fn
		   (pcase filename
			 ("elisp" "elisp-ja.info")
			 (t filename))
		   args))
  (advice-add 'Info-find-node :around 'Info-find-node--elisp-ja)

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
			"\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" ".howm-keys" "^//" "^/scp:"
			(lambda (file) (file-in-directory-p file package-user-dir))))
	(defun recentf-suppress-messages (orig-func &rest args)
	  "Suppress message output in minibuffer."
	  (setq inhibit-message t)
	  (apply orig-func args)
	  (setq inhibit-message nil)
	  'around)
	(advice-add 'recentf-cleanup   :around 'recentf-suppress-messages)
	(advice-add 'recentf-save-list :around 'recentf-suppress-messages))

  (leaf display-line-numbers
	:bind ("<f9>" . display-line-numbers-mode)
	:hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode))

  (leaf uniquify
	:config
	(setq uniquify-buffer-name-style 'post-forward-angle-brackets))

  (leaf generic-x :require t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 00_base.el ends here
