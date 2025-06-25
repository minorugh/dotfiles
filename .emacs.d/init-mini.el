;;; init-mini.el --- Emacs minimal configurations.	-*- lexical-binding: t no-byte-compile: t -*-
;;
;;; Commentary:
;;
;; This will start with typing `eq' at shell with minimal Emacs.
;; Write below at .zshrc or .bashrc.
;; alias eq="emacs -q -l ~/.emacs.d/init-mini.el"
;; Use when test of package and my Emacs don't start.
;;; Code:

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(set-frame-parameter nil 'fullscreen 'maximized)
(load-theme 'misterioso t)

;; Load path
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)
(push (expand-file-name "lisp" user-emacs-directory) load-path)
(setq default-directory user-emacs-directory)

;; Packages
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Encoding
(prefer-coding-system 'utf-8)

;; Better defaults
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ;; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ;; Forbide to make backup files
(setq auto-save-default nil)               ;; Disable auto save
(setq set-mark-command-repeat-pop t)       ;; Repeating C-SPC after popping mark pops it again
(setq dired-listing-switches "-AFl --group-directories-first") ;; Diectry first

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(global-hl-line-mode 1)
;; (global-display-line-numbers-mode 1)

;; Basic modes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(setq auto-save-default nil)
(when (fboundp 'savehist-mode)
  (savehist-mode 1))
(if (fboundp 'save-place-mode)
    (save-place-mode 1)
  (require 'saveplace)
  (setq-default save-place t))
(setq savehist-file "~/.emacs.d/tmp/history")
(setq save-place-file "~/.emacs.d/tmp/places")
(setq recentf-save-file "~/.emacs.d/tmp/recentf-test")

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Completion
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode 1))

;; fido-mode
(when (fboundp 'fido-mode)
  (fido-mode 1)
  (fido-vertical-mode 1)
  (defun fido-recentf-open ()
    "Use `completing-read' to find a recent file."
    (interactive)
    (if (find-file (completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))
(define-key global-map (kbd "C-x r") 'fido-recentf-open)

;; Change to short command
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'exit 'save-buffers-kill-emacs)

(defun my:keyboard-quit ()
  "Hoge."
  (interactive)
  (if (not (use-region-p))
      (minibuffer-keyboard-quit)
    (keyboard-quit)))

;; Key Modifiers
(define-key global-map (kbd "s-a")   #'mark-whole-buffer)
(define-key global-map (kbd "s-v")   #'yank)
(define-key global-map (kbd "s-c")   #'kill-ring-save)
(define-key global-map (kbd "s-s")   #'save-buffer)
(define-key global-map (kbd "M-/")   #'kill-buffer)
(define-key global-map (kbd "C-_")   #'undo)
(define-key global-map (kbd "C-/")   #'undo-redo)
(define-key global-map (kbd "C-:")   #'switch-to-buffer)
(define-key global-map (kbd "C-x f") #'find-file)
(define-key global-map (kbd "C-x j") #'dired-jump)
(define-key global-map (kbd "<muhenkan>") #'my:keyboard-quit)

;;; Init-mini.el ends here
