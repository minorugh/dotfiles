;;; init-mini.el --- Emacs minimal configurations.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;; This will start with typing `eq' at shell with minimal Emacs.
;; Write below at .zshrc or .bashrc.
;; alias eq="emacs -q -l ~/.emacs.d/init-mini.el"
;; Use when test of package and my Emacs don't start.
;;; Code:

(when (version< emacs-version "28.1")
  (error "This requires Emacs 28.1 and above!"))

;; Packages
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Encodig and font
(prefer-coding-system 'utf-8)
(add-to-list 'default-frame-alist '(font . "Cica-18"))

;; Better defaults
(setq inhibit-splash-screen t)
(setq-default bidi-display-reordering nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; UI
(load-theme 'misterioso t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-parameter nil 'fullscreen 'maximized)
(global-hl-line-mode 1)
;; (global-display-line-numbers-mode 1)

;; Basic modes
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/tmp/history-mini")
(setq-default show-trailing-whitespace t)
(setq scroll-preserve-screen-position t)

;; Use fido-mode
(fido-mode 1)
(fido-vertical-mode 1)
(setq dired-listing-switches "-AFl --group-directories-first")
(setq default-directory user-emacs-directory)

;; Key Modifiers
(setq select-enable-clipboard  t)
(define-key global-map (kbd "M-w") #'clipboard-kill-ring-save)
(define-key global-map (kbd "C-w") #'clipboard-kill-region)
(define-key global-map (kbd "s-v") #'yank)
(define-key global-map (kbd "s-c") #'clipboard-kill-ring-save)
(define-key global-map (kbd "M-/") #'kill-buffer)
(define-key global-map (kbd "C-_") #'undo)
(define-key global-map (kbd "C-/") #'undo-redo)
(define-key global-map (kbd "C-:") #'switch-to-buffer)
(define-key global-map (kbd "C-x f") #'find-file)
(define-key global-map (kbd "C-x j") #'dired-jump)

;;; Init-mini.el ends here
