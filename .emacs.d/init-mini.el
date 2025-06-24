;;; init-mini.el --- Centaur Emacs minimal configurations.	-*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2018-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d
;; Version: 1.2.0
;; Keywords: .emacs.d centaur

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;;; init-mini.el --- minimal init -*- no-byte-compile: t; lexical-binding: t -*-
;;; Commentary:
;;
;; This will start with typing `eq' at shell with minimal Emacs.
;; Write below at .zshrc or .bashrc.
;; alias eq="emacs -q -l ~/.emacs.d/mini-init.el"
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
(setq default-directory "~/.emacs.d/")

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better defaults
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; (global-hl-line-mode 1)

;; (if (fboundp 'display-line-numbers-mode)
;;     (global-display-line-numbers-mode 1)
;;   (global-linum-mode 1))

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
(setq recentf-save-file "~/.emacs.d/tmp/recentf")

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Completion
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode 1))

(if (fboundp 'fido-mode)
    (progn
      (fido-mode 1)
      (when (fboundp 'fido-vertical-mode)
        (fido-vertical-mode 1))

      (defun fido-recentf-open ()
        "Use `completing-read' to find a recent file."
        (interactive)
        (if (find-file (completing-read "Find recent file: " recentf-list))
            (message "Opening file...")
          (message "Aborting")))
      (define-key global-map (kbd "C-x C-r") 'fido-recentf-open))
  (progn
    (ido-mode 1)
    (ido-everywhere 1)

    (setq ido-use-virtual-buffers t
	  ido-use-filename-at-point 'guess
	  ido-create-new-buffer 'always
	  ido-enable-flex-matching t)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to find a recent file."
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	  (message "Opening file...")
	(message "Aborting")))
    (define-key global-map (kbd "C-x C-r") 'ido-recentf-open)))

;; Change to short command
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'exit 'save-buffers-kill-emacs)

;; Key Modifiers
(define-key global-map (kbd "s-a") #'mark-whole-buffer)
(define-key global-map (kbd "s-v") #'yank)
(define-key global-map (kbd "s-c") #'kill-ring-save)
(define-key global-map (kbd "s-s") #'save-buffer)
(define-key global-map (kbd "C-_") #'undo)
(define-key global-map (kbd "C-/") #'undo-redo)

;; Keybindings
(define-key global-map (kbd "C-x j") 'dired-jump)
(define-key global-map (kbd "M-/")   'kill-buffer)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(define-key global-map (kbd "<f5>") #'revert-current-buffer)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
