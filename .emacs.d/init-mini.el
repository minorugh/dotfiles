;;; init-mini.el --- Emacs minimal configurations.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;; This will start with typing `eq' at shell with minimal Emacs.
;; Write below at .zshrc or .bashrc.
;; alias eq="emacs -q -l ~/.emacs.d/init-mini.el"
;; Use when test of package and my Emacs don't start.
;;; Code:
(package-initialize)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(set-frame-parameter nil 'fullscreen 'maximized)
(load-theme 'misterioso t)

;; Save the file specified code with basic utf-8 if it exists
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(add-to-list 'default-frame-alist '(font . "Cica-18"))

;;; Faster rendering by not corresponding to right-to-left language
(setq-default bidi-display-reordering nil)
;; Do not make a backup file like *.~
(setq make-backup-files nil)
;; Do not use auto save
(setq auto-save-default nil)
;; Do not create lock file
(setq create-lockfiles nil)
;; When the mouse cursor is close to the text cursor, the mouse hangs away
(if (display-mouse-p) (mouse-avoidance-mode 'exile))
;; Disable automatic save
(setq auto-save-default nil)
;; Do not distinguish uppercase and lowercase letters on completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; Display the character position of the cursor
(column-number-mode t)

;; Save history of minibuffer
(savehist-mode 1)
(setq history-length 10000)
(setq savehist-file "~/.emacs.d/tmp/history-mini")
;; Make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)
;; Turn off warning sound screen flash
(setq visible-bell nil)
;; All warning sounds and flash are invalid (note that the warning sound does not sound completely)
(setq ring-bell-function 'ignore)
;; Brace the corresponding parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
;; Highlight the space at the end of the line
(setq-default show-trailing-whitespace t)
;; Do not change the position of the cursor on the screen as much as possible when scrolling pages
(setq scroll-preserve-screen-position t)

;; Use fido-mode
(fido-mode 1)
(fido-vertical-mode 1)
(setq dired-listing-switches "-AFl --group-directories-first")
(setq default-directory user-emacs-directory)

;; Use the X11 clipboard
(setq select-enable-clipboard  t)
(define-key global-map (kbd "M-w") 'clipboard-kill-ring-save)
(define-key global-map (kbd "C-w") 'clipboard-kill-region)

;; Key Modifiers
(define-key global-map (kbd "M-/")   #'kill-buffer)
(define-key global-map (kbd "C-_")   #'undo)
(define-key global-map (kbd "C-/")   #'undo-redo)
(define-key global-map (kbd "C-:")   #'switch-to-buffer)
(define-key global-map (kbd "C-x f") #'find-file)
(define-key global-map (kbd "C-x j") #'dired-jump)

;;; Init-mini.el ends here
