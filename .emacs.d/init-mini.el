;;; test.el --- test-init -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This will start with typing `eq' at shell with minimal Emacs.
;; Write below at .zshrc or .bashrc.
;; alias eq="emacs -q -l ~/.emacs.d/mini-init.el"
;; Use when test of package and my Emacs don't start.
;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(set-frame-parameter nil 'fullscreen 'maximized)

(load-theme 'misterioso t)

;; Package
;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(unless (bound-and-true-p package--initialized) ;; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ;; To prevent initializing twice
  (package-initialize))

;; Save the file specified code with basic utf-8 if it exists
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; font
(add-to-list 'default-frame-alist '(font . "Cica-21"))

;;; Faster rendering by not corresponding to right-to-left language
(setq-default bidi-display-reordering nil)

;; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;; Do not make a backup file like *.~
(setq make-backup-files nil)
;; Do not use auto save
(setq auto-save-default nil)
;; Do not create lock file
(setq create-lockfiles nil)

;; Dired with directory first
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil ls-lisp-dirs-first t)

;; C-h is backspace
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; Run M-/ same kill-buffer as C-x k
(define-key global-map (kbd "M-/") 'kill-this-buffer)

;; When the mouse cursor is close to the text cursor, the mouse hangs away
(if (display-mouse-p) (mouse-avoidance-mode 'exile))

;; Disable automatic save
(setq auto-save-default nil)

;; Do not distinguish uppercase and lowercase letters on completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Make it easy to see when it is the same name file
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Display the character position of the cursor
(column-number-mode t)

;; Save history of minibuffer
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/tmp/history")

;; Make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off warning sound screen flash
(setq visible-bell nil)
;; All warning sounds and flash are invalid (note that the warning sound does not sound completely)
(setq ring-bell-function 'ignore)

;; Use the X11 clipboard
(setq select-enable-clipboard  t)
(define-key global-map (kbd "M-w") 'clipboard-kill-ring-save)
(define-key global-map (kbd "C-w") 'clipboard-kill-region)

;; ivy
(if (package-installed-p 'counsel)
	(ivy-mode 1)
  (define-key global-map (kbd "C-s") 'swiper-thing-at-point))

;; Brace the corresponding parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; Continue hitting C-SPC and go back to past marks ...C-u C-SPC C-SPC
(setq set-mark-command-repeat-pop t)

;; Assign ibuffer to C-x C-b
(define-key global-map (kbd "C-x C-b") 'ibuffer)

;; Read elisp function source file
(define-key global-map (kbd "C-x F") 'find-function)
(define-key global-map (kbd "C-x V") 'find-variable)

;; Highlight the space at the end of the line
(setq-default show-trailing-whitespace t)

;; Do not change the position of the cursor on the screen as much as possible when scrolling pages
(setq scroll-preserve-screen-position t)

;; contain many mode setting
(require 'generic-x)


(provide 'init-mini)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init-mini.el ends here
