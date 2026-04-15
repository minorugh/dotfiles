;;; early-init.el --- Early initialization.  -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Note: Some settings that affect Emacs at the X11/display level are
;; intentionally configured in ~/.Xresources rather than here, because
;; they must be applied before Emacs starts.  The relevant entries are:
;;
;;   Emacs*useXIM: false
;;       Disables XIM (X Input Method) integration.  Avoids input lag
;;       and conflicts when using input methods such as Fcitx or IBus.
;;
;;   Xft.dpi: 120
;;       Sets the display DPI for font rendering via Xft.
;;       Adjust this value to match your monitor's actual DPI.
;;
;;   Emacs.background: #282c36
;;   Emacs.foreground: #f8f8f2
;;       Base colors for the doom-dracula theme, applied at the X11
;;       resource level so that the initial frame has the correct colors
;;       before Emacs finishes loading the theme.  This prevents the
;;       brief white-flash artifact on startup.
;;
;; To apply changes to ~/.Xresources, run:
;;   xrdb -merge ~/.Xresource

;;; Code:
;; (setq debug-on-error t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-jit-compilation nil)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Set language & font
(set-language-environment "Japanese")
(push '(font . "Cica-18") default-frame-alist)

;; Faster to disable these here (before they've been initialized)
(setq-default mode-line-format nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated . t) default-frame-alist)

;; Default frame settings. This is actually maximized, not full screen.
(push '(fullscreen . maximized) initial-frame-alist)

;; Inhibit startup screen.
(setq inhibit-startup-message t)

;; Disable saving Emacs state via the Session Manager
(setq emacs-save-session-functions nil)

(provide 'early-init)
;;; early-init.el ends here
