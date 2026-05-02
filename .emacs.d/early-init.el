;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;; Commentary:
;; Configures early-stage initialization (pre-init.el/packages).
;; X11/Display settings are offloaded to ~/.Xresources to ensure:
;; - No XIM lag (Emacs*useXIM: false)
;; - Proper font scaling (Xft.dpi: 120)
;; - Instant dark frame (Emacs.background/foreground) to prevent "white flash" artifact.
;; Apply changes with: xrdb -merge ~/.Xresources
;;; Code:
;; (setq debug-on-error t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Disable JIT compilation to prevent background CPU spikes at startup
(setq native-comp-jit-compilation nil)

;; Prevent automatic package initialization to handle it manually in init.el
(setq package-enable-at-startup nil)

;; Prioritize newer source files and skip mtime checks to save IO time
(setq load-prefer-newer noninteractive)

;; Improve UI snappiness by inhibiting frame resizing
(setq frame-inhibit-implied-resize t)

;; Force UTF-8 encoding to avoid OS-specific prompts
(prefer-coding-system 'utf-8)

;; Language and font configuration
(set-language-environment "Japanese")
(setq-default indent-line-function 'indent-for-current-mode)
(push '(font . "Cica-18") default-frame-alist)

;; Disable UI elements early to prevent flickering and speed up rendering
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)

;; Start with a maximized frame and hide the splash screen
(push '(fullscreen . maximized) initial-frame-alist)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
;; Do not force *scratch* on startup; let dashboard take over
(setq initial-buffer-choice nil)


(provide 'early-init)
;;; early-init.el ends here
