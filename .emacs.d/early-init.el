;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;; Commentary:
;; Configures early-stage initialization (pre-init.el/packages).
;; X11/Display settings are offloaded to ~/.Xresources to ensure:
;; - Proper font scaling     (Xft.dpi: 120)
;; - Instant dark frame      (Emacs.background/foreground) — prevents "white flash"
;; - No silver mode-line     (set-face-attribute) — prevents flash before theme loads
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Startup Performance
;; ============================================================

;; Defer GC during startup; restored to 16MB in init.el's startup hook
(setq gc-cons-threshold most-positive-fixnum)

;; Disable JIT compilation to prevent background CPU spikes at startup
(setq native-comp-jit-compilation nil)

;; Skip package init here; handled manually in init.el
(setq package-enable-at-startup nil)

;; Always prefer newer source files to prevent stale .elc bugs
(setq load-prefer-newer t)

;; Inhibit frame resizing to improve UI snappiness
(setq frame-inhibit-implied-resize t)


;; ============================================================
;;  Language, Encoding & Font
;; ============================================================

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(let ((font-size (if (string= (system-name) "P1") 18 16)))
  (push `(font . ,(format "Cica-%d" font-size)) initial-frame-alist))

(setq inhibit-compacting-font-caches t)


;; ============================================================
;;  UI — Disable Early to Prevent Flicker
;; ============================================================

(push '(menu-bar-lines    . 0) default-frame-alist)
(push '(tool-bar-lines    . 0) default-frame-alist)
(push '(vertical-scroll-bars ) default-frame-alist)
(push '(undecorated       . t) default-frame-alist)

;; ============================================================
;;  Frame Position & Splash Screen
;; ============================================================

;; Start maximized on external monitor (DP-1-2 at x=1920)
(push '(fullscreen . maximized) initial-frame-alist)
(push '(left . 1920)            initial-frame-alist)
(push '(top  . 0)               initial-frame-alist)

(setq inhibit-startup-message t)
(setq inhibit-startup-screen  t)

;; Leave the initial buffer selection to dashboard.
(setq initial-buffer-choice nil)

;; Prevent Customize from writing directly to init.el
(setq custom-file (locate-user-emacs-file "tmp/custom.el"))


(provide 'early-init)
;;; early-init.el ends here
