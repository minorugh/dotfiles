;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;; Commentary:
;; X11 display settings (font scale, colors) are configured in ~/.Xresources.
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Primary Machine Detection
;; ============================================================
;; Set the hostname of the primary machine here.
;; If you need to switch machines in the future, you only need to change this one line.
;; In subsequent configuration files, refer to `my-main-machine-p` instead of `system-name`.

(defconst my-main-hostname "P1")
(defconst my-main-machine-p (string= (system-name) my-main-hostname))


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

;; "P1" is the hostname of the main machine.
(let ((font-size (if my-main-machine-p 18 16)))
  (push `(font . ,(format "Cica-%d" font-size)) default-frame-alist))

(setq inhibit-compacting-font-caches t)


;; ============================================================
;;  UI — Disable Early to Prevent Flicker
;; ============================================================

(push '(menu-bar-lines     . 0) default-frame-alist)
(push '(tool-bar-lines     . 0) default-frame-alist)
(push '(vertical-scroll-bars  ) default-frame-alist)
(push '(undecorated        . t) default-frame-alist)

;; Launch maximized on the monitor to the right.
;; (A negative "left" value is measured from the right edge.)
(push '(left       .        -1) initial-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)


;; ============================================================
;;  Splash Screen
;; ============================================================

(setq inhibit-startup-message t)
(setq inhibit-startup-screen  t)

;; Leave the initial buffer selection to dashboard.
(setq initial-buffer-choice nil)

;; Prevent Customize from writing directly to init.el
(setq custom-file (locate-user-emacs-file "tmp/custom.el"))


(provide 'early-init)
;;; early-init.el ends here
