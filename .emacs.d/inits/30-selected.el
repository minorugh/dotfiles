;;; 20-selected.el --- Region selected configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ======================================================
;;  Top-level definitions
;; ======================================================

(defvar my-selected-mode-map (make-sparse-keymap)
  "Keymap active only while a region is selected.")

(define-minor-mode my-selected-mode
  "Minor mode auto-enabled during region selection."
  :keymap my-selected-mode-map)

(defvar-local my-ime-flag nil
  "Non-nil means IME was active before region activation.")


;; ============================================================
;;  Leaf Configuration
;; ============================================================

(leaf *selected
  :doc "Keymap for when region is active."
  :hook ((post-command-hook    . my-selected-mode-update)
         (activate-mark-hook   . my-selected-ime-off)
         (deactivate-mark-hook . my-selected-ime-restore))
  :init
  ;; Key bindings
  (keymap-set my-selected-mode-map ";" #'comment-dwim)
  (keymap-set my-selected-mode-map "c" #'kill-ring-save)
  (keymap-set my-selected-mode-map "s" #'swiper-region)
  (keymap-set my-selected-mode-map "g" #'my-google-search)
  (keymap-set my-selected-mode-map "w" #'my-weblio-search)
  (keymap-set my-selected-mode-map "d" #'deepl-translate)

  ;; Functions called from the hook
  (defun my-selected-mode-update ()
    "Toggle `my-selected-mode' based on whether a region is active."
    (if (use-region-p)
        (unless my-selected-mode (my-selected-mode 1))
      (when my-selected-mode (my-selected-mode -1))))

   (defun my-selected-ime-off ()
    "Temporarily disable IME when a region becomes active."
    (setq my-ime-flag current-input-method)
    (deactivate-input-method))

  (defun my-selected-ime-restore ()
    "Restore IME state after the region is deactivated."
    (when my-ime-flag
      (toggle-input-method)
      (setq my-ime-flag nil)))

  :preface
  ;; Set of independent search commands
  (defun my-get-region-or-word ()
    "Return active region text, or word at point."
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'word t)))

  (defun my-google-search (str)
    "Google STR."
    (interactive (list (my-get-region-or-word)))
    (browse-url (format "https://google.com/search?q=%s" (url-hexify-string str))))

  (defun my-weblio-search (str)
    "Weblio search for STR."
    (interactive (list (my-get-region-or-word)))
    (browse-url (format "https://www.weblio.jp/content/%s" (url-hexify-string str)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 20-selected.el ends here
