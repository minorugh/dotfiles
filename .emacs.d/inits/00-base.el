;;; 00-base.el --- Better default configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Performance
;; ============================================================

;; Faster rendering: disable right-to-left language support
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)


;; ============================================================
;;  File / Backup / Lock
;; ============================================================

(setq make-backup-files        nil)   ; no *.~ backup files
(setq auto-save-default        nil)   ; no auto-save
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles         nil)   ; no .#lockfiles
(setq vc-follow-symlinks       t)     ; follow symlinks without asking
(setq require-final-newline    t)     ; always end file with newline


;; ============================================================
;;  Temp / History File Locations  (~/.emacs.d/tmp/)
;; ============================================================

(setq auto-save-list-file-prefix  (locate-user-emacs-file "tmp/auto-save-list/.saves-"))
(setq tramp-persistency-file-name (locate-user-emacs-file "tmp/tramp"))
(setq request-storage-directory   (locate-user-emacs-file "tmp/request"))
(setq url-configuration-directory (locate-user-emacs-file "tmp/url"))
(setq bookmark-default-file       (locate-user-emacs-file "tmp/bookmarks"))
(setq save-place-file             (locate-user-emacs-file "tmp/places"))
(setq project-list-file           (locate-user-emacs-file "tmp/projects"))


;; ============================================================
;;  Editing Defaults
;; ============================================================

(setq completion-ignore-case              t)   ; case-insensitive completion
(setq read-file-name-completion-ignore-case t)
(setq scroll-preserve-screen-position    t)    ; point stays on scroll
(setq ring-bell-function                'ignore) ; no bell
(setq visible-bell                       nil)
(setq mouse-drag-copy-region             t)    ; mouse selection copies
(setq select-enable-clipboard            t)    ; use X11 clipboard
(setq uniquify-buffer-name-style        'post-forward-angle-brackets)
(setq byte-compile-docstring-max-column 120)
(setq-default cursor-in-non-selected-windows nil)
(set-fringe-mode 1)

;; Shorter aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'exit-emacs  'save-buffers-kill-emacs)


;; ============================================================
;;  Trash
;; ============================================================

(setq delete-by-moving-to-trash t)
(setq trash-directory (locate-user-emacs-file "tmp/trash"))

;; Auto-create trash directory if missing
(advice-add 'move-file-to-trash :before
            (lambda (&rest _)
              (unless (file-exists-p trash-directory)
                (make-directory trash-directory t))))


;; ============================================================
;;  Savehist / Recentf / Save-place
;; ============================================================

(add-hook 'after-init-hook #'savehist-mode)
(setq savehist-file             (locate-user-emacs-file "tmp/savehist"))
(setq history-length             200)
(setq history-delete-duplicates  t)
(setq savehist-additional-variables '(extended-command-history my-describe-history))

(run-with-idle-timer 0.5 nil #'recentf-mode)
(setq recentf-max-saved-items 100)
(setq recentf-auto-cleanup    'never)
(setq recentf-save-file       (locate-user-emacs-file "tmp/recentf"))
(setq recentf-exclude
      (list (expand-file-name "elpa/" user-emacs-directory)
            (expand-file-name "tmp/"  user-emacs-directory)
            "\\.howm-keys" "/session\\." "task.org" "/Dropbox/backup/" "/scp:" "neomutt-"))

(add-hook 'after-init-hook #'save-place-mode)


;; ============================================================
;;  Mode Associations / Global Minor Modes
;; ============================================================

(add-hook 'after-init-hook #'global-auto-revert-mode)

(dolist (pair '(("\\.\\(?:tmux\\.conf\\|muttrc\\|xprofile\\|Xmodmap\\)\\'" . conf-mode)
                ("\\.\\(?:gitattributes\\|gitignore\\|vimrc\\)\\'"         . conf-mode)
                ("/crontab\\(\\..*\\)?\\'" . conf-mode)
                ("\\.cgi\\'"               . perl-mode)
                ("/passwd/.*\\.cgi\\'"     . text-mode)))
  (add-to-list 'auto-mode-alist pair))

(defun my-view-mode-maybe ()
  "*.log ファイルなら `view-mode' にする."
  (when (and buffer-file-name
             (string-match-p "\\.log\\'" buffer-file-name))
    (when (fboundp 'evil-emacs-state)
      (evil-emacs-state))
    (view-mode 1)))

(defun my-read-only-maybe ()
  "*.dat ファイルなら read-only にする."
  (when (and buffer-file-name
             (string-match-p "\\.dat\\'" buffer-file-name))
    (read-only-mode 1)))

(add-hook 'find-file-hook #'my-view-mode-maybe)
(add-hook 'find-file-hook #'my-read-only-maybe)


;; ============================================================
;;  Keybindings & User Commands
;; ============================================================

(defun my-clipboard-kill-region ()
  "Kill region to clipboard, or `backward-kill-word' if no region."
  (interactive)
  (if (use-region-p)
      (clipboard-kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun other-window-or-split ()
  "Split window horizontally if only one window; otherwise go to next window."
  (interactive)
  (when (window-live-p (frame-root-window))
    (split-window-horizontally))
  (other-window 1))

(defun handle-delete-frame (event)
  "Override `handle-delete-frame': minimize last EVENT frame instead of deleting."
  (interactive "e")
  (let ((frame  (posn-window (event-start event)))
        (numfrs (length (visible-frame-list))))
    (cond ((> numfrs 1) (delete-frame frame t))
          ((iconify-frame)))))

(keymap-global-set "C-x b"      #'ibuffer)
(keymap-global-set "C-x m"      #'counsel-imenu)
(keymap-global-set "M-,"        #'xref-find-definitions)
(keymap-global-set "M-w"        #'clipboard-kill-ring-save)
(keymap-global-set "C-w"        #'my-clipboard-kill-region)
(keymap-global-set "M-/"        #'kill-current-buffer)
(keymap-global-set "s-c"        #'clipboard-kill-ring-save)
(keymap-global-set "s-v"        #'clipboard-yank)
(keymap-global-set "C-q"        #'other-window-or-split)
(keymap-global-set "C-<tab>"    #'quoted-insert)
(keymap-global-set "S-<return>" (lambda () (interactive) (end-of-line) (newline)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 00-base.el ends here
