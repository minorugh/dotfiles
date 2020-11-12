;;; 07_buffer.el --- buffer control tools  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


(leaf user-buffer-utility
  :init
  (leaf auto-save-buffers-enhanced
	:ensure t
	:config
	(setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "^/scp:" "/sudo:"))
	(setq auto-save-buffers-enhanced-quiet-save-p t)
	(auto-save-buffers-enhanced t))

  (leaf persistent-scratch
	:ensure t
	:init
	(setq persistent-scratch-save-file "~/Dropbox/emacs/persistent-scratch")
	:config
	(persistent-scratch-setup-default))

  (leaf tempbuf
	:el-get (tempbuf.el
			 :url "https://www.emacswiki.org/emacs/download/tempbuf.el")
	:hook ((dired-mode-hook direx:direx-mode magit-mode compilation-mode)
		   . turn-on-tempbuf-mode)
	:config
	(setq tempbuf-kill-message nil))

  (leaf undohist
	:ensure t
	:hook (emacs-startup-hook . undohist-initialize)
	:config
	(setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

  (leaf undo-tree
	:ensure t
	:hook ((prog-mode-hook text-mode-hook) . undo-tree-mode)
	:config
	(bind-key* "C-_" 'undo-tree-undo)
	(bind-key* "C-\\" 'undo-tree-undo)
	(bind-key* "C-/" 'undo-tree-redo)
	(bind-key* "C-x u" 'undo-tree-visualize)
	:init
	(make-variable-buffer-local 'undo-tree-visualizer-diff)
	(setq-default undo-tree-visualizer-diff t)
	(setq undo-tree-visualizer-timestamps t)
	(setq undo-tree-visualizer-diff t)
	(setq undo-tree-enable-undo-in-region nil)
	(setq undo-tree-auto-save-history nil)
	(setq undo-tree-history-directory-alist
		  `(("." . ,(concat user-emacs-directory "undo-tree-hist/")))))

  :config
  (bind-key "s-s" 'toggle-scratch)
  (defun toggle-scratch ()
    "Toggle current buffer and *scratch* buffer."
    (interactive)
    (if (not (string= "*scratch*" (buffer-name)))
  		(progn
  		  (setq toggle-scratch-prev-buffer (buffer-name))
  		  (switch-to-buffer "*scratch*"))
	  (switch-to-buffer toggle-scratch-prev-buffer)))

  (defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (get-buffer "*tramp/scp xsrv*")
      (counsel-tramp-quit))
    (message "Killed Other Buffers!"))
  )


;; (leaf user-buffer-functions
;;   :config
;;   (bind-key "s-s" 'toggle-scratch)
;;   :init
;;   (defun toggle-scratch ()
;;     "Toggle current buffer and *scratch* buffer."
;;     (interactive)
;;     (if (not (string= "*scratch*" (buffer-name)))
;;   		(progn
;;   		  (setq toggle-scratch-prev-buffer (buffer-name))
;;   		  (switch-to-buffer "*scratch*"))
;; 	  (switch-to-buffer toggle-scratch-prev-buffer)))

;;   (defun kill-other-buffers ()
;;     "Kill all other buffers."
;;     (interactive)
;;     (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
;;     (when (get-buffer "*tramp/scp xsrv*")
;;       (counsel-tramp-quit))
;;     (message "Killed Other Buffers!"))

;;   )


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 07_buffer.el ends here
