;;; 20_custom.el --- User custom functions. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *user-custom-functions
  :bind	 (("<f3>" . thunar-open)
		  ("<f4>" . terminal-open)
		  ("<f5>" . ssh-vim)
		  ("<f8>" . toggle-menu-bar-mode-from-frame)
		  ("S-<delete>" . my:delete-this-file)
		  ("C-c h" . chromium-tegaki)
		  ("<muhenkan>" . my:muhenkan)
		  ("C-c <left>" . winner-undo)
		  ("C-c <right>" . winner-redo)
		  ("<zenkaku-hankaku>" . toggle-evil-mode))
  :init
  (defun toggle-evil-mode ()
	"Toggle on and off evil mode."
	(interactive)
	(if evil-mode (evil-mode 0)
	  (evil-mode 1)))

  (defun my:muhenkan ()
	(interactive)
	(if (not (use-region-p))
		(minibuffer-keyboard-quit)
	  (keyboard-quit)))

  (defun ssh-vim ()
	"Open thunar with current dir."
	(interactive)
	(shell-command "gnome-terminal -- ssh xsrv"))

  (defun thunar-open ()
	"Open thunar with current dir."
	(interactive)
	(shell-command (concat "xdg-open " default-directory)))

  (defun terminal-open ()
	"Open termninal with current dir."
	(interactive)
	(let ((dir (directory-file-name default-directory)))
	  (when (and (eq system-type 'gnu/linux)
				 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
		(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
	  (shell-command (concat "gnome-terminal --working-directory " dir))))

  (defun my:delete-this-file ()
	"Delete the current file, and kill the buffer."
	(interactive)
	(unless (buffer-file-name)
	  (error "No file is currently being edited"))
	(when (yes-or-no-p (format "Really delete '%s'?"
							   (file-name-nondirectory buffer-file-name)))
	  (delete-file (buffer-file-name))
	  (kill-this-buffer)))

  (defun chromium-tegaki ()
	"Chromium tegaki site."
	(interactive)
	(browse-url "https://mojinavi.com/tegaki"))

  ;; Automatically open root permission file with sudo
  (leaf *sudo-open
	:doc "https://ameblo.jp/grennarthmurmand1976/entry-12151018656.html"
	:config
	(defun file-root-p (filename)
	  "Return t if file FILENAME created by root."
	  (eq 0 (nth 2 (file-attributes filename))))

	(defadvice find-file (around my:find-file activate)
	  "Open FILENAME using tramp's sudo method if it's root permission."
	  (if (and (file-root-p (ad-get-arg 0))
			   (not (file-writable-p (ad-get-arg 0)))
			   (y-or-n-p (concat (ad-get-arg 0)
								 " is root permission. Open it as root? ")))
		  (my:find-file-sudo (ad-get-arg 0))
		ad-do-it))

	(defun my:find-file-sudo (file)
	  "Opens FILE with root privileges."
	  (interactive "F")
	  (set-buffer (find-file (concat "/sudo::" file)))))


  ;; PS-printer
  (defalias 'ps-mule-header-string-charsets 'ignore)
  (setq ps-multibyte-buffer 'non-latin-printer
		ps-paper-type 'a4
		ps-font-size 9
		;; ps-font-family 'Helvetica
		ps-font-family 'Courier
		ps-line-number-font 'Courier
		ps-printer-name nil
		ps-print-header nil
		ps-show-n-of-n t
		ps-line-number t
		ps-print-footer nil))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_custom.el ends here
