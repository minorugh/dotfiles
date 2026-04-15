;;; 10-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf my-user-functions
  :doc "User functions to launch external tools, SSH, and file managers."
  :bind (("<f3>" . terminal-open)
	 ("<f4>" . xsrv-gh)
	 ("<f6>" . thunar-open))
  :init
  (defun my-sudo-reopen ()
    "Reopen current file with sudo privileges via TRAMP."
    (interactive)
    (let ((pos (point)))
      (find-alternate-file (concat "/sudo:localhost:" (buffer-file-name)))
      (goto-char pos)))

  (defun terminal-open ()
    "Open gnome-terminal at current dir on adjacent display."
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (start-process-shell-command
       "gnome-terminal" nil
       (concat "gnome-terminal --working-directory " dir))
      (run-with-timer
       0.5 nil
       (lambda ()
	 (shell-command
          "xdotool search --sync --onlyvisible --class gnome-terminal windowmove 0 0")))))

  (defun thunar-open ()
    "Open Thunar at current dir on adjacent display."
    (interactive)
    (start-process-shell-command
     "thunar" nil
     (concat "thunar " default-directory))
    (run-with-timer
     0.5 nil
     (lambda ()
       (shell-command
	"xdotool search --sync --onlyvisible --class thunar windowmove 0 0"))))

  (defun xsrv-gh ()
    "Open the xserver gospel-haiku.com in a terminal."
    (interactive)
    (start-process-shell-command "xsrv-gh" nil "gnome-terminal --maximize -- ssh xsrv-GH"))

  (defun my-reload-keychain ()
    "Reload keychain environment variables in Emacs session for SSH."
    (interactive)
    ;; keychain が書いた SSH_AUTH_SOCK と SSH_AGENT_PID を Emacs 内に設定
    (let ((keychain-file (expand-file-name (concat "~/.keychain/" (system-name) "-sh"))))
      (when (file-exists-p keychain-file)
	(with-temp-buffer
          (insert-file-contents keychain-file)
          ;; export 文を eval して Emacs 内に反映
          (goto-char (point-min))
          (while (re-search-forward "^export \\([^=]+\\)=\\(.*\\)$" nil t)
            (setenv (match-string 1) (replace-regexp-in-string "^\"\\|\"$" "" (match-string 2))))))
      (message "Keychain reloaded in Emacs!")))
  (add-hook 'after-init-hook #'my-reload-keychain)

  (defun fzilla-GH ()
    "Open Filezilla with `gospel-haiku.com'."
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/gospel-haiku.com'"))

  (defun fzilla-minoruGH ()
    "Open Filezilla with `minorugh.com'."
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/minorugh.com'"))

  (defun fzilla-s ()
    "Open Filezilla with list of connections."
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla -s"))

  (defun keepassxc ()
    "Open keepassxc with auto passwd input."
    (interactive)
    (start-process-shell-command "keepass" nil "keepass.sh")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-funcs.el ends here
