;;; 10-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-

;;; Commentary:

;; ファンクションキーおよびコマンドから外部ツールを起動するユーザー関数を定義する。
;;
;; 主な機能:
;;   - F3: gnome-terminal を現在の作業ディレクトリで開く
;;   - F4: SSH で gospel-haiku.com サーバーに接続する
;;   - F6: Thunar ファイルマネージャーを現在のディレクトリで開く
;;   - Keychain の SSH 環境変数を Emacs セッションに読み込む (起動時自動実行)
;;   - FileZilla で各リモートサイトへ接続する
;;   - KeePassXC をスクリプト経由で起動する

;;; Code:
;; (setq debug-on-error t)

(leaf my-user-functions
  :doc "User functions to launch external tools, SSH, and file managers."
  :bind (("<f3>" . terminal-open)
	 ("<f4>" . xsrv-gh)
	 ("<f6>" . thunar-open))
  :init
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

  (defun thunderbird ()
    "Open thunderbird mail-client for Gmail."
    (interactive)
    (start-process "thunderbird" nil "thunderbird"))

  (defun neomutt ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (start-process-shell-command "neomutt" nil "neomutt.sh"))
  (setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

  (defun mattermost ()
    "Open mattermost-desktop."
    (interactive)
    (start-process "mattermost" nil "mattermost-desktop"))

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
