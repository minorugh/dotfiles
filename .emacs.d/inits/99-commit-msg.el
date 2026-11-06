;;; my-commit-msg.el --- Commit message buffer (trial) -*- lexical-binding: t -*-
;;; Commentary:
;;
;; 試用版。M-x load-file で読み込み、/tmp のテスト用リポジトリで試す。
;;
;;   M-x my-commit-this-file   開いているファイル（dired なら行のファイル）だけ commit
;;   M-x my-commit-all         すべての変更を commit
;;
;; メッセージバッファ内:
;;   C-c C-c   確定して commit
;;   C-c C-k   キャンセル
;;   空のまま確定すると "auto: 日時" の仮メッセージになる。
;;   # で始まる行はメッセージに含まれない。
;;
;;; Code:

(require 'seq)

(defvar my-commit-msg-buffer "*commit-msg*"
  "Name of the commit message buffer.")

(defvar-local my-commit-msg--root nil)
(defvar-local my-commit-msg--file nil)
(defvar-local my-commit-msg--wconf nil)

(define-derived-mode my-commit-msg-mode text-mode "CommitMsg"
  "Major mode for writing a commit message."
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1)))

(define-key my-commit-msg-mode-map (kbd "C-c C-c") #'my-commit-msg-finish)
(define-key my-commit-msg-mode-map (kbd "C-c C-k") #'my-commit-msg-cancel)

(defun my-commit-msg--git (root &rest args)
  "Run git in ROOT with ARGS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil t nil "-C" root args)))
      (cons status (string-trim (buffer-string))))))

(defun my-commit-msg--text ()
  "Return the message in the current buffer without # lines."
  (let ((lines (seq-remove (lambda (l) (string-prefix-p "#" l))
                           (split-string (buffer-string) "\n"))))
    (string-trim (mapconcat #'identity lines "\n"))))

(defun my-commit-msg--open (root file)
  "Open the message buffer for ROOT. FILE nil means all changes."
  (let* ((args (and file (list "--" file)))
         (changes (cdr (apply #'my-commit-msg--git root "status" "--short" args))))
    (if (string-empty-p changes)
        (message "commit: 変更なし")
      (let ((wconf (current-window-configuration)))
        (pop-to-buffer (get-buffer-create my-commit-msg-buffer))
        (erase-buffer)
        (my-commit-msg-mode)
        (setq my-commit-msg--root root
              my-commit-msg--file file
              my-commit-msg--wconf wconf)
        (insert "\n# C-c C-c: commit   C-c C-k: キャンセル   空のまま確定 = auto:\n"
                "# 対象: " (or file "すべての変更") "\n#\n")
        (dolist (line (split-string changes "\n"))
          (insert "# " line "\n"))
        (goto-char (point-min))))))

(defun my-commit-msg--target-path ()
  "Return the true path of the file in this buffer or dired line."
  (let ((path (cond ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
                    (buffer-file-name buffer-file-name))))
    (unless path (user-error "ファイルバッファまたは dired で実行してください"))
    (file-truename path)))

(defun my-commit-msg--root (path)
  "Return the git root of PATH or signal an error."
  (let ((root (locate-dominating-file path ".git")))
    (unless root (user-error "git 管理下のファイルではありません"))
    (expand-file-name root)))

;;;###autoload
(defun my-commit-this-file ()
  "Commit only the file of the current buffer."
  (interactive)
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let* ((file (my-commit-msg--target-path))
         (root (my-commit-msg--root file)))
    (my-commit-msg--open root file)))

;;;###autoload
(defun my-commit-all ()
  "Commit all changes in the repository of the current buffer."
  (interactive)
  (let ((root (my-commit-msg--root (my-commit-msg--target-path))))
    (my-commit-msg--open root nil)))

(defun my-commit-msg-finish ()
  "Commit with the message in this buffer."
  (interactive)
  (let* ((root my-commit-msg--root)
         (file my-commit-msg--file)
         (wconf my-commit-msg--wconf)
         (text (my-commit-msg--text))
         (msg (if (string-empty-p text)
                  (format-time-string "auto: %Y-%m-%d %H:%M:%S")
                text))
         (res (if file
                  (progn
                    (my-commit-msg--git root "add" "--" file)
                    (my-commit-msg--git root "commit" "-m" msg "--" file))
                (my-commit-msg--git root "add" "-A")
                (my-commit-msg--git root "commit" "-m" msg))))
    (kill-buffer (current-buffer))
    (set-window-configuration wconf)
    (message "%s%s" (if (zerop (car res)) "" "commit 失敗: ") (cdr res))))

(defun my-commit-msg-cancel ()
  "Cancel the commit."
  (interactive)
  (let ((wconf my-commit-msg--wconf))
    (kill-buffer (current-buffer))
    (set-window-configuration wconf)
    (message "commit: キャンセル")))

(provide '99-commit-msg)
;;; 99-commit-msg.el ends here
