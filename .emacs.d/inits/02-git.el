;;; 02-git.el --- Git commit workflow: message buffer, reword, hydra-git. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; commit と push を分ける運用のための設定。
;;
;;   my-commit-this-file   開いているファイル（dired なら行のファイル）だけ commit
;;   my-commit-all         すべての変更を commit（dotfiles では先に make prep）
;;   my-commit-reword      直前のコミットのメッセージだけ書き換える（push 済みは不可）
;;
;; メッセージバッファ内:
;;   C-c C-c   確定
;;   C-c C-k   キャンセル
;;   空のまま確定すると "auto: 日時" の仮メッセージになる（reword は不可）。
;;   # で始まる行はメッセージに含まれない。
;;
;;   my-make-git          dotfiles/git の make git を実行（hydra-dired の ]）
;;                        メイン機は push のみ、サブ機は端末で pull する。
;;
;;; Code:

(require 'seq)

(defvar my-main-machine-p)
(defvar tig-context-file)

(defvar my-commit-dotfiles-dir "~/src/github.com/minorugh/dotfiles/"
  "Dotfiles repository. `make prep' runs only for this repository.")

(defvar my-commit-msg-buffer "*commit-msg*"
  "Name of the commit message buffer.")

(defvar-local my-commit-msg--root nil)
(defvar-local my-commit-msg--file nil)
(defvar-local my-commit-msg--wconf nil)
(defvar-local my-commit-msg--amend nil)

;; ============================================================
;;  Message buffer
;; ============================================================
(define-derived-mode my-commit-msg-mode text-mode "CommitMsg"
  "Major mode for writing a commit message."
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1)))

(define-key my-commit-msg-mode-map (kbd "C-c C-c") #'my-commit-msg-finish)
(define-key my-commit-msg-mode-map (kbd "C-c C-k") #'my-commit-msg-cancel)

(defun my-commit-msg--run (program &rest args)
  "Run PROGRAM with ARGS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (cons status (string-trim (buffer-string))))))

(defun my-commit-msg--git (root &rest args)
  "Run git in ROOT with ARGS and return (STATUS . OUTPUT)."
  (apply #'my-commit-msg--run "git" "-C" root args))

(defun my-commit-msg--text ()
  "Return the message in the current buffer without # lines."
  (let ((lines (seq-remove (lambda (l) (string-prefix-p "#" l))
                           (split-string (buffer-string) "\n"))))
    (string-trim (mapconcat #'identity lines "\n"))))

(defun my-commit-msg--open (root file &optional amend)
  "Open the message buffer for ROOT.
FILE nil means all changes.  AMEND non-nil rewords the last commit."
  (let* ((changes (if amend
                      (cdr (my-commit-msg--git root "show" "--stat" "--format=" "HEAD"))
                    (cdr (apply #'my-commit-msg--git root "status" "--short"
                                (and file (list "--" file)))))))
    (if (string-empty-p changes)
        (message "commit: 変更なし")
      (let ((wconf (current-window-configuration))
            (old (and amend
                      (cdr (my-commit-msg--git root "log" "-1" "--format=%B")))))
        (pop-to-buffer (get-buffer-create my-commit-msg-buffer))
        (erase-buffer)
        (my-commit-msg-mode)
        (setq my-commit-msg--root root
              my-commit-msg--file file
              my-commit-msg--wconf wconf
              my-commit-msg--amend amend)
        (insert (if old (concat old "\n") "")
                "\n"
                (if amend
                    "# 直前のコミットのメッセージを書き換えます（push 済みは不可）\n"
                  (concat "# 対象: " (or file "すべての変更") "\n"))
                "# C-c C-c: 確定   C-c C-k: キャンセル"
                (if amend "" "   空のまま確定 = auto:")
                "\n#\n")
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

(defun my-commit-msg--dotfiles-p (root)
  "Return non-nil if ROOT is the dotfiles repository."
  (file-equal-p root (expand-file-name my-commit-dotfiles-dir)))

(defun my-commit-msg--check-machine (root)
  "Refuse to commit dotfiles on a sub machine (ROOT is the repository)."
  (when (and (my-commit-msg--dotfiles-p root)
             (not my-main-machine-p))
    (user-error "サブ機の dotfiles からは commit しません")))

(defun my-commit-msg--prep (root)
  "Run `make prep' when ROOT is the dotfiles repository."
  (when (my-commit-msg--dotfiles-p root)
    (let ((res (my-commit-msg--run "make" "-s" "-C"
                                   (expand-file-name "git" root) "prep")))
      (unless (zerop (car res))
        (user-error "make prep 失敗: %s" (cdr res))))))

(defun my-commit-msg--close ()
  "Kill the message buffer and restore the window configuration."
  (let ((wconf my-commit-msg--wconf))
    (kill-buffer (current-buffer))
    (set-window-configuration wconf)))

(defun my-commit-msg-finish ()
  "Commit (or reword) with the message in this buffer."
  (interactive)
  (let* ((root my-commit-msg--root)
         (file my-commit-msg--file)
         (amend my-commit-msg--amend)
         (text (my-commit-msg--text)))
    (when (and amend (string-empty-p text))
      (user-error "メッセージが空です"))
    (let* ((msg (if (string-empty-p text)
                    (format-time-string "auto: %Y-%m-%d %H:%M:%S")
                  text))
           (res (cond
                 (amend
                  (my-commit-msg--git root "commit" "--amend" "--only" "-m" msg))
                 (file
                  (my-commit-msg--git root "add" "--" file)
                  (my-commit-msg--git root "commit" "-m" msg "--" file))
                 (t
                  (my-commit-msg--git root "add" "-A")
                  (my-commit-msg--git root "commit" "-m" msg)))))
      (my-commit-msg--close)
      (message "%s%s" (if (zerop (car res)) "" "commit 失敗: ") (cdr res)))))

(defun my-commit-msg-cancel ()
  "Cancel the commit."
  (interactive)
  (my-commit-msg--close)
  (message "commit: キャンセル"))

;; ============================================================
;;  Commands
;; ============================================================
(defun my-commit-this-file ()
  "Commit only the file of the current buffer."
  (interactive)
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let* ((file (my-commit-msg--target-path))
         (root (my-commit-msg--root file)))
    (my-commit-msg--check-machine root)
    (my-commit-msg--open root file)))

(defun my-commit-all ()
  "Commit all changes in the repository of the current buffer."
  (interactive)
  (let ((root (my-commit-msg--root (my-commit-msg--target-path))))
    (my-commit-msg--check-machine root)
    (my-commit-msg--prep root)
    (my-commit-msg--open root nil)))

(defun my-commit-reword ()
  "Reword the last commit if it has not been pushed."
  (interactive)
  (let ((root (my-commit-msg--root (my-commit-msg--target-path))))
    (when (zerop (car (my-commit-msg--git root "merge-base" "--is-ancestor"
                                          "HEAD" "@{upstream}")))
      (user-error "直前のコミットは push 済みです。書き換えません"))
    (my-commit-msg--open root nil t)))

;; ============================================================
;;  Push
;; ============================================================
(declare-function my-make-run-async "09-makefile")

(defun my-make-git ()
  "Run `make git' in dotfiles/git.
The main machine pushes in a compile buffer.  A sub machine pulls in a
terminal because env-sync asks for confirmation."
  (interactive)
  (let ((dir (file-name-as-directory
              (expand-file-name "git" my-commit-dotfiles-dir))))
    (if my-main-machine-p
        (let ((default-directory dir))
          (compile "make git"))
      (my-make-run-async dir "git"))))

;; ============================================================
;;  tig launchers
;; ============================================================
(defun my-git--tig (dir &rest args)
  "Run tig with ARGS in gnome-terminal at DIR."
  (start-process "tig" nil "gnome-terminal" "--maximize"
                 "--working-directory" dir
                 "--" "bash" "-c"
                 (format "tig %s; exec bash"
                         (mapconcat #'shell-quote-argument args " "))))

(defun my-git-status ()
  "Open `tig status' for the current repository."
  (interactive)
  (my-git--tig (my-commit-msg--root (my-commit-msg--target-path)) "status"))

(defun my-git-unpushed ()
  "Open tig on the commits not pushed yet."
  (interactive)
  (my-git--tig (my-commit-msg--root (my-commit-msg--target-path))
               "origin/main..HEAD"))

(defun my-git-blame ()
  "Open `tig blame' for the current file."
  (interactive)
  (let* ((file (my-commit-msg--target-path))
         (root (my-commit-msg--root file)))
    (when (bound-and-true-p tig-context-file)
      (write-region file nil tig-context-file nil 'silent))
    (my-git--tig root "blame" (file-relative-name file root))))

;; ============================================================
;;  Hydra Git
;; ============================================================
(leaf *hydra-git
  :after evil
  :hydra
  (hydra-git
   (:hint nil :exit t)
   "
 Git.menu
  _a_ll.commit  _r_eword  _u_npushed  _s_tatus  _b_lame
"
   ("a" my-commit-all)
   ("r" my-commit-reword)
   ("u" my-git-unpushed)
   ("s" my-git-status)
   ("b" my-git-blame)
   ("q" top-level)
   ("<muhenkan>" nil)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 02-git.el ends here
