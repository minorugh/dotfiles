;;; 02-git.el --- Git commit workflow: message buffer, reword, hydra-git. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; commit と push を分ける運用のための設定。
;;
;;   my-commit-this-file   開いているファイル（Dired なら行のファイル）だけ commit
;;   my-commit-all         すべての変更を commit（dotfiles では先に make prep）
;;   my-commit-reword      直前のコミットのメッセージだけ書き換える（push 済みは不可）
;;
;; メッセージバッファ内:
;;   C-c C-c   確定
;;   C-c C-k   キャンセル
;;   空のまま確定すると "auto: 日時" の仮メッセージになる（reword は不可）。
;;   メッセージは上に書く。下の情報ボックスは read-only。
;;
;;   my-make-git          dotfiles/git の make git を実行（hydra-dired の ]）
;;                        メイン機は push のみ、サブ機は端末で pull する。
;;
;;   open-tig-project     リポジトリ全体の履歴を tig で開く（Dired の ]）
;;
;;; Code:

(require 'seq)

(defvar my-main-machine-p)
(defvar tig-context-file)
(declare-function my-modeline-apply-highlight-now "30-ui")

(defvar my-commit-dotfiles-dir "~/src/github.com/minorugh/dotfiles/"
  "Dotfiles repository `make prep' runs only for this repository.")

(defvar my-commit-msg-buffer "*commit-msg*"
  "Name of the commit message buffer.")

(defvar-local my-commit-msg--root nil)
(defvar-local my-commit-msg--file nil)
(defvar-local my-commit-msg--wconf nil)
(defvar-local my-commit-msg--amend nil)
(defvar-local my-commit-msg--end nil)

(defface my-commit-msg-box
  '((((background dark)) :background "#2b3452" :extend t)
    (t :background "#e8ecf5" :extend t))
  "Background of the commit info box.")

(defface my-commit-msg-title
  '((t :inherit font-lock-keyword-face :weight bold))
  "Title and labels of the commit info box.")

;; ============================================================
;;  Message buffer
;; ============================================================
(define-derived-mode my-commit-msg-mode text-mode "CommitMsg"
  "Major mode for writing a commit message.")

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

(defun my-commit-msg--status-face (st)
  "Return a face for git status string ST."
  (cond ((string-match-p "D" st) '(:foreground "#f0626a" :weight bold))
        ((string-match-p "M" st) '(:foreground "#e5a64a" :weight bold))
        (t '(:foreground "#7fd18b" :weight bold))))

(defun my-commit-msg--header (target summary amend)
  "Return the read-only info box string for TARGET and SUMMARY.
AMEND non-nil means the box is for rewording."
  (let* ((align (propertize " " 'display '(space :align-to 16)))
         (str (concat
               "\n"
               (propertize "  【 " 'face 'my-commit-msg-title)
               (propertize (if amend "コミット書き換え" "コミット情報")
                           'face 'my-commit-msg-title)
               (propertize " 】\n" 'face 'my-commit-msg-title)
               "\n"
               "  " (propertize "対象" 'face 'my-commit-msg-title) align ": " target "\n"
               "  " (propertize (if amend "直前の変更" "変更ファイル")
                                'face 'my-commit-msg-title)
               align ": "
               (mapconcat
                (lambda (line)
                  (if (or amend (< (length line) 4))
                      line
                    (concat (propertize (string-trim (substring line 0 2))
                                        'face (my-commit-msg--status-face
                                               (substring line 0 2)))
                            "  " (substring line 3))))
                (split-string summary "\n")
                (concat "\n" align "  "))
               "\n\n  "
               (make-string 44 ?─)
               "\n  "
               (propertize "C-c C-c" 'face 'my-commit-msg-title) ": 確定  "
               (propertize "C-c C-k" 'face 'my-commit-msg-title) ": キャンセル"
               (if amend "  (push 済みは不可)" "  (空のまま確定 = auto: timestamp)")
               "\n\n")))
    (add-face-text-property 0 (length str) 'my-commit-msg-box t str)
    (add-text-properties 0 (length str) '(read-only t rear-nonsticky t) str)
    str))

(defun my-commit-msg--text ()
  "Return the message typed above the info box."
  (string-trim (buffer-substring-no-properties (point-min) my-commit-msg--end)))

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
        (let ((inhibit-read-only t))
          (erase-buffer))
        (my-commit-msg-mode)
        (setq my-commit-msg--root root
              my-commit-msg--file file
              my-commit-msg--wconf wconf
              my-commit-msg--amend amend)
        (insert (propertize "\n\n" 'read-only t 'rear-nonsticky t)
                (my-commit-msg--header
                 (if file (file-name-nondirectory file) "すべての変更")
                 changes amend))
        (setq my-commit-msg--end (copy-marker (point-min) t))
        (goto-char (point-min))
        (insert (or old ""))))))

(defun my-commit-msg--target-path ()
  "Return the true path of the file in this buffer or Dired line."
  (let ((path (cond ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
                    (buffer-file-name buffer-file-name))))
    (unless path (user-error "ファイルバッファまたは Dired で実行してください"))
    (file-truename path)))

(defun my-commit-msg--root (path)
  "Return the git root of PATH or signal an error."
  (let ((root (locate-dominating-file path ".git")))
    (unless root (user-error "Git 管理下のファイルではありません"))
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
        (user-error "Make prep 失敗: %s" (cdr res))))))

(defun my-commit-msg--close ()
  "Kill the message buffer and restore the window configuration."
  (let ((wconf my-commit-msg--wconf))
    (kill-buffer (current-buffer))
    (set-window-configuration wconf)
    (my-modeline-apply-highlight-now)))

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

(defun my-commit-file-from-tig (file)
  "Open the commit message buffer for FILE.  Called from tig via emacsclient."
  (let* ((path (file-truename file))
         (root (my-commit-msg--root path))
         (buf (find-buffer-visiting path)))
    (when (and buf (buffer-modified-p buf))
      (with-current-buffer buf (save-buffer)))
    (my-commit-msg--check-machine root)
    (my-commit-msg--open root path)
    (select-frame-set-input-focus (selected-frame))))

(defun my-commit-all-from-tig (root)
  "Open the commit message buffer for all Change in ROOT.
Called from tig via emacsclient."
  (my-commit-msg--check-machine root)
  (my-commit-msg--prep root)
  (my-commit-msg--open root nil)
  (select-frame-set-input-focus (selected-frame)))

(defun my-commit-all ()
  "Commit all change in the repository of the current buffer."
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
(declare-function my-make-run-async "10-makefile")

(defun my-make-git ()
  "Run `make git' in the current directory.
In the dotfiles repository, uncommitted changes open `tig status' instead.
The main machine pushes in a compile buffer.  A sub machine pulls in a
terminal because env-sync asks for confirmation."
  (interactive)
  (if my-main-machine-p
      (let* ((dir (file-truename default-directory))
             (root (locate-dominating-file dir ".git")))
        (if (and root
                 (my-commit-msg--dotfiles-p (expand-file-name root))
                 (not (string-empty-p
                       (cdr (my-commit-msg--git root "status" "--porcelain")))))
            (my-git--tig (expand-file-name root) "status")
          (compile "make git")))
    (my-make-run-async default-directory "git")))

;; (defun my-make-git ()
;;   "Run `make git' in the current directory.
;; The main machine pushes in a compile buffer.  A sub machine pulls in a
;; terminal because env-sync asks for confirmation."
;;   (interactive)
;;   (if my-main-machine-p
;;       (compile "make git")
;;     (my-make-run-async default-directory "git")))

;; ============================================================
;;  tig launchers
;; ============================================================
(defun my-git--terminal (dir command)
  "Run shell COMMAND in gnome-terminal at DIR and close terminal on exit."
  (start-process "tig" nil "gnome-terminal"
                 "--geometry" (let ((wa (frame-monitor-workarea)))
                                (format "80x24+%d+%d" (nth 0 wa) (nth 1 wa)))
                 "--maximize"
                 "--title" (format "tig: %s"
                                   (file-name-nondirectory
                                    (directory-file-name dir)))
                 "--working-directory" dir
                 "--" "bash" "-c" command))

(defun my-git--tig (dir &rest args)
  "Run tig with ARGS in gnome-terminal at DIR."
  (my-git--terminal dir (format "tig %s"
                                (mapconcat #'shell-quote-argument args " "))))

(defun my-git-status ()
  "Open `tig status' for the current repository."
  (interactive)
  (my-git--tig (my-commit-msg--root (my-commit-msg--target-path)) "status"))

(defun my-git-unpushed ()
  "Open tig on the commits not pushed yet."
  (interactive)
  (let ((root (my-commit-msg--root (my-commit-msg--target-path))))
    (my-git--terminal
     root
     (if (string= (cdr (my-commit-msg--git root "rev-list" "--count"
                                           "origin/main..HEAD"))
                  "0")
         "echo '未 push のコミットはありません' | tig"
       "tig origin/main..HEAD"))))

(defun my-git-blame ()
  "Open `tig blame' for the current file."
  (interactive)
  (let* ((file (my-commit-msg--target-path))
         (root (my-commit-msg--root file)))
    (when (bound-and-true-p tig-context-file)
      (write-region file nil tig-context-file nil 'silent))
    (my-git--tig root "blame" (file-relative-name file root))))

;;;###autoload
(defun open-tig-project ()
  "Open `tig' for the current directory within its Git repository."
  (interactive)
  (let* ((dir default-directory)
         (root (my-commit-msg--root dir))
         (path (file-relative-name dir root)))
    (my-git--tig root "--" path)))

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
  _a_ll.commit  _r_eword  _u_npushed  _s_tatus  _b_lame  _d_eleted
"
   ("a" my-commit-all)
   ("r" my-commit-reword)
   ("u" my-git-unpushed)
   ("s" my-git-status)
   ("b" my-git-blame)
   ("d" git-peek-deleted)
   ("q" top-level)
   ("<muhenkan>" nil)))

;; ============================================================
;; git-peek (差分プレビュー)
;; ============================================================
(leaf git-peek
  :vc (:url "https://github.com/minorugh/git-peek" :rev :newest)
  :preface
  (autoload 'git-peek "git-peek" nil t)
  (autoload 'git-peek-deleted "git-peek" nil t)
  :config
  (setq git-peek-sidebar-width 45)
  (setq git-peek-save-dir (expand-file-name "~/tmp/git-peek/")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 02-git.el ends here
