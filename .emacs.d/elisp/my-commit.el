;;; my-commit.el --- Commit message buffer, dotfiles guard, Claude bridge. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; commit と push を分ける運用の、commit 側の設定。
;; 02-git.el の leaf で、下の「3. commit の入口」の関数が最初に呼ばれたときに
;; 読み込まれる。root の判定 (my-git--root) は 02-git.el にある。
;;
;; 構成
;;
;;   1. 共通部品          git の実行、dotfiles 用のガード
;;   2. commit メッセージ  *commit-msg* バッファ
;;   3. commit の入口      tig から呼ぶもの（tig-git.sh が emacsclient で呼ぶ）
;;
;; 2. commit メッセージ
;;   C-c C-c で確定、C-c C-k でキャンセル。
;;   メッセージは上に書く。下の情報ボックスは read-only。
;;   空のまま確定すると "auto: 日時" の仮メッセージになる（reword は不可）。
;;   C-c C-g: Claude に頼むためのプロンプト（直近の件名・変更ファイル・diff）を
;;   クリップボードにコピーし、3分間クリップボードを見張る。Claude の返事
;;   （1行、または件名＋空行＋箇条書き）をコピーすると、上の欄が空なら自動で入り、
;;   Emacs が前面に戻る。C-/ で消せる。
;;
;; 3. commit の入口（tig から呼ぶ）
;;   tig-git.sh が emacsclient で呼ぶ。tig status の C・A・R に対応:
;;     my-commit-file-from-tig  my-commit-all-from-tig  my-commit-reword-from-tig
;;
;;; Code:

(require 'seq)

(defvar my-main-machine-p)

(declare-function my-git--root "02-git")

;; ============================================================
;;  1. 共通部品
;;     git の実行、dotfiles 用のガード
;; ============================================================
(defvar my-commit-dotfiles-dir "~/src/github.com/minorugh/dotfiles/"
  "Dotfiles repository `make prep' runs only for this repository.")

(defun my-commit-msg--run (program &rest args)
  "Run PROGRAM with ARGS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'call-process program nil t nil args)))
      (cons status (string-trim (buffer-string))))))

(defun my-commit-msg--git (root &rest args)
  "Run git in ROOT with ARGS and return (STATUS . OUTPUT)."
  (apply #'my-commit-msg--run "git" "-C" root args))

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

;; ============================================================
;;  2. commit メッセージ
;;     *commit-msg* バッファ（確定は C-c C-c、キャンセルは C-c C-k）
;; ============================================================
(declare-function my-modeline-apply-highlight-now "30-ui")

(defvar my-commit-msg-buffer "*commit-msg*"
  "Name of the commit message buffer.")

(defvar-local my-commit-msg--root nil)

(defvar-local my-commit-msg--file nil)

(defvar-local my-commit-msg--wconf nil)

(defvar-local my-commit-msg--amend nil)

(defvar-local my-commit-msg--end nil)

(defvar my-commit-msg--watch-timer nil
  "Timer that watches the clipboard for Claude's reply.")

(defface my-commit-msg-box
  '((((background dark)) :background "#2b3452" :extend t)
    (t :background "#e8ecf5" :extend t))
  "Background of the commit info box.")

(defface my-commit-msg-title
  '((t :inherit font-lock-keyword-face :weight bold))
  "Title and labels of the commit info box.")

(define-derived-mode my-commit-msg-mode text-mode "CommitMsg"
  "Major mode for writing a commit message.")

(define-key my-commit-msg-mode-map (kbd "C-c C-c") #'my-commit-msg-finish)

(define-key my-commit-msg-mode-map (kbd "C-c C-k") #'my-commit-msg-cancel)

(define-key my-commit-msg-mode-map (kbd "C-c C-g") #'my-commit-msg-copy-prompt)

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

(defun my-commit-msg--close ()
  "Kill the message buffer and restore the window configuration."
  (my-commit-msg--watch-stop)
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

;; ---- Claude に頼む（クリップボード経由） ----
(defvar my-commit-msg-diff-max-lines 300
  "Maximum number of diff lines put into the prompt.")

(defvar my-commit-msg-watch-seconds 180
  "Seconds to watch the clipboard for the reply after the prompt is copied.")

(defun my-commit-msg--prompt ()
  "Return the prompt that asks Claude for a commit message."
  (let* ((root my-commit-msg--root)
         (file my-commit-msg--file)
         (subjects (seq-take
                    (seq-remove
                     (lambda (s) (string-prefix-p "auto:" s))
                     (split-string
                      (cdr (my-commit-msg--git root "log" "-30" "--format=%s"))
                      "\n" t))
                    10))
         (status (cdr (apply #'my-commit-msg--git root "status" "--short"
                             (and file (list "--" file)))))
         (raw-diff (cdr (cond (my-commit-msg--amend
                               (my-commit-msg--git root "show" "HEAD"))
                              (file
                               (my-commit-msg--git root "diff" "HEAD" "--" file))
                              (t
                               (my-commit-msg--git root "diff" "HEAD")))))
         (lines (split-string raw-diff "\n"))
         (diff (if (> (length lines) my-commit-msg-diff-max-lines)
                   (concat (string-join
                            (seq-take lines my-commit-msg-diff-max-lines) "\n")
                           "\n（以下省略）")
                 raw-diff)))
    (format (concat "以下の変更に対するコミットメッセージを書いてください。\n"
                    "変更が1〜2点なら、件名の1行だけを書いてください。\n"
                    "3点以上なら、1行目に件名、2行目を空行にし、3行目以降に変更点を"
                    "「- 」で始まる箇条書きで、1項目1行で書いてください。\n"
                    "言語と書き方は、次の直近のコミットの件名に合わせてください。\n"
                    "説明、引用符、コードブロックは付けず、メッセージ本文だけを出力してください。\n"
                    "\n## 直近のコミットの件名\n%s\n"
                    "\n## 変更されたファイル\n%s\n"
                    "\n## diff\n%s\n")
            (string-join subjects "\n") status diff)))

(defun my-commit-msg--watch-stop ()
  "Stop watching the clipboard."
  (when (timerp my-commit-msg--watch-timer)
    (cancel-timer my-commit-msg--watch-timer))
  (setq my-commit-msg--watch-timer nil))

(defun my-commit-msg--raise-frame ()
  "Bring the Emacs frame to the front."
  (select-frame-set-input-focus (selected-frame))
  (call-process "xdotool" nil nil nil "windowactivate"
                (frame-parameter nil 'outer-window-id)))

(defun my-commit-msg--reply-p (text)
  "Return non-nil if TEXT look like a commit message reply.
One line up to 200 chars, or up to 15 lines whose second line is empty
or starts a bullet list."
  (let ((lines (split-string text "\n")))
    (if (null (cdr lines))
        (<= (length text) 200)
      (and (<= (length lines) 15)
           (<= (length text) 1000)
           (let ((second (nth 1 lines)))
             (or (string-empty-p second)
                 (string-match-p "\\`\\(?:- \\|\\* \\|・\\)" second)))))))

(defun my-commit-msg--watch-tick (buf prompt start)
  "Put a one-line clipboard reply into BUF, the message buffer.
PROMPT is the copied prompt and START is the time the watch began."
  (if (or (not (buffer-live-p buf))
          (> (- (float-time) start) my-commit-msg-watch-seconds))
      (my-commit-msg--watch-stop)
    (let* ((clip (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
           (text (and (stringp clip) (string-trim clip))))
      (when (and text
                 (not (equal clip prompt))
                 (not (string-empty-p text))
                 (my-commit-msg--reply-p text))
        (my-commit-msg--watch-stop)
        (let ((win (get-buffer-window buf)))
          (when win (select-window win)))
        (with-current-buffer buf
          (if (string-empty-p (my-commit-msg--text))
              (progn (goto-char (point-min))
                     (insert text)
                     (message "コミットメッセージ案を入れました（C-/ で消せます）"))
            (message "入力済みのため、案は入れませんでした")))
        (my-commit-msg--raise-frame)))))

(defun my-commit-msg-copy-prompt ()
  "Copy a prompt asking Claude for a commit message, then watch the clipboard.
Paste the prompt into Claude and copy its reply: the reply is put into this
buffer when the message area is empty."
  (interactive)
  (let ((prompt (my-commit-msg--prompt))
        (buf (current-buffer)))
    (kill-new prompt)
    (my-commit-msg--watch-stop)
    (setq my-commit-msg--watch-timer
          (run-with-timer 0.5 0.5 #'my-commit-msg--watch-tick
                          buf prompt (float-time)))
    (message "プロンプトをコピーしました。Claude に貼り、返事をコピーしてください")))

;; ============================================================
;;  3. commit の入口（tig から呼ぶ）
;;     tig-git.sh が emacsclient で呼ぶ。C・A・R に対応
;; ============================================================
(defun my-commit-file-from-tig (file)
  "Open the commit message buffer for FILE.  Called from tig via emacsclient."
  (let* ((path (file-truename file))
         (root (my-git--root path))
         (buf (find-buffer-visiting path)))
    (when (and buf (buffer-modified-p buf))
      (with-current-buffer buf (save-buffer)))
    (my-commit-msg--check-machine root)
    (my-commit-msg--open root path)
    (select-frame-set-input-focus (selected-frame))))

(defun my-commit-all-from-tig (root)
  "Open the commit message buffer for all change in ROOT.
Called from tig via emacsclient."
  (my-commit-msg--check-machine root)
  (my-commit-msg--prep root)
  (my-commit-msg--open root nil)
  (select-frame-set-input-focus (selected-frame)))

(defun my-commit-reword-from-tig (root)
  "Open the commit message buffer to reword the last commit in ROOT.
Called from tig via emacsclient."
  (my-commit-msg--open root nil t)
  (select-frame-set-input-focus (selected-frame)))

(provide 'my-commit)
;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; my-commit.el ends here
