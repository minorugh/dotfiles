;;; git-peek.el --- Browse and extract past git-committed file versions -*- lexical-binding: t -*-

;; Copyright (C) 2026 Minoru Yamada and Claude (Anthropic)
;; Author: Minoru Yamada <minorugh@gmail.com>
;; URL: https://github.com/minorugh/git-peek
;; Version: 1.10.0
;; Package-Requires: ((emacs "27.1") (ivy "0.13.0"))
;; Keywords: git, backup, versioning

;;; Commentary:

;; Browse past git-committed versions of files via ivy with real-time preview.
;; Selected versions are saved to ~/Dropbox/backup/tmp/ with a date prefix.
;;
;; レイアウト:
;;   左サイドバー : *git-peek-commits*  commit一覧
;;   右           : *git-peek-preview*  内容プレビュー（全画面）
;;
;; キー操作（*git-peek-commits* バッファ内）:
;;   <down> / j / SPC  次のコミット＋プレビュー更新
;;   <up> / k / b      前のコミット＋プレビュー更新
;;   RET           プレビューバッファへフォーカス移動
;;   s             確定・保存
;;   C-d           diff/full切り替え
;;   ?             キーガイドをミニバッファに表示
;;   q / C-g       キャンセル・終了
;;
;; キー操作（*git-peek-preview* バッファ内）:
;;   RET / f       サイドバーへフォーカス復帰
;;   s             確定・保存
;;   ?             キーガイドをミニバッファに表示
;;   q / C-g       キャンセル・終了
;;   その他        通常の読み取り専用スクロール
;;
;; カスタマイズ:
;;   git-peek-next-key / git-peek-prev-key で追加キーを設定可能
;;   git-peek-preview-modeline-color でアクティブバッファのモードライン色を指定
;;                                    （サイドバー・プレビュー共用）
;;   git-peek-filename-face でサイドバー先頭のファイル名行の表示スタイルを指定
;;
;; Usage:
;;   M-x git-peek         - Browse current files in the repository
;;   M-x git-peek-deleted - Browse deleted files in the repository
;;
;; Requires: ivy, git

;;; Code:

(require 'dired)

(declare-function ivy-read "ivy")

;;; Customizable variables

(defvar git-peek-sidebar-width 30
  "Width of the left sidebar showing commit list.")

(defvar git-peek-save-dir (expand-file-name "~/tmp/git-peek/")
  "Directory to save extracted files.")

;; デフォルトの保存先ディレクトリがなければ自動作成
(unless (file-directory-p git-peek-save-dir)
  (make-directory git-peek-save-dir t))

(defvar git-peek-finish-hook nil
  "Hook run after `git-peek--finish' completes.")

(defvar git-peek-show-diff nil
  "When non-nil, preview shows diff against current file instead of full content.")

(defvar git-peek-toggle-diff-key (kbd "C-d")
  "Key to toggle diff/full-content preview during commit selection.")

(defvar git-peek-next-key nil
  "Additional key to move to next commit, nil = disabled.")

(defvar git-peek-prev-key nil
  "Additional key to move to previous commit, nil = disabled.")

(defvar git-peek-preview-modeline-color "#852941"
  "Modeline background color for the active`git-peek' buffer (sidebar and preview).
Set to nil to disable color change.
Example: \"#852941\"")

(defface git-peek-filename-face
  '((t :foreground "#aaaaaa" :background "#2a3a5a" :weight bold :extend t))
  "Face for the filename header line at the top of the sidebar.")

;;; Session variables

(defvar git-peek--root nil)
(defvar git-peek--file nil)
(defvar git-peek--deleted nil)
(defvar git-peek--hl-overlay nil)
(defvar git-peek--filename-overlay nil "Overlay covering the filename header line in the sidebar.")
(defvar git-peek--sidebar-win nil "The sidebar window object.")
(defvar git-peek--preview-win nil "The preview window object.")
(defvar git-peek--dimmer-was-on nil "Non-nil if dimmer-mode was active before `git-peek'.")
(defvar git-peek--saved-wconf nil "Window configuration saved before `git-peek' layout.")
(defvar git-peek--preview-modeline-cookie nil "Face-remap cookie for preview modeline color.")
(defvar git-peek--modeline-color-default nil "Default mode-line background color saved before `git-peek'.")

;;; Internal helpers

(defun git-peek--find-root ()
  "Return the git repository root for the current buffer, or signal an error."
  (let ((root (locate-dominating-file
               (or buffer-file-name default-directory) ".git")))
    (unless root (error "Git repository not found"))
    (file-name-as-directory (file-truename root))))

(defun git-peek--normalize-path (path)
  "Remove leading \"./\" from PATH for use in git commands."
  (if (string-prefix-p "./" path) (substring path 2) path))

(defun git-peek--initial-input (root)
  "Return an ivy initial-input string.
The string is for the current buffer's file relative to ROOT."
  (let ((abspath
         (cond
          ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
          (buffer-file-name buffer-file-name)
          (t nil))))
    (when abspath
      (concat "^" (git-peek--normalize-path (file-relative-name abspath root))))))

(defun git-peek--mozc-off ()
  "Disable `mozc-mode' if it is currently active."
  (when (and (boundp 'mozc-mode) mozc-mode)
    (mozc-mode -1)))

(defun git-peek--git (root &rest args)
  "Run a git command in ROOT with ARGS and return trimmed output."
  (string-trim
   (shell-command-to-string
    (concat "git -C " root " " (mapconcat #'identity args " ")))))

;;; Focus management

(defun git-peek--set-modeline-color (color)
  "Set mode-line background to COLOR and record the cookie."
  (when git-peek-preview-modeline-color
    (set-face-background 'mode-line color)
    (setq git-peek--preview-modeline-cookie t)))

(defun git-peek--focus-preview ()
  "Move focus to preview window and apply modeline color."
  (when (window-live-p git-peek--preview-win)
    (select-window git-peek--preview-win)
    (git-peek--set-modeline-color git-peek-preview-modeline-color)))

(defun git-peek--focus-sidebar ()
  "Move focus back to sidebar and apply modeline color."
  (when (window-live-p git-peek--sidebar-win)
    (select-window git-peek--sidebar-win)
    (git-peek--set-modeline-color git-peek-preview-modeline-color)))

;;; Preview
;;
;; 重要: with-selected-window を使わない。
;; バッファ書き込みは with-current-buffer のみ。
;; ウィンドウ表示位置は set-window-point のみで制御。

(defun git-peek--render-preview (commit)
  "Render COMMIT content into *git-peek-preview* buffer.
Never changes window focus - sidebar remains selected."
  (when (string-match-p "^[0-9a-f]\\{7,\\}" commit)
    (condition-case err
        (let* ((hash (car (split-string commit " ")))
               (current-file (expand-file-name git-peek--file git-peek--root))
               (npath (git-peek--normalize-path git-peek--file))
               (content
                (if (and git-peek-show-diff
                         (not git-peek--deleted)
                         (file-exists-p current-file))
                    (let ((tmpfile (make-temp-file "git-peek-")))
                      (shell-command
                       (format "git -C %s show %s:%s > %s"
                               git-peek--root hash npath tmpfile))
                      (prog1
                          (shell-command-to-string
                           (format "diff %s %s" current-file tmpfile))
                        (delete-file tmpfile)))
                  (shell-command-to-string
                   (format "git -C %s show %s:%s" git-peek--root hash npath))))
               (date (git-peek--git git-peek--root
                                    "show -s --format=%cd --date=format:%Y%m%d" hash))
               (label (format "%s_%s" date (file-name-nondirectory git-peek--file))))
          (with-current-buffer (get-buffer-create "*git-peek-preview*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (if (string-empty-p content)
                          "(no diff - identical to current)"
                        content))
              (goto-char (point-min)))
            (let ((mode (if git-peek-show-diff
                            #'diff-mode
                          (assoc-default git-peek--file auto-mode-alist #'string-match))))
              (when mode (funcall mode)))
            (setq-local mode-line-buffer-identification
                        (list (propertize (format " [preview] %s" label)
                                          'face 'mode-line-buffer-id)))
            (setq-local buffer-read-only t)
            (when (and (fboundp 'view-mode) view-mode)
              (view-mode -1))
            (use-local-map git-peek-preview-mode-map)
            (when (fboundp 'evil-local-mode)
              (evil-local-mode -1)))
          (when (window-live-p git-peek--preview-win)
            (set-window-point git-peek--preview-win 1)))
      (error (message "Git-peek preview error: %S" err)))))

;;; Commit sidebar

(defun git-peek--show-help ()
  "Show a brief key guide in the minibuffer."
  (interactive)
  (message "[sidebar] ↓/j/SPC:next  ↑/k/b:prev  RET:preview  s:save  C-d:diff  q:quit  |  [preview] RET/f:back  s:save  q:quit"))

(defun git-peek--highlight-filename ()
  "Apply overlay covering the full filename header line in the sidebar."
  (when (overlayp git-peek--filename-overlay)
    (delete-overlay git-peek--filename-overlay)
    (setq git-peek--filename-overlay nil))
  (save-excursion
    (goto-char (point-min))
    ;; 行末までスペースを詰めてウィンドウ幅を埋める
    (let* ((inhibit-read-only t)
           (text (string-trim-right
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
           (pad  (max 0 (- git-peek-sidebar-width (length text)))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert (concat text (make-string pad ?\s))))
    (let* ((bol (line-beginning-position))
           (eol (min (point-max) (1+ (line-end-position))))
           (ov  (make-overlay bol eol (current-buffer))))
      (overlay-put ov 'face 'git-peek-filename-face)
      (overlay-put ov 'extend t)
      (overlay-put ov 'priority 50)
      (setq git-peek--filename-overlay ov))))

(defun git-peek--highlight-current ()
  "Highlight the line at point.
Must be called with *git-peek-commits* as current buffer."
  (when (overlayp git-peek--hl-overlay)
    (delete-overlay git-peek--hl-overlay)
    (setq git-peek--hl-overlay nil))
  (let* ((bol (line-beginning-position))
         (eol (min (point-max) (1+ (line-end-position))))
         (ov  (make-overlay bol eol (current-buffer))))
    (overlay-put ov 'face 'highlight)
    (overlay-put ov 'priority 100)
    (setq git-peek--hl-overlay ov)))

(defun git-peek--current-commit ()
  "Return commit string at point in current buffer."
  (string-trim
   (buffer-substring-no-properties
    (line-beginning-position) (line-end-position))))

(defun git-peek--move-and-preview (lines)
  "Move LINES forward in sidebar with wraparound, highlight, and update preview.
Keeps focus on the sidebar window throughout."
  (unless (window-live-p git-peek--sidebar-win)
    (error "Git-peek: sidebar window is gone"))
  (with-selected-window git-peek--sidebar-win
    (let* ((inhibit-read-only t)
           ;; 有効なコミット行の範囲: 2行目〜最後のコミット行
           (top (save-excursion
                  (goto-char (point-min))
                  (forward-line 1)
                  (point)))
           ;; コミット行は " HASH ..." の形式（先頭スペース＋7桁以上の16進数）
           (bottom (save-excursion
                     (goto-char (point-max))
                     (while (and (not (bobp))
                                 (not (string-match-p "^[0-9a-f]\\{7,\\}"
                                                      (string-trim
                                                       (buffer-substring-no-properties
                                                        (line-beginning-position)
                                                        (line-end-position))))))
                       (forward-line -1))
                     (line-beginning-position))))
      (forward-line lines)
      (cond
       ((< (point) top)    (goto-char bottom))  ; 上端→末尾へ
       ((> (point) bottom) (goto-char top)))    ; 下端→先頭へ
      (git-peek--highlight-current)
      (set-window-point git-peek--sidebar-win (point))
      (git-peek--render-preview (git-peek--current-commit)))))

(defun git-peek--commit-next ()
  "Move to next commit and update preview."
  (interactive)
  (git-peek--move-and-preview 1))

(defun git-peek--commit-prev ()
  "Move to previous commit and update preview."
  (interactive)
  (git-peek--move-and-preview -1))

(defun git-peek--commit-toggle-diff ()
  "Toggle diff/full preview and refresh."
  (interactive)
  (setq git-peek-show-diff (not git-peek-show-diff))
  (message "git-peek: preview = %s" (if git-peek-show-diff "diff" "full"))
  (git-peek--render-preview (git-peek--current-commit)))

(defun git-peek--commit-go-preview ()
  "Move focus to preview buffer."
  (interactive)
  (git-peek--focus-preview))

(defun git-peek--preview-go-sidebar ()
  "Move focus back to sidebar."
  (interactive)
  (git-peek--focus-sidebar))

(defun git-peek--commit-save ()
  "Save current commit (called from sidebar)."
  (interactive)
  (git-peek--finish (git-peek--current-commit)))

(defun git-peek--preview-save ()
  "Save current commit (called from preview buffer)."
  (interactive)
  (if (buffer-live-p (get-buffer "*git-peek-commits*"))
      (with-current-buffer "*git-peek-commits*"
        (git-peek--finish (git-peek--current-commit)))
    (message "git-peek: commits buffer not found")))

(defun git-peek--commit-cancel ()
  "Cancel commit selection."
  (interactive)
  (git-peek--finish nil))

(defun git-peek--finish (commit)
  "Tear down `git-peek' layout; save COMMIT if non-nil."
  (when (overlayp git-peek--hl-overlay)
    (delete-overlay git-peek--hl-overlay)
    (setq git-peek--hl-overlay nil))
  (when (overlayp git-peek--filename-overlay)
    (delete-overlay git-peek--filename-overlay)
    (setq git-peek--filename-overlay nil))
  ;; modeline色を必ず復元してからクリア
  (when git-peek--preview-modeline-cookie
    (set-face-background 'mode-line git-peek--modeline-color-default)
    (setq git-peek--preview-modeline-cookie nil))
  (when (and git-peek--dimmer-was-on (fboundp 'dimmer-mode))
    (dimmer-mode 1))
  ;; ウィンドウ設定を復元
  (when git-peek--saved-wconf
    (set-window-configuration git-peek--saved-wconf)
    (setq git-peek--saved-wconf nil))
  ;; 残存バッファをkill
  (dolist (bname '("*git-peek-commits*" "*git-peek-preview*"))
    (when (get-buffer bname)
      (kill-buffer bname)))
  (if (null commit)
      (message "git-peek: cancelled")
    (let* ((hash (car (split-string commit " ")))
           (date (git-peek--git git-peek--root
                                "show -s --format=%cd --date=format:%Y%m%d" hash))
           (dest (concat git-peek-save-dir date "_"
                         (file-name-nondirectory git-peek--file))))
      (unless (file-directory-p git-peek-save-dir)
        (make-directory git-peek-save-dir t))
      (shell-command
       (format "git -C %s show %s:%s > %s"
               git-peek--root hash
               (git-peek--normalize-path git-peek--file) dest))
      (dired git-peek-save-dir)
      (message "Saved: %s" dest)))
  (run-hooks 'git-peek-finish-hook))

;;; Keymaps

(defvar git-peek-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") #'git-peek--commit-next)
    (define-key map (kbd "SPC")    #'git-peek--commit-next)
    (define-key map (kbd "j")      #'git-peek--commit-next)
    (define-key map (kbd "<up>")   #'git-peek--commit-prev)
    (define-key map (kbd "b")      #'git-peek--commit-prev)
    (define-key map (kbd "k")      #'git-peek--commit-prev)
    (define-key map (kbd "RET")    #'git-peek--commit-go-preview)
    (define-key map (kbd "s")      #'git-peek--commit-save)
    (define-key map (kbd "C-g")    #'git-peek--commit-cancel)
    (define-key map (kbd "q")      #'git-peek--commit-cancel)
    (define-key map (kbd "?")      #'git-peek--show-help)
    map)
  "Keymap for *git-peek-commits* sidebar buffer.")

(defvar git-peek-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-global-map))
    (define-key map (kbd "RET") #'git-peek--preview-go-sidebar)
    (define-key map (kbd "f")   #'git-peek--preview-go-sidebar)
    (define-key map (kbd "s")   #'git-peek--preview-save)
    (define-key map (kbd "C-g") #'git-peek--commit-cancel)
    (define-key map (kbd "q")   #'git-peek--commit-cancel)
    (define-key map (kbd "?")   #'git-peek--show-help)
    map)
  "Keymap for *git-peek-preview* buffer.
Inherits global map so normal scroll keys (\\[scroll-up-command], \\[scroll-down-command], etc.) work.")

;;; Commit mode

(define-derived-mode git-peek-commit-mode fundamental-mode "git-peek"
  "Major mode for `git-peek' commit selection sidebar."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  (setq-local buffer-read-only t)
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1))
  (setq-local mode-line-buffer-identification
              (list (propertize " [git-peek] ↓/j/SPC:次 ↑/k/b:前 RET:プレビューへ s:保存 q:終了 C-d:diff ?:help"
                                'face 'mode-line-buffer-id))))

;;; Layout setup

(defun git-peek--run (root file &optional deleted)
  "Set up sidebar+preview layout for FILE in ROOT."
  (let* ((log-cmd (if deleted
                      (format "git -C %s log --all --oneline -- %s" root file)
                    (format "git -C %s log --oneline -- %s" root file)))
         (commits (split-string (shell-command-to-string log-cmd) "\n" t)))
    (unless commits
      (error "Git-peek: no commits found for %s" file))
    (setq git-peek--root    root
          git-peek--file    file
          git-peek--deleted deleted
          git-peek--modeline-color-default (face-background 'mode-line))
    ;; dimmer-mode paused
    (setq git-peek--dimmer-was-on
          (and (boundp 'dimmer-mode) dimmer-mode))
    (when git-peek--dimmer-was-on
      (dimmer-mode -1))
    ;; Clear existing buffer overlay
    (when (overlayp git-peek--hl-overlay)
      (delete-overlay git-peek--hl-overlay)
      (setq git-peek--hl-overlay nil))
    (dolist (bname '("*git-peek-commits*" "*git-peek-preview*"))
      (when (get-buffer bname)
        (let ((win (get-buffer-window bname)))
          (when win (delete-window win)))
        (kill-buffer bname)))
    ;; --- Window Layout Construction ---
    ;; Save current window settings
    (setq git-peek--saved-wconf (current-window-configuration))
    (delete-other-windows)
    (let* ((cbuf (get-buffer-create "*git-peek-commits*"))
           (pbuf (get-buffer-create "*git-peek-preview*"))
           (swin (split-window (selected-window) (- git-peek-sidebar-width) 'left))
           (pwin (selected-window)))
      (setq git-peek--sidebar-win swin
            git-peek--preview-win pwin)
      (set-window-buffer swin cbuf)
      (set-window-buffer pwin pbuf)
      (with-current-buffer cbuf
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; First line: display selected file name (face applies to entire line with overlay)
          (insert (format " %s\n" git-peek--file))
          ;; Commit line: 1 half-width indent
          (dolist (c commits) (insert (format " %s\n" c)))
          (goto-char (point-min))
          (forward-line 1))  ;; Skip filename line to first commit
        (git-peek-commit-mode)
        ;; C-d/additional keys are dynamically set to keymap after mode activation
        (local-set-key git-peek-toggle-diff-key #'git-peek--commit-toggle-diff)
        (when git-peek-next-key (local-set-key git-peek-next-key #'git-peek--commit-next))
        (when git-peek-prev-key (local-set-key git-peek-prev-key #'git-peek--commit-prev))
        (git-peek--highlight-filename)
        (git-peek--highlight-current))
      ;;; Ensure window point is set to first commit line
      (set-window-point swin
                        (with-current-buffer cbuf
                          (save-excursion
                            (goto-char (point-min))
                            (forward-line 1)
                            (point))))
      (select-window swin)
      ;; Apply modeline color as sidebar is activated
      (when git-peek-preview-modeline-color
        (set-face-background 'mode-line git-peek-preview-modeline-color)
        (setq git-peek--preview-modeline-cookie t))
      ;; draw a preview of the first commit
      (let ((first-commit (with-current-buffer cbuf
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line 1)
                              (string-trim
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position)))))))
        (git-peek--render-preview first-commit)))))

;;; Public commands

;;;###autoload
(defun git-peek ()
  "Browse past versions of files in the current git repository.
Skip ivy if variable `buffer-file-name' exactly matches the candidate."
  (interactive)
  (git-peek--mozc-off)
  (let* ((root (git-peek--find-root))
         (files (split-string
                 (shell-command-to-string
                  (format "git -C %s ls-files" root)) "\n" t))
         (rel   (and buffer-file-name
                     (git-peek--normalize-path
                      (file-relative-name buffer-file-name root))))
         (file  (if (and rel (member rel files))
                    (progn (message "git-peek: ivy Skip → %s" rel) rel)
                  (ivy-read "Select File: " files
                            :initial-input (git-peek--initial-input root)))))
    (git-peek--run root file nil)
    ;; Since the window selection may not come to the sidebar reliably when skipping ivy
    ;;; After run, return focus to sidebar and redraw initial preview.
    (when (and rel (member rel files))
      (when (window-live-p git-peek--sidebar-win)
        (select-window git-peek--sidebar-win))
      (with-current-buffer (get-buffer "*git-peek-commits*")
        (let ((first-commit
               (save-excursion
                 (goto-char (point-min))
                 (forward-line 1)
                 (string-trim
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))))
          (git-peek--render-preview first-commit))))))

;;;###autoload
(defun git-peek-deleted ()
  "Browse past versions of deleted files in the current git repository."
  (interactive)
  (git-peek--mozc-off)
  (let* ((root (git-peek--find-root))
         (files (delete-dups
                 (split-string
                  (shell-command-to-string
                   (format "git -C %s log --all --diff-filter=D --name-only --format=''" root))
                  "\n" t)))
         (file (ivy-read "Select Deleted File: " files)))
    (git-peek--run root file t)))

;;; Debug command

;;;###autoload
(defun git-peek-debug ()
  "Show diagnostic info about the current `git-peek' session in *Messages*."
  (interactive)
  (let* ((cbuf (get-buffer "*git-peek-commits*"))
         (cwin (and cbuf (get-buffer-window cbuf)))
         (pbuf (get-buffer "*git-peek-preview*"))
         (pwin (and pbuf (get-buffer-window pbuf))))
    (message "=== git-peek-debug ===")
    (if (not cbuf)
        (message "[commits] buffer: NOT FOUND")
      (message "[commits] buffer: exist, window: %s" (if cwin "exist" "NOT FOUND"))
      (when cwin
        (message "[commits] window-point: %d" (window-point cwin)))
      (with-current-buffer cbuf
        (message "[commits] buffer-point: %d  line: %d"
                 (point) (line-number-at-pos))
        (message "[commits] major-mode: %s" major-mode)
        (message "[commits] buffer-read-only: %s" buffer-read-only)
        (message "[commits] local-map: %s"
                 (if (eq (current-local-map) git-peek-commit-mode-map)
                     "git-peek-commit-mode-map (OK)"
                   (format "OTHER: %s" (current-local-map))))
        (message "[commits] current-commit: %S" (git-peek--current-commit))
        (message "[commits] hl-overlay: %s  buf: %s"
                 git-peek--hl-overlay
                 (and (overlayp git-peek--hl-overlay)
                      (overlay-buffer git-peek--hl-overlay)))))
    (if (not pbuf)
        (message "[preview] buffer: NOT FOUND")
      (message "[preview] buffer: exist, window: %s" (if pwin "exist" "NOT FOUND"))
      (when pwin
        (message "[preview] window-point: %d" (window-point pwin)))
      (with-current-buffer pbuf
        (message "[preview] buffer size: %d chars" (buffer-size))))
    (message "[session] root: %s" git-peek--root)
    (message "[session] file: %s" git-peek--file)
    (message "=== end ===")))

(provide 'git-peek)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; git-peek.el ends here
