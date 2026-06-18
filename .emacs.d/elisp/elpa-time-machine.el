;;; elpa-time-machine.el --- Browse past elpa snapshots via sidebar  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Minoru Yamada and Claude (Anthropic)
;; Author: Minoru Yamada <minorugh@gmail.com>
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: elpa, backup, versioning

;;; Commentary:

;; elpaバックアップ（rsync + git管理）の過去スナップショットを
;; サイドバーで選択してプレビュー、s で tmp/yyyymmdd-elpa/ に保存する。
;;
;; バックアップ構成（Makefile側）:
;;   ~/Dropbox/backup/elpa/  ← rsync + git管理されたelpaミラー
;;   /tmp/tm-yyyymmdd-elpa/  ← プレビュー展開先（Q終了時に自動削除）
;;   ~/tmp/yyyymmdd-elpa/    ← S で保存する恒久保存先
;;
;; キー操作（*elpa-tm-commits* サイドバー内）:
;;   <down> / j / SPC  次のコミット＋右バッファ更新
;;   <up> / k / b      前のコミット＋右バッファ更新
;;   RET               右バッファ（`dired'）へフォーカス移動
;;   s                 確定・tmp/yyyymmdd-elpa/ に保存してfinish
;;   q / C-g           キャンセル
;;   ?                 キーガイドをミニバッファに表示
;;
;; キー操作（右 `dired' バッファ内）:
;;   RET / f           サイドバーへフォーカス復帰
;;   s                 確定・保存してfinish
;;   q / C-g           キャンセル
;;
;; Usage:
;;   M-x elpa-time-machine  - 過去のelpaスナップショットを選択・展開

;;; Code:

;;; Customizable variables

(defvar elpa-tm-backup-dir (expand-file-name "~/Dropbox/backup/elpa/")
  "Rsync + git管理されたelpaバックアップディレクトリ.")

(defvar elpa-tm-preview-dir "/tmp/"
  "Preview directory under /tmp/.  Cleaned up on exit.")

(defvar elpa-tm-save-dir (expand-file-name "~/tmp/")
  "S で保存する恒久保存先.")

(defvar elpa-tm-sidebar-width 36
  "コミット一覧サイドバーの幅（文字数）.")

(defvar elpa-tm-modeline-color "#852941"
  "アクティブバッファのモードライン背景色。nil で無効.")

(defface elpa-tm-header-face
  '((t :foreground "#aaaaaa" :background "#2a3a5a" :weight bold :extend t))
  "サイドバー先頭ヘッダー行のフェイス.")

;;; Session variables

(defvar elpa-tm--sidebar-win nil)
(defvar elpa-tm--preview-win nil)
(defvar elpa-tm--hl-overlay nil)
(defvar elpa-tm--header-overlay nil)
(defvar elpa-tm--saved-wconf nil)
(defvar elpa-tm--modeline-cookie nil)
(defvar elpa-tm--modeline-default nil)
(defvar elpa-tm--current-dest nil "現在プレビュー中の展開先パス.")

;;; Internal helpers

(defun elpa-tm--git (cmd)
  "Elpa-tm-backup-dir で git コマンド CMD を実行して結果を返す."
  (string-trim
   (shell-command-to-string
    (format "git -C %s %s" elpa-tm-backup-dir cmd))))

(defun elpa-tm--commits ()
  "コミット一覧を (表示ラベル . hash) のリストで返す."
  (let ((lines (split-string
                (elpa-tm--git
                 "log --format='%h	%cd	%s' --date=format:'%Y-%m-%d %H:%M'")
                "\n" t)))
    (mapcar (lambda (line)
              (let* ((parts (split-string line "\t"))
                     (hash  (nth 0 parts))
                     (date  (nth 1 parts))
                     (subj  (nth 2 parts)))
                (cons (format "%-18s  %s" date subj) hash)))
            lines)))

(defun elpa-tm--current-hash ()
  "サイドバーのカーソル行からhashを返す."
  (get-text-property (line-beginning-position) 'elpa-tm-hash))

(defun elpa-tm--set-modeline-color ()
  "モードライン色を設定する."
  (when elpa-tm-modeline-color
    (set-face-background 'mode-line elpa-tm-modeline-color)
    (setq elpa-tm--modeline-cookie t)))

;;; Focus management

(defun elpa-tm--focus-preview ()
  "右プレビューウィンドウにフォーカスを移す."
  (when (window-live-p elpa-tm--preview-win)
    (select-window elpa-tm--preview-win)
    (elpa-tm--set-modeline-color)))

(defun elpa-tm--focus-sidebar ()
  "サイドバーウィンドウにフォーカスを戻す."
  (when (window-live-p elpa-tm--sidebar-win)
    (select-window elpa-tm--sidebar-win)
    (elpa-tm--set-modeline-color)))

;;; Highlight

(defun elpa-tm--highlight-header ()
  "サイドバー先頭行にヘッダーオーバーレイを適用する."
  (when (overlayp elpa-tm--header-overlay)
    (delete-overlay elpa-tm--header-overlay)
    (setq elpa-tm--header-overlay nil))
  (save-excursion
    (goto-char (point-min))
    (let* ((inhibit-read-only t)
           (text " [elpa-time-machine]")
           (pad  (max 0 (- elpa-tm-sidebar-width (length text)))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert (concat text (make-string pad ?\s))))
    (let* ((bol (line-beginning-position))
           (eol (min (point-max) (1+ (line-end-position))))
           (ov  (make-overlay bol eol (current-buffer))))
      (overlay-put ov 'face 'elpa-tm-header-face)
      (overlay-put ov 'extend t)
      (overlay-put ov 'priority 50)
      (setq elpa-tm--header-overlay ov))))

(defun elpa-tm--highlight-current ()
  "カーソル行をハイライトする."
  (when (overlayp elpa-tm--hl-overlay)
    (delete-overlay elpa-tm--hl-overlay)
    (setq elpa-tm--hl-overlay nil))
  (let* ((bol (line-beginning-position))
         (eol (min (point-max) (1+ (line-end-position))))
         (ov  (make-overlay bol eol (current-buffer))))
    (overlay-put ov 'face 'highlight)
    (overlay-put ov 'priority 100)
    (setq elpa-tm--hl-overlay ov)))

;;; Preview (右バッファに`dired'を表示)

(defun elpa-tm--render-preview (hash)
  "HASH のスナップショットを右バッファに `dired' で表示する.
フォーカスはサイドバーに保つ."
  (when hash
    (let* ((date (elpa-tm--git
                  (format "show -s --format=%%cd --date=format:%%Y%%m%%d %s" hash)))
           (dest (expand-file-name (concat "tm-" date "-elpa") elpa-tm-preview-dir)))
      (setq elpa-tm--current-dest dest)
      (unless (file-directory-p dest)
        (make-directory dest t)
        (message "elpa-time-machine: 展開中 %s ..." date)
        (shell-command
         (format "git -C %s checkout %s -- ." elpa-tm-backup-dir hash))
        (shell-command
         (format "rsync -a --exclude='.git' %s/ %s/"
                 elpa-tm-backup-dir dest))
        (shell-command
         (format "git -C %s checkout HEAD -- ." elpa-tm-backup-dir))
        (message "elpa-time-machine: 展開完了 → %s" dest))
      (when (window-live-p elpa-tm--preview-win)
        (let ((buf (dired-noselect dest)))
          (with-current-buffer buf
            (elpa-tm-preview-mode 1))
          (with-selected-window elpa-tm--preview-win
            (switch-to-buffer buf)))))))

;;; Navigation

(defun elpa-tm--move (lines)
  "LINES 行移動してハイライトと右バッファを更新する."
  (with-selected-window elpa-tm--sidebar-win
    (let ((top (save-excursion
                 (goto-char (point-min))
                 (forward-line 1)
                 (point)))
          (bottom (save-excursion
                    (goto-char (point-max))
                    (skip-chars-backward "\n")
                    (line-beginning-position))))
      (forward-line lines)
      (cond
       ((< (point) top)    (goto-char bottom))
       ((> (point) bottom) (goto-char top)))
      (elpa-tm--highlight-current)
      (set-window-point elpa-tm--sidebar-win (point))
      (elpa-tm--render-preview (elpa-tm--current-hash)))))

(defun elpa-tm-next ()
  "次のコミットへ移動."
  (interactive)
  (elpa-tm--move 1))

(defun elpa-tm-prev ()
  "前のコミットへ移動."
  (interactive)
  (elpa-tm--move -1))

(defun elpa-tm-go-preview ()
  "右バッファへフォーカスを移す."
  (interactive)
  (elpa-tm--focus-preview))

(defun elpa-tm-go-sidebar ()
  "サイドバーへフォーカスを戻す."
  (interactive)
  (elpa-tm--focus-sidebar))

(defun elpa-tm-show-help ()
  "キーガイドをミニバッファに表示."
  (interactive)
  (message "[sidebar] ↓/j:next ↑/k:prev RET:右へ s:保存 q:終了  |  [preview] RET/f:戻る s:保存 q:終了"))

;;; Save & finish

(defun elpa-tm-save ()
  "現在プレビュー中のスナップショットを ~/tmp/yyyymmdd-elpa/ に保存してfinishする."
  (interactive)
  (if elpa-tm--current-dest
      (let* ((basename (file-name-nondirectory elpa-tm--current-dest))
             ;; tm-20260606-elpa → 20260606-elpa
             (savename (replace-regexp-in-string "^tm-" "" basename))
             (dest (expand-file-name savename elpa-tm-save-dir)))
        (unless (file-directory-p dest)
          (make-directory dest t)
          (shell-command
           (format "rsync -a %s/ %s/" elpa-tm--current-dest dest)))
        (elpa-tm--finish)
        (switch-to-buffer (dired-noselect dest))
        (message "elpa-time-machine: 保存済み → %s" dest))
    (message "elpa-time-machine: プレビューがありません（先にj/kで選択）")))

(defun elpa-tm-cancel ()
  "キャンセルして終了."
  (interactive)
  (elpa-tm--finish)
  (message "elpa-time-machine: cancelled"))

;;; Cleanup & Teardown

(defun elpa-tm--cleanup-previews ()
  "プレビュー用に展開した /tmp/tm-*-elpa/ をまとめて削除する."
  (dolist (dir (file-expand-wildcards "/tmp/tm-*-elpa" t))
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (message "elpa-time-machine: /tmp/tm-*-elpa を削除しました"))

(defun elpa-tm--finish ()
  "レイアウトを解体してウィンドウ設定を復元する."
  (when (overlayp elpa-tm--hl-overlay)
    (delete-overlay elpa-tm--hl-overlay)
    (setq elpa-tm--hl-overlay nil))
  (when (overlayp elpa-tm--header-overlay)
    (delete-overlay elpa-tm--header-overlay)
    (setq elpa-tm--header-overlay nil))
  (when elpa-tm--modeline-cookie
    (set-face-background 'mode-line elpa-tm--modeline-default)
    (setq elpa-tm--modeline-cookie nil))
  (when elpa-tm--saved-wconf
    (set-window-configuration elpa-tm--saved-wconf)
    (setq elpa-tm--saved-wconf nil))
  (dolist (bname '("*elpa-tm-commits*"))
    (when (get-buffer bname)
      (kill-buffer bname)))
  (elpa-tm--cleanup-previews))

;;; Keymaps

(defvar elpa-tm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") #'elpa-tm-next)
    (define-key map (kbd "j")      #'elpa-tm-next)
    (define-key map (kbd "SPC")    #'elpa-tm-next)
    (define-key map (kbd "<up>")   #'elpa-tm-prev)
    (define-key map (kbd "k")      #'elpa-tm-prev)
    (define-key map (kbd "b")      #'elpa-tm-prev)
    (define-key map (kbd "RET")    #'elpa-tm-go-preview)
    (define-key map (kbd "s")      #'elpa-tm-save)
    (define-key map (kbd "q")      #'elpa-tm-cancel)
    (define-key map (kbd "C-g")    #'elpa-tm-cancel)
    (define-key map (kbd "?")      #'elpa-tm-show-help)
    map)
  "Elpa-time-machine サイドバーのキーマップ.")

(defvar elpa-tm-preview-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "RET") #'elpa-tm-go-sidebar)
    ;; (define-key map (kbd "f")   #'elpa-tm-go-sidebar)
    (define-key map (kbd "q")   #'elpa-tm-cancel)
    (define-key map (kbd "C-g") #'elpa-tm-cancel)
    (define-key map (kbd "?")   #'elpa-tm-show-help)
    map)
  "Elpa-time-machine プレビュー用 minor-mode キーマップ.")

(define-minor-mode elpa-tm-preview-mode
  "Eelpa-time-machine プレビューバッファ用 minor-mode."
  :keymap elpa-tm-preview-mode-map)

;;; Major mode

(define-derived-mode elpa-tm-mode fundamental-mode "elpa-tm"
  "Elpa-time-machine のコミット選択モード."
  (setq-local truncate-lines t)
  (setq-local cursor-type 'bar)
  (setq-local buffer-read-only t)
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1))
  (setq-local mode-line-buffer-identification
              (list (propertize " [elpa-tm] ↓/j:next ↑/k:prev RET:右へ s:保存 Q:終了 ?:help"
                                'face 'mode-line-buffer-id))))

;;; Entry point

;;;###autoload
(defun elpa-time-machine ()
  "Elpaバックアップの過去スナップショットをサイドバーで選択して展開する.
j/k で移動すると右バッファに自動展開。s で tmp/yyyymmdd-elpa/ に確定保存."
  (interactive)
  (unless (file-directory-p elpa-tm-backup-dir)
    (error "elpa-time-machine: バックアップディレクトリが見つかりません: %s"
           elpa-tm-backup-dir))
  (unless (file-directory-p (expand-file-name ".git" elpa-tm-backup-dir))
    (error "elpa-time-machine: Gitリポジトリではありません: %s"
           elpa-tm-backup-dir))
  (let ((commits (elpa-tm--commits)))
    (unless commits
      (error "elpa-time-machine: コミットが見つかりません"))
    (setq elpa-tm--saved-wconf      (current-window-configuration)
          elpa-tm--modeline-default (face-background 'mode-line)
          elpa-tm--current-dest     nil)
    (delete-other-windows)
    (let* ((cbuf (get-buffer-create "*elpa-tm-commits*"))
           (pwin (selected-window))
           (swin (split-window pwin (- elpa-tm-sidebar-width) 'left)))
      (setq elpa-tm--sidebar-win swin
            elpa-tm--preview-win pwin)
      ;; 右ウィンドウ: バックアップ`dired'を初期表示
      (set-window-buffer pwin (dired-noselect elpa-tm-backup-dir))
      ;; 左サイドバー: コミット一覧
      (set-window-buffer swin cbuf)
      (with-current-buffer cbuf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "\n")
          (dolist (entry commits)
            (let ((label (car entry))
                  (hash  (cdr entry)))
              (insert (propertize (format " %s\n" label)
                                  'elpa-tm-hash hash))))
          (goto-char (point-min))
          (forward-line 1))
        (elpa-tm-mode)
        (elpa-tm--highlight-header)
        (elpa-tm--highlight-current))
      (set-window-point swin
                        (with-current-buffer cbuf
                          (save-excursion
                            (goto-char (point-min))
                            (forward-line 1)
                            (point))))
      (select-window swin)
      (elpa-tm--set-modeline-color)
      ;; 右バッファに preview minor-mode を有効化
      (with-current-buffer (window-buffer pwin)
        (elpa-tm-preview-mode 1))
      ;; 起動時は backup/elpa `dired' をそのまま表示（展開しない）
      )))

(provide 'elpa-time-machine)
;;; elpa-time-machine.el ends here
