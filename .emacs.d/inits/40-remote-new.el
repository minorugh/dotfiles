;;; 40-remote-new.el --- Xserver deploy/backup/2pane operations. -*- lexical-binding: t -*-
;;; Commentary:
;;; xsrv-GH / xsrv-minorugh 関連の個人設定をすべてここに集約する。
;;; 旧 my-xsrv.el の内容(2pane本体・UI・divider)もこのファイルに統合済み。
;;;
;;; 大分類:
;;;   ▼ 共通       … 1
;;;   ▼ UI/見た目系 … 2-4
;;;   ▼ 機能系      … 5-8
;;;
;;; 目次 (swiper/counsel-grep で見出し文言を検索してジャンプする想定):
;;;   1. xsrv ルート判定  (共通ヘルパー)
;;;   2. xsrv-2pane 見た目  (ヘッダー)
;;;   3. window-divider
;;;   4. バッファ識別  (背景色)
;;;   5. Deploy / Download
;;;   6. xsrv-2pane 本体
;;;   7. git-peek 連携
;;;   8. 動的フォルダー保護 & rsync lock
;;;
;;; 40-remote.el (旧版) からの変更点:
;;;   - 未使用のアイコン表示コード (remote/local アイコン名, --icon-for,
;;;     --prepend-icon) を削除。
;;;   - 命名プレフィックスを my-xsrv- に統一 (旧 my:xsrv- を統合)。
;;;   - 動的フォルダー保護の key-chord 登録が、対象ファイルを開くたびに
;;;     無条件で再定義されていたのを、未登録の場合のみ登録するよう変更
;;;     (key-chord の内部表現には依存せず、登録済みキーマップを自前で
;;;     記録する方式で冪等化)。
;;;   - `--fontify-header' を削除。直後の `--set-header-line' で対象行が
;;;     invisible 化され画面に反映されていなかったことを動作確認の上で
;;;     確定し、関数ごと削除した。
;;;   - セクション順を「共通→UI/見た目系→機能系」の大分類順に並び替え。
;;;     ただし機能系の中で UI/見た目系の関数を呼び出す依存関係がある
;;;     (5.xsrv-2pane本体 → 2.見た目/3.divider) ため、UI/見た目系を
;;;     機能系より前に配置している。
;;; Code:

;; ============================================================
;; ▼ 共通
;; ============================================================

;; ============================================================
;; 1. xsrv ルート判定  (共通ヘルパー)
;; ============================================================

(defconst my-xsrv-roots
  `((,(expand-file-name "~/src/github.com/minorugh/xsrv-GH/")
     . ,(expand-file-name "~/Dropbox/GH/"))
    (,(expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/")
     . ,(expand-file-name "~/Dropbox/minorugh.com/")))
  "Xsrv 側ルートパスとローカル(Dropbox)側ルートパスの対応表.")

(defun my-xsrv-root-for (path)
  "PATH が xsrv-GH/xsrv-minorugh 配下なら (xsrv-root . local-root) を返す。それ以外は nil."
  (let ((path (expand-file-name path)))
    (cl-find-if (lambda (pair) (string-prefix-p (car pair) path)) my-xsrv-roots)))

(defun my-xsrv-p (path)
  "PATH が xsrv-GH/xsrv-minorugh 配下であれば t を返す."
  (and path (my-xsrv-root-for path) t))


;; ============================================================
;; ▼ UI / 見た目系
;; ============================================================

;; ============================================================
;; 2. xsrv-2pane 見た目  (ヘッダー)
;; 70-hydra-dired.el の hydra-dired から呼ばれる想定。
;; ペイン終了処理(my-2pane-quit/my-dired-quit)は 70-hydra-dired.el 側の汎用機能。
;; ============================================================

(defface my-xsrv-2pane-header-face
  '((t (:inherit dired-header :background "#1A2640" :box (:line-width 2))))
  "Xsrv-2pane の `dired' バッファでヘッダー2行に使う face."
  :group 'dired)

(defun my-xsrv-2pane--set-header-line ()
  "Dired バッファの上2行を `header-line-format' に固定表示し、本体から隠す."
  (save-excursion
    (goto-char (point-min))
    (let* ((label (if (my-xsrv-p default-directory) "  [REMOTE]" "  [LOCAL]"))
           (line1 (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (line2 (progn (forward-line 1)
                         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
           (end (progn (forward-line 1) (point))))
      (setq-local header-line-format
                  (list (propertize (concat label "  " line1 "  " line2
                                            (make-string 200 ?\s))
                                    'face 'my-xsrv-2pane-header-face)))
      (with-silent-modifications
        (put-text-property (point-min) end 'invisible t)))))

(defun my-xsrv-2pane-refresh-ui ()
  "Xsrv-2pane バッファの見た目（ヘッダー行表示）を適用する.`revert-buffer' 後にも呼べるよう冪等にしてある."
  (when (derived-mode-p 'dired-mode)
    (my-xsrv-2pane--set-header-line)))

(defun my-xsrv-2pane-enable-ui ()
  "現在のバッファを xsrv-2pane 対象として UI 調整を適用する.`my-open-xsrv-2pane' から呼ぶこと."
  (add-hook 'dired-after-readin-hook #'my-xsrv-2pane-refresh-ui nil t)
  (my-xsrv-2pane-refresh-ui))


;; ============================================================
;; 3. window-divider
;; ============================================================

(defvar my-2pane-divider-active nil
  "Non-nil while the xsrv-2pane window-divider highlight is active.")

(defun my-2pane-divider-on ()
  "Enable a prominent window-divider, scoped to xsrv-2pane usage."
  (window-divider-mode -1)
  (setq window-divider-default-right-width 4)
  (setq window-divider-default-bottom-width 0)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)
  (set-face-foreground 'window-divider "#61bfff")
  (set-face-foreground 'window-divider-first-pixel "#61bfff")
  (set-face-foreground 'window-divider-last-pixel "#61bfff")
  (setq my-2pane-divider-active t))

(defun my-2pane-divider-off ()
  "Restore window-divider to its default (disabled) state."
  (when my-2pane-divider-active
    (window-divider-mode -1)
    (setq my-2pane-divider-active nil)))


;; ============================================================
;; 4. バッファ識別  (背景色)
;; xsrv-GH / xsrv-minorugh 配下を背景色で示す。
;; ============================================================

(defvar my-xsrv-buffer-color "#233B6C"
  "Background color applied to buffers under xsrv-GH or xsrv-minorugh.")

(defun my-xsrv--maybe-colorize ()
  "Xsrv-GH/xsrv-minorugh 配下のバッファなら `buffer-face-mode' で背景色を適用する."
  (when (my-xsrv-p default-directory)
    (buffer-face-set `(:background ,my-xsrv-buffer-color))))

(add-hook 'dired-mode-hook         #'my-xsrv--maybe-colorize)
(add-hook 'dired-after-readin-hook #'my-xsrv--maybe-colorize)
(add-hook 'find-file-hook          #'my-xsrv--maybe-colorize)


;; ============================================================
;; ▼ 機能系
;; ============================================================

;; ============================================================
;; 5. Deploy / Download  (local dired ⇄ xserver)
;; キーバインドは 60-dired.el で定義。
;; ============================================================

(defun xsrv-deploy-dired ()
  "Deploy file at point in `dired' to xserver via deploy.pl."
  (interactive)
  (let* ((file (dired-get-filename))
         (name (file-name-nondirectory file)))
    (cond
     ((file-directory-p file)
      (message "Error: ディレクトリは deploy できません。"))
     ((string-match-p "\\(^Makefile$\\|^README\\|\\.mk$\\|\\.bak$\\)" name)
      (message "Error: %s は deploy 対象外です。" name))
     ((not (or (string-prefix-p "/home/minoru/Dropbox/GH/" file)
               (string-prefix-p "/home/minoru/Dropbox/minorugh.com/" file)))
      (message "Error: deploy 対象外のファイルです。"))
     (t
      (when (x-popup-dialog
             t
             `(,(format "本当に deploy しますか？\n\n  %s" name)
               ("Deploy する" . t)
               ("やめる"      . nil)))
        (shell-command
         (format "perl ~/Dropbox/GH/common/deploy.pl %s" file)))))))

(defun xsrv-download-dired ()
  "Download file at point from xsrv-GH or xsrv-minorugh to local Dropbox."
  (interactive)
  (let* ((file      (dired-get-filename))
         (name      (file-name-nondirectory file))
         (root-pair (my-xsrv-root-for file)))
    (unless root-pair
      (user-error "Error: xsrv-GH/xsrv-minorugh の Dired から実行してください"))
    (let* ((xsrv-root  (car root-pair))
           (local-root (cdr root-pair))
           (rel        (file-relative-name file xsrv-root))
           (dest       (concat local-root rel)))
      (when (x-popup-dialog
             t
             `(,(format "ローカルにダウンロードしますか？\n\n  %s" name)
               ("Download する" . t)
               ("やめる"        . nil)))
        (if (and (file-exists-p dest)
                 (not (y-or-n-p (format "%s は既にあります。上書きしますか?" name))))
            (message "キャンセルしました。")
          (copy-file file dest t)
          (message "Downloaded: %s" rel)
          (dolist (root (list xsrv-root local-root))
            (let ((buf (get-buffer (file-name-nondirectory
                                    (directory-file-name root)))))
              (when buf
                (with-current-buffer buf
                  (revert-buffer))))))))))


;; ============================================================
;; 6. xsrv-2pane 本体
;; ============================================================

(defun my-open-xsrv-2pane (src-dir pair-dir)
  "Open SRC-DIR and PAIR-DIR side by side."
  (setq my-2pane-origin-buffer (current-buffer))
  (shell-command "~/.emacs.d/elisp/bin/xsrv-backup-smart.sh &")
  (delete-other-windows)
  (dired src-dir)
  (my-xsrv-2pane-enable-ui)
  (split-window-right)
  (other-window 1)
  (dired pair-dir)
  (my-xsrv-2pane-enable-ui)
  (other-window 1)
  (my-2pane-divider-on))

;; my-2pane-quit-hook は 70-hydra-dired.el 側で defvar される拡張ポイント。
;; ここでは divider 解除だけを差し込む。
(add-hook 'my-2pane-quit-hook #'my-2pane-divider-off)

;; -- hydra から呼ぶための薄いラッパー (70-hydra-dired.el の ":" ";" から参照) --

(defun my-open-xsrv-2pane-gh ()
  "Xsrv-GH と Dropbox/GH を 2ペインで開く."
  (interactive)
  (let ((pair (car my-xsrv-roots)))
    (my-open-xsrv-2pane (car pair) (cdr pair))))

(defun my-open-xsrv-2pane-minorugh ()
  "Xsrv-minorugh と Dropbox/minorugh.com を 2ペインで開く."
  (interactive)
  (let ((pair (cadr my-xsrv-roots)))
    (my-open-xsrv-2pane (car pair) (cdr pair))))


;; ============================================================
;; 7. git-peek 連携  (差分プレビュー、xsrv配下なら2pane復元と連携)
;; ============================================================

(leaf git-peek
  :vc (:url "https://github.com/minorugh/git-peek" :only-if-missing t)
  :config
  (setq git-peek-save-dir (expand-file-name "~/tmp/"))

  (defun my-git-peek-smart ()
    "Run `git-peek' with save-dir adjusted for xsrv dired context.
xsrv 配下なら差分表示後に 2ペインを復元する。"
    (interactive)
    (let* ((dir          (expand-file-name default-directory))
           (orig         git-peek-save-dir)
           (root-pair    (my-xsrv-root-for dir))
           (new-save-dir (if root-pair
                              (concat (cdr root-pair)
                                      (file-relative-name dir (car root-pair)))
                            orig)))
      (setq git-peek-save-dir new-save-dir)
      (let ((fn nil))
        (setq fn (lambda ()
                   (setq git-peek-save-dir orig)
                   (when root-pair
                     (my-open-xsrv-2pane dir new-save-dir))
                   (remove-hook 'git-peek-finish-hook fn)))
        (add-hook 'git-peek-finish-hook fn))
      (git-peek))))


;; ============================================================
;; 8. 動的フォルダー保護 & rsync lock
;; Dropbox/GH 配下の動的フォルダーを自動 read-only 化。
;; read-only 解除時に rsync lock を発行する。
;; カレントから外れるか kill されたら自動で read-only に戻し、
;; 全バッファが read-only になったら lock を解除する。
;; 緊急停止は power-menu の BACKUP STOP/START で手動対応。
;; ============================================================

(defconst my-xsrv-dynamic-dirs
  (mapcar (lambda (d) (expand-file-name (concat "~/Dropbox/GH/" d)))
          '("apvoice/log/" "apvoice/voice/"
            "danwa/data/" "danwa/html/"
            "dia/divoice/"
            "d_select/voice/" "m_select/voice/" "s_select/voice/" "w_select/voice/"
            "d_kukai/back/" "d_kukai/data/" "d_kukai/html/" "d_kukai/score/"
            "m_kukai/back/" "m_kukai/data/" "m_kukai/html/" "m_kukai/score/"
            "s_kukai/back/" "s_kukai/data/" "s_kukai/html/" "s_kukai/score/"
            "w_kukai/back/" "w_kukai/data/" "w_kukai/html/" "w_kukai/score/"))
  "Rsync lock の対象となる動的フォルダーの絶対パスリスト.")

(defconst my-xsrv-lockfile (expand-file-name "~/xsrv-rsync.lock"))

(defun my-xsrv-dynamic-p (file)
  "FILE が動的フォルダー配下であれば t を返す."
  (when file
    (cl-some (lambda (dir) (string-prefix-p dir file))
             my-xsrv-dynamic-dirs)))

(defun my-xsrv-lock ()
  "Rsync lock ファイルを発行する."
  (unless (file-exists-p my-xsrv-lockfile)
    (write-region "" nil my-xsrv-lockfile)
    (message "[xsrv] rsync lock ON")))

(defun my-xsrv-unlock-if-clean ()
  "動的フォルダー配下の編集中バッファがゼロなら lock を解除する."
  (unless (cl-some (lambda (buf)
                     (with-current-buffer buf
                       (and (buffer-file-name)
                            (my-xsrv-dynamic-p (buffer-file-name))
                            (not buffer-read-only))))
                   (buffer-list))
    (when (file-exists-p my-xsrv-lockfile)
      (delete-file my-xsrv-lockfile)
      (message "[xsrv] rsync lock OFF"))))

(defvar my-xsrv--qq-chord-registered-maps nil
  "`my-xsrv-find-file-hook' が qq の key-chord を登録済みのキーマップ一覧.
key-chord の内部表現に依存せず、自前で登録済みかどうかを判定するために使う。")

(defun my-xsrv-find-file-hook ()
  "動的フォルダー配下のファイルを自動 read-only にする.
`qq' の key-chord は、まだ登録していないキーマップに対してのみ登録する
(対象ファイルを開くたびに無条件で再定義していたのを冪等化)."
  (when (my-xsrv-dynamic-p (buffer-file-name))
    (read-only-mode 1)
    (let ((map (or (current-local-map) global-map)))
      (unless (memq map my-xsrv--qq-chord-registered-maps)
        (key-chord-define map "qq" #'my-makefile-toggle-readonly)
        (push map my-xsrv--qq-chord-registered-maps)))))

(defun my-xsrv-read-only-hook ()
  "Read-only 解除時に lock を発行、復帰時に unlock チェックする."
  (when (my-xsrv-dynamic-p (buffer-file-name))
    (if buffer-read-only
        (my-xsrv-unlock-if-clean)
      (my-xsrv-lock))))

(defun my-xsrv-kill-buffer-hook ()
  "動的ファイルの kill 時に read-only 化してから unlock チェックする."
  (when (my-xsrv-dynamic-p (buffer-file-name))
    (read-only-mode 1)
    (my-xsrv-unlock-if-clean)))

(defun my-xsrv-buffer-list-update-hook ()
  "カレントから外れた動的バッファを自動 read-only に戻す."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (my-xsrv-dynamic-p (buffer-file-name))
                   (not buffer-read-only))
          (read-only-mode 1)
          (my-xsrv-unlock-if-clean))))))

(add-hook 'find-file-hook          #'my-xsrv-find-file-hook)
(add-hook 'read-only-mode-hook     #'my-xsrv-read-only-hook)
(add-hook 'kill-buffer-hook        #'my-xsrv-kill-buffer-hook)
(add-hook 'buffer-list-update-hook #'my-xsrv-buffer-list-update-hook)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 40-remote-new.el ends here
