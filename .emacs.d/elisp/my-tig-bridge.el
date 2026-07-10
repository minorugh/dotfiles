;;; my-tig-bridge.el --- tig ↔ git-peek bridge -*- lexical-binding: t -*-
;;; Commentary:
;;
;; tig を起動する際にコンテキスト（ファイルパス）を /tmp/tig-peek-context に書き出す。
;; tig 側から emacsclient 経由で git-peek-from-hash を呼ぶことで
;; tig で見つけたコミットを git-peek で開ける。
;;
;; ~/.tigrc に以下を追加:
;;   bind generic E !sh -c "emacsclient --eval '(git-peek-from-hash \"%(commit)\")'"
;;
;;; Code:

(defvar my-tig-context-file "/tmp/tig-peek-context"
  "Tig 起動時にファイルパスを書き出す一時ファイル。常に上書きで一つだけ保持.")

;;;###autoload
(defun my-open-tig ()
  "Run tig for the current file (`dired' or file buffer) in gnome-terminal.
起動時に対象ファイルパスを `my-tig-context-file' に書き出す。
git 管理外の場合はエラーメッセージを表示する。"
  (interactive)
  (let* ((path (cond
                ((derived-mode-p 'dired-mode)
                 (dired-get-filename nil t))
                (buffer-file-name
                 buffer-file-name)
                (t nil)))
         (dir  (and path
                    (if (file-directory-p path)
                        path
                      (file-name-directory path))))
         (root (and dir (locate-dominating-file dir ".git"))))
    (cond
     ((null path)
      (message "tig: ファイルバッファまたは dired で実行してください"))
     ((null root)
      (message "tig: git 管理下のファイルではありません"))
     (t
      ;; コンテキストを書き出し（常に上書き）
      (write-region path nil my-tig-context-file nil 'silent)
      (start-process
       "tig" nil "gnome-terminal" "--maximize"
       "--working-directory" dir
       "--" "bash" "-c"
       (format "tig %s; exec bash"
               (shell-quote-argument path)))))))

(defun git-peek-from-hash (hash)
  "HASH を受け取り、`my-tig-context-file' のファイルを `git-peek' で開く.
tig 上のキーバインドから emacsclient 経由で呼ばれることを想定。
xsrv 配下なら `my-git-peek-smart' 相当の処理を自動適用する。"
  (let* ((ctx-file my-tig-context-file)
         (target (when (file-exists-p ctx-file)
                   (string-trim (with-temp-buffer
                                  (insert-file-contents ctx-file)
                                  (buffer-string))))))
    (unless (and target (file-exists-p target))
      (error "git-peek-from-hash: コンテキストファイルが見つかりません: %s" ctx-file))
    ;; 対象ファイルのバッファを用意してから git-peek を起動
    (find-file target)
    (let* ((dir          (expand-file-name default-directory))
           (orig         git-peek-save-dir)
           (xsrv-gh-root (expand-file-name "~/src/github.com/minorugh/xsrv-GH/"))
           (xsrv-mn-root (expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/"))
           (xsrv-p       (or (string-prefix-p xsrv-gh-root dir)
                             (string-prefix-p xsrv-mn-root dir)))
           (new-save-dir (cond
                          ((string-prefix-p xsrv-gh-root dir)
                           (concat (expand-file-name "~/Dropbox/GH/")
                                   (file-relative-name dir xsrv-gh-root)))
                          ((string-prefix-p xsrv-mn-root dir)
                           (concat (expand-file-name "~/Dropbox/minorugh.com/")
                                   (file-relative-name dir xsrv-mn-root)))
                          (t orig))))
      (setq git-peek-save-dir new-save-dir)
      (let ((fn nil))
        (setq fn (lambda ()
                   (setq git-peek-save-dir orig)
                   (when xsrv-p
                     (my-open-xsrv-2pane dir new-save-dir))
                   (remove-hook 'git-peek-finish-hook fn)))
        (add-hook 'git-peek-finish-hook fn))
      ;; hash 位置へジャンプして git-peek 起動
      (git-peek-from-hash--run hash))))

(defun git-peek-from-hash--run (hash)
  "Git-peek のサイドバーを開き、HASH の行にカーソルを移動する."
  (git-peek)
  ;; サイドバーが開いた後に hash 行を探してジャンプ
  (when (get-buffer "*git-peek-commits*")
    (with-current-buffer "*git-peek-commits*"
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (when (re-search-forward (regexp-quote (substring hash 0 7)) nil t)
          (beginning-of-line)
          (git-peek--highlight-current)
          (when (window-live-p git-peek--sidebar-win)
            (set-window-point git-peek--sidebar-win (point)))
          (git-peek--render-preview (git-peek--current-commit)))))))

(provide 'my-tig-bridge)
;;; my-tig-bridge.el ends here
