;;; 02-git.el --- Git tools: tig launchers, git-peek bridge, commit entry. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; commit と push を分ける運用の設定。tig を git 操作の入口にしている。
;;
;; 構成
;;
;;   1. 共通部品          対象ファイルと root の判定
;;   2. commit            elisp/commit-msg.el を遅延ロード
;;   3. tig を開く        tig status / ファイル履歴
;;   4. make の実行       my-make-git / my-make-auto-commit
;;   5. git-peek 連携     tig の E で選んだコミットを git-peek で開く
;;
;; 2. commit
;;   commit メッセージバッファ、dotfiles 用のガード、Claude 連携は
;;   elisp/commit-msg.el にある。tig-git.sh が emacsclient で呼ぶ入口
;;   （my-commit-file-from-tig / my-commit-all-from-tig /
;;   my-commit-reword-from-tig）が最初に呼ばれたときに読み込まれる。
;;
;; 3. tig を開く（どれも新しい端末で開き、tig を終了すると端末も閉じる）
;;   my-git-status  開いているファイル（Dired なら行のファイル）の repo の
;;                  tig status。git 操作の入口（F10、hydra-dired の g）
;;   my-open-tig    開いているファイル（Dired なら行のファイル）の履歴（hydra-dired の t）
;;                  環境変数 TIG_PEEK_FILE に対象ファイルのパスを入れて tig を起動する
;;
;; 4. make の実行
;;   my-make-git   カレントディレクトリの make git を実行する（hydra-dired の ]）。
;;                 メイン機は push のみ、サブ機は端末で pull する。
;;   my-make-auto-commit  dotfiles/git で make auto-commit を実行する（hydra-dired の [）。
;;
;; 5. git-peek 連携
;;   my-git-peek-from-hash  tig の E から tig-git.sh と emacsclient 経由で呼ばれ、
;;                        選んだコミットを git-peek で開く。対象ファイルは、
;;                        my-open-tig が環境変数 TIG_PEEK_FILE で tig に渡す。
;;                        他の画面の tig では TIG_PEEK_FILE が空なので、
;;                        E は警告を出すだけで何もしない（tig-git.sh peek）。
;;                        ~/.config/tig/config:
;;                          bind generic E >…/tig-git.sh peek "%(commit)"
;;
;;; Code:

;; ============================================================
;;  1. 共通部品
;;     対象ファイルと root の判定
;; ============================================================
(defun my-git--target-path ()
  "Return the true path of the file in this buffer or Dired line."
  (let ((path (cond ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
                    (buffer-file-name buffer-file-name))))
    (unless path (user-error "ファイルバッファまたは Dired で実行してください"))
    (file-truename path)))

(defun my-git--root (path)
  "Return the git root of PATH or signal an error."
  (let ((root (locate-dominating-file path ".git")))
    (unless root (user-error "Git 管理下のファイルではありません"))
    (expand-file-name root)))

;; ============================================================
;;  2. commit
;;     elisp/commit-msg.el を、tig からの入口が呼ばれたときに読み込む
;; ============================================================
(leaf commit-msg
  :doc "commit メッセージバッファと Claude 連携"
  :commands my-commit-file-from-tig my-commit-all-from-tig my-commit-reword-from-tig)

;; ============================================================
;;  3. tig を開く
;;     どれも新しい端末で開き、tig を終了すると端末も閉じる
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
                 "--" "bash" "-c" (concat "exec " command)))

(defun my-git-status ()
  "Open `tig status' for the repository of the current file or Dired line."
  (interactive)
  (let ((root (my-git--root (my-git--target-path))))
    (my-git--terminal root "tig status")))

(defun my-open-tig ()
  "Open `tig' for the file of the current buffer or Dired line.
TIG_PEEK_FILE carries the file path to tig so that E opens `git-peek'."
  (interactive)
  (let* ((path (my-git--target-path))
         (root (my-git--root path)))
    (my-git--terminal root (format "env TIG_PEEK_FILE=%s tig %s"
                                   (shell-quote-argument path)
                                   (shell-quote-argument path)))))

;; ============================================================
;;  4. make の実行
;;     make git（メイン機は push、サブ機は pull）と make auto-commit
;; ============================================================
(defun my-make-git ()
  "Run `make git' in the current directory.
The main machine pushes in a compile buffer.  A sub machine pulls in a
terminal because env-sync asks for confirmation."
  (interactive)
  (if my-main-machine-p
      (compile "make git")
    (my-make-run-async default-directory "git")))

(defun my-make-auto-commit ()
  "Run `make auto-commit' in the dotfiles git directory."
  (interactive)
  (my-make "auto-commit" "~/src/github.com/minorugh/dotfiles/git/"))

;; ============================================================
;;  5. git-peek 連携
;;     tig の E で選んだコミットを git-peek で開く
;; ============================================================
(leaf git-peek
  :vc (:url "https://github.com/minorugh/git-peek" :rev :newest)
  :config
  (setq git-peek-sidebar-width 40)
  (setq git-peek-save-dir (expand-file-name "~/tmp/git-peek/"))
  (setq git-peek-show-hash nil)
  :init
  (defun my-git-peek-from-hash (hash file)
    "HASH と FILE を受け取り、FILE を `git-peek' で開く.
tig の E から tig-git.sh と emacsclient 経由で呼ばれることを想定。
xsrv 配下なら `my-git-peek-smart' 相当の処理を自動適用する。"
    (require 'git-peek)
    (find-file file)
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
      (my-git-peek-from-hash--run hash)))

  (defun my-git-peek-from-hash--run (hash)
    "Git-peek のサイドバーを開き、HASH の行にカーソルを移動する."
    (git-peek)
    (when (get-buffer "*git-peek-commits*")
      (with-current-buffer "*git-peek-commits*"
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (when (re-search-forward (regexp-quote (substring hash 0 7)) nil t)
            (beginning-of-line)
            (git-peek--highlight-current)
            (when (window-live-p git-peek--sidebar-win)
              (set-window-point git-peek--sidebar-win (point)))
            (git-peek--render-preview (git-peek--current-commit))))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 02-git.el ends here
