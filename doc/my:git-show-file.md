# git 過去ファイル取り出し

## 基本：Emacs から `M-x my:git-show-file`

カレントディレクトリが git リポジトリ内であればどこでも動く。

### 操作の流れ
1. dired でリポジトリ内に移動
2. `M-x my:git-show-file` を実行
3. ivy でファイルを選択（ファイル名で絞り込み可）
4. ivy でコミットを選択（そのファイルの変更履歴のみ表示）
5. `~/Dropbox/backup/tmp/ファイル名_YYYYMMDD-HHMM` に保存
6. dired で `~/Dropbox/backup/tmp/` が開く

### elisp コード（`09-funcs.el`）
```elisp
(defun my:git-show-file ()
  "過去のコミットからファイルを取り出して ~/Dropbox/backup/tmp/ に保存する."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "git リポジトリが見つかりません")))
         (files
          (split-string
           (shell-command-to-string
            (format "git -C %s ls-files" root)) "\n" t))
         (file (ivy-read "ファイルを選択: " files))
         (commits
          (split-string
           (shell-command-to-string
            (format "git -C %s log --oneline -- %s" root file)) "\n" t))
         (commit (ivy-read "コミットを選択: " commits))
         (hash (car (split-string commit " ")))
         (date (string-trim
                (shell-command-to-string
                 (concat "git -C " root
                         " show -s --format=%cd --date=format:%Y%m%d-%H%M "
                         hash))))
         (dest-dir (expand-file-name "~/Dropbox/backup/tmp/"))
         (dest (concat dest-dir (file-name-nondirectory file) "_" date)))
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))
    (shell-command
     (format "git -C %s show %s:%s > %s" root hash file dest))
    (dired dest-dir)
    (message "保存しました: %s" dest)))
```

---

## 補助：tig でコミット内容を確認する

```bash
sudo apt install tig
cd ~/src/github.com/minorugh/リポジトリ名
tig
```

### 基本操作
- `↑↓` でコミット移動
- `Enter` で diff 表示
- `t` でファイルツリー表示
- `q` で戻る／終了

### 注意点
- tig のツリーには git 管理外のファイルも混在して表示される
- 実際に取り出せるのは git 管理されているファイルのみ
- git 管理確認: `git ls-files | grep ファイル名`
