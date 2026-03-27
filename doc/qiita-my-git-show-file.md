# Emacs から git の過去ファイルを手軽に取り出す `my:git-show-file`

## はじめに

```text
プロは変更履歴のために git を使う。でも git は「過去のファイルを取り出す保管庫」としても最高のツールだ。
```

みなさん、世代バックアップしてますか？

「念のため上書き前にバックアップ」「`.bak` をつけてコピー」——そういった世代管理、git を使えばもっとスマートにできます。でも実際のところ、「git で管理はしているけど、過去バージョンのファイルを取り出すのが面倒」という人は多いのではないでしょうか。

```bash
git log --oneline          # どのコミット？
git show abc1234:path/to/file > /tmp/file.old  # コマンド長い…
```

これを毎回やるのは億劫です。

そこで Emacs の ivy/counsel を使って、**ファイル選択 → コミット選択 → 保存**を数ステップで完結させる `my:git-show-file` を作りました。

## 完成イメージ

1. `M-x my:git-show-file` を実行
2. ivy でファイルを選択（そのリポジトリの全管理ファイルが一覧表示）
3. ivy でコミットを選択（**そのファイルの変更履歴のみ**表示されるので探しやすい）
4. `~/Dropbox/backup/tmp/YYYYMMDD_ファイル名` に保存
5. dired で保存先が開く

コミット日付がファイル名の先頭に付くので、拡張子が末尾に残り Emacs のメジャーモードも正しく効きます。

## コード

```elisp
(defun my:git-show-file ()
  "過去のコミットからファイルを取り出して ~/Dropbox/backup/tmp/ に保存する."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "git リポジトリが見つかりません")))
         ;; リポジトリの全管理ファイルを一覧
         (files
          (split-string
           (shell-command-to-string
            (format "git -C %s ls-files" root)) "\n" t))
         ;; ファイルを ivy で選択
         (file (ivy-read "ファイルを選択: " files))
         ;; そのファイルの変更履歴のみ取得
         (commits
          (split-string
           (shell-command-to-string
            (format "git -C %s log --oneline -- %s" root file)) "\n" t))
         ;; コミットを ivy で選択
         (commit (ivy-read "コミットを選択: " commits))
         (hash (car (split-string commit " ")))
         ;; コミット日付を取得（concat で % エスケープ問題を回避）
         (date (string-trim
                (shell-command-to-string
                 (concat "git -C " root
                         " show -s --format=%cd --date=format:%Y%m%d "
                         hash))))
         (dest-dir (expand-file-name "~/Dropbox/backup/tmp/"))
         (dest (concat dest-dir date "_" (file-name-nondirectory file))))
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))
    (shell-command
     (format "git -C %s show %s:%s > %s" root hash file dest))
    (dired dest-dir)
    (message "保存しました: %s" dest)))
```

## ポイント解説

### ファイルを先に選ぶ

最初にコミット一覧を出すのではなく、**ファイルを先に選ぶ**のがミソです。

`git log --oneline -- ファイルパス` でそのファイルに関係するコミットだけに絞り込めるので、auto commit でコミットメッセージが無意味でも日付で見当をつけられます。

### コミット日付をファイル名の先頭に付ける

`git show` で取り出したファイルのタイムスタンプは現在時刻になってしまいます。そこでコミット日付をファイル名の先頭に付加することで、いつの版かを保持します。

```
20260217_.muttrc
20260301_00-base.el
```

拡張子が末尾に残るので、Emacs で開いたときにメジャーモードが正しく効くのもメリットです。

### `format` 内の `%` エスケープ問題

`--date=format:%Y%m%d` を Emacs の `format` 関数内で使うと `%Y` などが展開されてしまいます。`concat` で文字列を組み立てることで回避しています。

### カレントディレクトリから自動でリポジトリを検出

`locate-dominating-file` で `.git` を探すので、リポジトリ内のどのディレクトリにいても動作します。dired でリポジトリ内に移動してから実行するだけでOKです。

## 使い方

### 前提
- Emacs + ivy/counsel が使えること
- 対象ファイルが git 管理されていること

### 手順
1. dired でリポジトリ内の任意のディレクトリに移動
2. `M-x my:git-show-file` を実行
3. ファイル名を入力して絞り込み → `Enter`
4. 日付でコミットを絞り込み → `Enter`
5. `~/Dropbox/backup/tmp/` に保存されて dired が開く

## まとめ

`.bak` ファイルや世代バックアップの代わりに git を使っている方には特に便利なコマンドです。「あの頃のあのファイルが欲しい」という用途に、Emacs からシームレスにアクセスできます。

ivy の絞り込みが強力なので、ファイル数やコミット数が多くても快適に使えます。ぜひ試してみてください。

---

> コードと記事は Claude と一緒に作りました。
