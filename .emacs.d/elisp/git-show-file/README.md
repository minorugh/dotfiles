# my-git-show-file.el

git 管理下のファイルの過去バージョンを ivy で検索・プレビューして
`~/Dropbox/backup/tmp/` に保存するツール。

## 概要

- git リポジトリ内のファイルを一覧表示して選択
- 選択したファイルのコミット履歴を一覧表示
- **カーソル移動に連動してリアルタイムプレビュー**
- `RET` で `YYYYMMDD_ファイル名` 形式で `~/Dropbox/backup/tmp/` に保存

## 必要要件

- Emacs 27.1 以上
- [ivy](https://github.com/abo-abo/swiper)
- git

## インストール

```
~/.emacs.d/elisp/my-git-show-file/my-git-show-file.el
```

に配置し、`init.el` または `inits/` の該当ファイルに以下を追加：

```elisp
(leaf my-git-show-file
  :load-path "~/.emacs.d/elisp/my-git-show-file"
  :require t)
```

## 使い方

1. git リポジトリ配下のバッファまたは dired で `M-x my-git-show-file`
2. ファイルを ivy で選択 → `RET`
3. コミット一覧が表示され、カーソル移動に連動して上部にプレビュー表示
4. 目的のバージョンが見つかったら `RET` で保存
5. 保存先 `~/Dropbox/backup/tmp/` の dired が開く

## カスタマイズ

プレビューウィンドウの高さを用途に応じて調整できます。

```elisp
;; デフォルト: 0.8（画面の80%）
(setq my-git-show-file-preview-height 0.8)
```

| 値    | 用途                                   |
|-------|----------------------------------------|
| `1.0` | ほぼ全画面（dired 等から使うとき）     |
| `0.8` | デフォルト（汎用）                     |
| `0.5` | 上下分割（現在ファイルと対比したいとき）|

## 保存ファイル名の形式

```
~/Dropbox/backup/tmp/YYYYMMDD_filename
```

例: `20260328_init.el`（2026-03-28 のコミット時点の `init.el`）

## 仕組み

- `advice-add` で `ivy-next-line` / `ivy-previous-line` にフックし、
  カーソル移動のたびに `git show` でファイル内容を取得してプレビューバッファを更新
- プレビューは `display-buffer-in-side-window` で上部に固定表示
- `RET` 後はプレビューバッファを自動クリーンアップ
