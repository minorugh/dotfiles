# git-peek.el 開発引き継ぎ書

## 現状
- 現ファイル: `~/.emacs.d/elisp/git-show-file/my-git-show-file.el`
- 次回リネーム: `git-peek.el`（`my-` 削除、関数名も `git-peek` に統一）
- 単独リポジトリで公開予定
- 著作権: Minoru Higuchi and Claude (Anthropic) の共著

## 確定している未実装課題

### 1. git-peek-deleted（削除済みファイル対象）
テストで動作確認済み。現行との差分は2箇所のみ：
```bash
# ファイル一覧取得（現行）
git -C <root> ls-files

# ファイル一覧取得（deleted版）
git -C <root> log --all --diff-filter=D --name-only --format='' | sort -u
```
```bash
# コミット履歴取得（現行）
git -C <root> log --oneline -- <file>

# コミット履歴取得（deleted版）
git -C <root> log --all --oneline -- <file>
```

### 2. 切り替え方式
- **案2**（別コマンド）と**案3**（ivy内 `C-d` で切り替え）を次回実装時に両方試す
- git-peek 起動中は別コマンド呼び出し不可（Emacs の仕様）なので
  「起動中に切り替えたい」なら案3が有利
- mozc との競合対策: `git-peek` 起動時に `mozc-mode -1` のフックを仕込む
  （他の関数でも同様のパターンを使用済み）
- 案3のキーは `C-d`（ミニバッファ内限定なので mozc と干渉しない）
  もし競合するなら `C-t` や `M-d` に変更

### 3. フルパス対応
- 現行の `git ls-files` はリポジトリルートからの相対パスを返す
- `index.html` 等同名ファイルが複数ディレクトリに存在する場合に区別できない
- ivy にフルパスで渡す方式に変更が必要

### 4. diff 表示
- 現行プレビューは「その時点のファイル全体」を表示
- 現在版との `git diff` を表示する方式が実用的
- 実装方針は次回検討

### 5. コンテキスト判定（プリセット）
- 現在バッファでファイルを開いている場合:
  `buffer-file-name` + `:preselect` でファイル選択を自動プリセット
- dired の場合:
  `dired-get-filename` でカーソル下ファイル名をプリセット
- 起動時にコンテキストを判定して分岐

## プレビューウィンドウ
- `display-buffer-in-side-window` で上部固定表示
- 高さは `git-peek-preview-height` 変数で調整（デフォルト 0.8）
- RET 後に `*git-preview*` バッファを自動クリーンアップ

## hydra への登録方針
- キーを増やしたくないので案3（ivy内切り替え）が有力
- 案2の場合は小/大文字で使い分け:
```elisp
  ("g" git-peek         "git-peek")
  ("G" git-peek-deleted "git-peek deleted")
```
## hydra への登録方針（追記）
- 案2と案3を併用する方針で実装する
  - hydra から `g` で git-peek、`G` で git-peek-deleted を直接起動（案2）
  - git-peek 起動中に ivy 内 `C-d` でも deleted に切り替え可能（案3）
  - どちらのアクションでも同じ結果になるよう両方実装する
  
## 公開用ヘッダー（次回作成）
- ファイル名: `git-peek.el`
- 著作権表記: `Copyright (C) 2026 Minoru Higuchi and Claude (Anthropic)`
- GitHub リポジトリ名: `git-peek`
- Qiita 記事と連動（`my:magit-insert-timestamp` の話題も組み込む予定）

## checkdoc 警告（修正済み or 次回対応）
- docstring 2行目のインデント除去
- ピリオド後スペース2つ
- 手元で修正済みのため次回は修正版を貼る

## リポジトリ管理方針

### 配置
- 本体: `~/src/github.com/minorugh/git-peek/`（単独公開リポジトリ）
- elisp からはシンボリックリンクで参照:
```bash
  ln -s ~/src/github.com/minorugh/git-peek \
        ~/.emacs.d/elisp/git-peek
```
- dotfiles の git 管理上はシンボリックリンク自体がコミットされるので二重管理にならない

### 自動化
`~/Dropbox/makefile` の `git-push` ターゲットに1行追加するだけ:
```makefile
git-push:
    $(MAKE) -C ${HOME}/Dropbox/GH git
    $(MAKE) -C ${HOME}/Dropbox/minorugh.com git
    $(MAKE) -C ${HOME}/src/github.com/minorugh/git-peek git
```

各リポジトリの `git` ターゲットは共通なので追加作業はこの1行のみ。

PS:ローカル＆githubリポジトリ作成済み(2026-03-29)
