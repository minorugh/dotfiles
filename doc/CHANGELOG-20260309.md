# CHANGELOG

## 2026-03-09

### Makefile

- `allinstall` の依存リストから `autologin` を削除
- autologin廃止コメントブロック（136行目付近）を削除
- `myjob` ターゲットと `install` ターゲットの間に空行を追加
- `texlive` ターゲットをフルインストールから scheme-medium + collection-langjapanese に変更（約7GB → 約2GB）
- `texlive-full` ターゲットを新設（旧フルインストール手順を保持）
- `texlive-full` 内の `##` コメントを `#` に修正（make help に誤表示されるのを防止）
- sudoers編集手順の `sudo nano /etc/sudoers` を `visudo` に変更（netinstall直後はsudoが未設定のため使用不可）
- `cpenv` ターゲットを削除（dotfilesにbackupディレクトリが存在しない残骸）
- `filezilla` ターゲットに `fzilla-gh.sh` と `fzilla-gh.desktop` のシンボリックリンク作成を追加

### .autostart.sh

- 冒頭に `pkill ssh-agent && eval $(ssh-agent -s)` を追加（起動時にクリーンなソケットを確保）
- 末尾の `exit` を削除し `source ~/.keychain/$(hostname)-sh` に変更（keychainの環境変数をセッションに反映）

### .xprofile

- GUIアプリ（FileZillaなど）向けに keychain 環境変数の読み込みを追加
  `source ~/.keychain/${HOSTNAME}-sh`

### init.el

- 単独で浮いていた `(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")` を `exec-path-from-shell` の `:config` ブロックに統合しコメントを追加

### bin/fzilla-gh.sh（新規）

- FileZilla を gospel-haiku.com サイトプロファイルで起動するラッパースクリプトを新設
- `.desktop` 経由の起動では `SSH_AUTH_SOCK` が引き継がれないため `source ~/.keychain/${HOSTNAME}-sh` を冒頭に追加

### .local/share/applications/fzilla-gh.desktop（新規）

- `fzilla-gh.sh` を呼び出す `.desktop` ファイルを新設（パネルランチャー用）
