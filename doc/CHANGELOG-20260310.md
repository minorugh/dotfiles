# CHANGELOG 2026-03-10

## 問題
再起動後に FileZilla・upsftp.pl で SSH パスフレーズを毎回求められる。
ターミナルは問題なし。X250（サブ機）セットアップ時に発覚。

## 根本原因
`autostart.desktop` の `Exec=bash $HOME/.autostart.sh` で
`$HOME` が展開されずスクリプトが実行されていなかった。
→ keychain が起動せず `SSH_AUTH_SOCK` のソケットが存在しない状態になっていた。

P1 では別の経路（.zshrc 経由）で動いていたため気づかなかった。

## 修正内容

### autostart.desktop
`$HOME` をフルパスに変更。

```
# 変更前
Exec=bash $HOME/.autostart.sh

# 変更後
Exec=bash /home/minoru/.autostart.sh
```

### .xprofile
keychain の起動を削除し `.autostart.sh` に一本化。
`${HOSTNAME}` を `$(hostname)` に統一。

```bash
if [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi
dbus-update-activation-environment --systemd SSH_AUTH_SOCK SSH_AGENT_PID
source ~/.keychain/$(hostname)-sh
```

### .autostart.sh
- `.zsh_history` のコピーを削除（`.zshrc` で親機/サブ機分岐済みのため不要）
- keyrings のコピーを `cp -rf` から `cp -a` に変更
  - `-a` はタイムスタンプ・パーミッションを元のまま保持するため、Default_keyring 以外のファイルのタイムスタンプが再起動のたびに更新される問題を解消

```bash
# 変更前
cp -rf ~/Dropbox/backup/keyrings/. ~/.local/share/keyrings/

# 変更後
cp -a ~/Dropbox/backup/keyrings/. ~/.local/share/keyrings/
```

### Makefile（dotfiles）
- `keyring` ターゲットを P1/サブ機で分岐：
  - P1: Dropbox へのシンボリックリンク（従来通り）
  - サブ機: シンボリックリンクが残っていれば削除するだけ（コピーは autostart.sh が担当）
- `emacs-mozc` の `ln -vsfn -rf` を `ln -vsn` に修正（`-rf` は `ln` には不正なオプション）
- 冒頭の英語手順コメントを削除し README.md に移動
- 全ターゲットの `## コメント` を日本語化
- footer の英語版「Customize settings」ブロックを削除（日本語版のみ残す）

```makefile
keyring:
    $(APT) seahorse libsecret-tools
ifeq ($(shell uname -n),P1)
    # 親機: Dropbox/backup/keyrings へのシンボリックリンク（正本）
    test -L ${HOME}/.local/share/keyrings || rm -rf ${HOME}/.local/share/keyrings
    ln -vsfn {${HOME}/Dropbox/backup,${HOME}/.local/share}/keyrings
else
    # サブ機: シンボリックリンクが残っていれば削除するだけ
    # コピーは起動時に autostart.sh が行う（Dropbox競合防止）
    test -L ${HOME}/.local/share/keyrings && rm -f ${HOME}/.local/share/keyrings || true
endif
```

### filezilla.sh
デバッグログを削除、`export SSH_AUTH_SOCK` を追加して完成。

```bash
source ~/.keychain/$(hostname)-sh 2>/dev/null
export SSH_AUTH_SOCK
filezilla "$@" &
```

### README.md
- Makefile 冒頭の手動準備手順を移動し Markdown で整形
- SSH キー・keychain の仕組みのセクションを新設
- make ターゲット一覧を表形式で追加
- 更新履歴を表形式に整理、誤記（Debian10→Debian11）を修正

## 確認済み動作
- P1・X250 両機とも再起動後にパスフレーズなしで動作
  - upsftp.pl、filezilla.sh、Emacs からの FileZilla 起動、すべて正常
- `~/Dropbox/backup/keyrings/` に競合発生なし
- `cp -a` により Default_keyring 以外のタイムスタンプが保持されることを確認

## 教訓
- `autostart.desktop` の `Exec=` では環境変数 `$HOME` が展開されない
- `cp -rf` ではコピー先のタイムスタンプがコピー実行時刻になる。バックアップ系のコピーには `-a` を使う
- `ln` に `-rf` は不正なオプション（`cp`・`rm` 用）。シンボリックリンク作成には `-vsn` で十分
