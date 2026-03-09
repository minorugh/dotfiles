# SSH鍵自動入力_設定手順書

## 概要

OS起動時にSSH鍵のパスフレーズを自動入力し、ターミナルを開くたびに聞かれないようにする設定。
`keychain` がSSHエージェントを管理し、一度認証すれば以降のターミナル起動では聞かれなくなる。
パスフレーズは `secret-tool`（GNOME Keyring）に安全に保存し、`autostart.sh` が起動時に自動入力する。

---

## 必要なパッケージのインストール

`keychain` は `Makefile` の `PACKAGES` に含まれているため `make install` で自動インストールされる。
`libsecret-tools`（secret-tool）は `make keyring` でインストールされる。

個別にインストールする場合：

```bash
sudo apt install keychain
sudo apt install libsecret-tools   # secret-tool が含まれる
```

---

## Makefile の変更内容（2025年）

| ターゲット | 変更内容 |
|---|---|
| `autologin` | 廃止。`autologin.sh` / `autologin.desktop` も不要 |
| `autostart` | `chmod 600` → `chmod +x` に修正。説明文も更新 |
| `keyring` | `libsecret-tools` のインストールを追加。説明コメントを追記 |

---

## GNOME Keyring（keyrings）の構成

`~/.local/share/keyrings` は Dropbox へのシンボリックリンクになっている。

```
~/.local/share/keyrings@ -> /home/minoru/Dropbox/backup/keyrings
```

このシンボリックリンクは `make keyring` で作成される。
Dropbox経由でP1・X250が同じkeyringsを共有しているため、**P1で一度 `secret-tool store` すればX250での登録作業は不要**。

> ⚠️ P1とX250を同時に起動した状態で `secret-tool store` を実行すると
> Dropboxの競合コピーが発生する。同時使用時は注意すること。

---

## パスフレーズ管理の移行

**移行前：** `~/Dropbox/backup/zsh/env.sh` に平文で `$PW` として保存

**移行後：** GNOME Keyring（`secret-tool`）で管理

```bash
# 登録コマンド（P1で一度だけ実行。X250は不要）
secret-tool store --label="SSH key passphrase" service ssh-key account id_rsa
# → パスフレーズの入力を求められるので、SSH鍵のパスフレーズを入力する
```

パスフレーズのバックアップは **KeePassXC** にも保存済み。
`~/Dropbox/backup/zsh/env.sh` の平文パスワードは確認後に削除すること。

---

## `.zshrc` への追記

以下を `~/.zshrc` に追記する。

```bash
# keychain config
/usr/bin/keychain --quiet --noask $HOME/.ssh/id_rsa
source $HOME/.keychain/$HOST-sh
```

追記後、設定を反映させる：

```bash
source ~/.zshrc
```

各行の意味：
- `--quiet --noask` … keychainを起動。`--noask` はパスフレーズを聞かない（autostart.shが自動入力するため）
- `source $HOME/.keychain/$HOST-sh` … keychainが管理するSSHエージェントの情報をシェルに読み込む

---

## `.autostart.sh` の該当部分

OS起動時に自動実行され、keychainにパスフレーズを自動入力する。

```bash
# SSH key auto-add
/usr/bin/expect -c "
set PW [exec secret-tool lookup service ssh-key account id_rsa]
spawn /usr/bin/keychain --eval --quiet /home/minoru/.ssh/id_rsa
expect {
    \"Enter passphrase\" { send \"\$PW\n\"; interact }
    eof {}
}
"
```

差し替え後は実行権限を付与すること（`make autostart` でも設定される）：

```bash
chmod +x ~/.autostart.sh
```

---

## 廃止したファイル

| ファイル | 理由 |
|---|---|
| `~/autologin.sh` | expectでssh-addしていたスクリプト。autostart.shに統合のため廃止 |
| `~/.config/autostart/autologin.desktop` | 上記の自動起動設定。同様に廃止 |

---

## X250（サブ機）での作業手順

GitHubでdotfilesを管理しているため、P1での変更をpushした後：

```bash
# 1. dotfilesをpull
cd ~/src/github.com/minorugh/dotfiles
git pull

# 2. make keyring（libsecret-toolsインストール＋シンボリックリンク作成）
make keyring
# → keyrings は Dropbox共有のため、パスフレーズ登録（secret-tool store）は不要

# 3. make autostart（実行権限付与＋autostart.desktop設定）
make autostart

# 4. .zshrc に keychain の設定が含まれているか確認（git pullで反映済みのはず）
grep -A2 "keychain config" ~/.zshrc

# 5. 設定を反映
source ~/.zshrc

# 6. 再起動して動作確認
```

---

## 動作確認方法

```bash
ssh-add -l
# → 鍵のフィンガープリントが表示されればOK
# → "The agent has no identities." が出たら失敗
```

---

## 備考

- `secret-tool` はGNOME Keyringが起動してから使えるため、デスクトップ環境起動前には実行不可
- `autostart.desktop` 経由で起動するため、タイミングは問題なし
- seahorse（GUIでKeyringを確認するツール）と secret-tool（CLIツール）は別パッケージだが、同じGNOME Keyringのデータを操作する
- KeePassXCにもパスフレーズのバックアップあり
