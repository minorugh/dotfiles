# SSH鍵自動入力 設定手順書

## 概要

OS起動時にSSH鍵のパスフレーズを自動入力し、ターミナル・GUIアプリ・Perlスクリプトすべてで
パスフレーズを聞かれないようにする設定。

- `keychain` がSSHエージェントを管理し、一度認証すれば以降は自動
- パスフレーズは `secret-tool`（GNOME Keyring）に安全に保存
- `.autostart.sh` が起動時に `SSH_ASKPASS` 経由で自動入力

---

## 必要なパッケージのインストール

`keychain` は `Makefile` の `PACKAGES` に含まれているため `make install` で自動インストールされる。
`libsecret-tools`（secret-tool）は `make keyring` でインストールされる。

個別にインストールする場合：

```bash
sudo apt install keychain
sudo apt install libsecret-tools
```

---

## GNOME Keyring（keyrings）の構成

`~/.local/share/keyrings` は Dropbox へのシンボリックリンクになっている。

```
~/.local/share/keyrings@ -> /home/minoru/Dropbox/backup/keyrings
```

このシンボリックリンクは `make keyring` で作成される。
Dropbox経由でP1・X250が同じ keyrings を共有しているため、
**P1で一度 `secret-tool store` すれば X250 での登録作業は不要**。

> ⚠️ P1とX250を同時に起動した状態で `secret-tool store` を実行すると
> Dropboxの競合コピーが発生する。同時使用時は注意すること。

---

## パスフレーズの登録（初回のみ・P1で実行）

```bash
secret-tool store --label="SSH key id_rsa" ssh-key id_rsa
```

実行するとパスワード入力を求められるので、SSH鍵のパスフレーズを入力する。

### 登録確認

```bash
secret-tool lookup ssh-key id_rsa
```

パスフレーズが表示されればOK。

### 注意
- パスフレーズのバックアップは **KeePassXC** にも保存すること
- 登録の属性は `ssh-key id_rsa`（`service`/`account` ではない）

---

## `.autostart.sh` の設定

OS起動時に自動実行され、keychain にパスフレーズを自動入力する。
`SSH_ASKPASS` + `secret-tool` 方式を使う（`expect` 方式は廃止）。

```bash
#!/bin/bash
# Reset ssh-agent to ensure clean socket
pkill ssh-agent

if [ ! $(hostname) == "P1" ]; then
    rm -rf $HOME/.mozc
    cp -rf ~/Dropbox/backup/mozc/.mozc ~/
    cp -f ~/Dropbox/backup/shell/.zsh_history ./
fi

# run xmodmap at startup
/usr/bin/zsh -c "sleep 5; /usr/bin/xmodmap $HOME/.Xmodmap"

# SSH key auto-add via SSH_ASKPASS
ASKPASS_SCRIPT=$(mktemp /tmp/askpass.XXXXXX.sh)
echo '#!/bin/bash' > "$ASKPASS_SCRIPT"
echo 'secret-tool lookup ssh-key id_rsa' >> "$ASKPASS_SCRIPT"
chmod +x "$ASKPASS_SCRIPT"
DISPLAY=:0 SSH_ASKPASS="$ASKPASS_SCRIPT" SSH_ASKPASS_REQUIRE=force \
    /usr/bin/keychain --eval --quiet ~/.ssh/id_rsa
rm -f "$ASKPASS_SCRIPT"

# keychainの環境変数をセッションに反映
source ~/.keychain/$(hostname)-sh
```

**ポイント：** `$(hostname)` を使うこと。zsh では `$HOSTNAME` が未設定のため空文字になる。

---

## `.zshrc` の設定

keychain の起動は `.autostart.sh` に一本化し、`.zshrc` では環境変数の読み込みのみ行う。

```bash
# keychain config
[ -f $HOME/.keychain/$HOST-sh ] && source $HOME/.keychain/$HOST-sh
```

> ⚠️ `--noask` オプションは削除すること。付けるとキーが agent に登録されない原因になる。

---

## `.xprofile` の設定

GUIアプリ（FileZillaなど）向けに keychain 環境変数を読み込む。

```bash
# 最終行に追加
source ~/.keychain/$(hostname)-sh
```

---

## GUIアプリ・Perlスクリプトへの対処

ターミナルのシェルには `SSH_AUTH_SOCK` が渡っていても、GUIアプリや Perl スクリプトは
その環境変数を引き継がない。それぞれ個別に対処が必要。

### Perl スクリプト（upsftp.pl, movepdf.pl）

`use` より前の `BEGIN` ブロックで keychain ファイルを読み込む。

```perl
BEGIN {
    my $hn = $ENV{HOSTNAME} || do { chomp(my $h = `hostname -s`); $h };
    my $kc = "$ENV{HOME}/.keychain/$hn-sh";
    if (-f $kc) {
        open(my $fh, '<', $kc) or die "keychain読み込み失敗: $!";
        while (<$fh>) {
            if (/^(\w+)=([^;]+);/) { $ENV{$1} = $2 }
        }
        close($fh);
    }
}

use Net::SFTP::Foreign;
```

`$moreoptions` で明示的にソケットを指定する。

```perl
my $moreoptions = [
    -o => 'StrictHostKeyChecking no',
    -o => "IdentityAgent $ENV{SSH_AUTH_SOCK}",
];
```

### FileZilla（filezilla.sh）

`.desktop` や Emacs からの起動では `SSH_AUTH_SOCK` が引き継がれないため、
ラッパースクリプト経由で起動する。`"$@"` で引数をそのまま渡すことで
`-s` も `--site=...` も両方使える。

```bash
#!/bin/bash
source ~/.keychain/$(hostname)-sh 2>/dev/null
filezilla "$@" &
exit
```

### Emacs（init.el）

`exec-path-from-shell` で `SSH_AUTH_SOCK` を引き継ぎ、
FileZilla の呼び出しはラッパースクリプト経由にする。

```elisp
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(defun fzilla-GH ()
  (interactive)
  (compile "filezilla.sh --site='0/gospel-haiku.com'"))
```

---

## X250（サブ機）での作業手順

```bash
# 1. dotfilesをpull
cd ~/src/github.com/minorugh/dotfiles
git pull

# 2. make keyring（libsecret-toolsインストール＋シンボリックリンク作成）
make keyring
# → keyrings は Dropbox共有のため、secret-tool store は不要

# 3. make autostart（実行権限付与＋autostart.desktop設定）
make autostart

# 4. 設定を反映
source ~/.zshrc

# 5. 再起動して動作確認
```

---

## 動作確認手順

```bash
# autostart を手動実行
bash ~/.autostart.sh

# 環境変数を読み込む
source ~/.keychain/$(hostname)-sh

# 鍵が登録されているか確認
ssh-add -l
# → 鍵のフィンガープリントが表示されればOK
# → "Error connecting to agent" や "The agent has no identities" はNG

# ソケットファイルが実在するか確認
ls -la $SSH_AUTH_SOCK
```

---

## トラブルシューティング

### パスフレーズを聞かれる場合のチェックリスト

| チェック項目 | コマンド | 期待する結果 |
|---|---|---|
| secret-tool に登録済みか | `secret-tool lookup ssh-key id_rsa` | パスフレーズが表示される |
| ソケットファイルが実在するか | `ls -la $SSH_AUTH_SOCK` | ファイルが存在する |
| 鍵が agent に登録済みか | `ssh-add -l` | フィンガープリントが表示される |

### よくある原因

- `secret-tool store` を実行していない → 初回登録を行う
- `secret-tool` の属性が違う → `ssh-key id_rsa`（`service`/`account` ではない）
- `.autostart.sh` が実行されていない → `bash ~/.autostart.sh` で手動実行して確認
- `.zshrc` に `--noask` が残っている → 削除する

---

## 廃止したもの

| 項目 | 理由 |
|---|---|
| `expect` によるパスフレーズ自動入力 | keychain が既存 agent を再利用する際に失敗するため |
| `autologin.sh` / `autologin.desktop` | `autostart.sh` に統合 |
| `.zshrc` の `--noask` オプション | キーが agent に登録されない原因 |
| `secret-tool` の `service`/`account` 属性 | `ssh-key id_rsa` に統一 |
