# bin/

dotfiles の `bin/` ディレクトリに置かれたシェルスクリプト群です。
各スクリプトは `~/.local/bin/` または `/usr/local/bin/` にシンボリックリンクされ、
Emacs やデスクトップメニューから呼び出されます。

---

## スクリプト一覧

### filezilla.sh
FileZilla を SSH エージェント付きで起動するラッパー。

keychain の `SSH_AUTH_SOCK` を引き継いでから FileZilla を起動することで、
メニュー・Emacs どちらから起動してもパスフレーズなしで SFTP 接続できる。

```bash
~/.local/bin/filezilla.sh → bin/filezilla.sh
```

### neomutt.sh
neomutt メールクライアントを gnome-terminal で起動するラッパー。

添付ファイルの保存先を `~/Downloads` にするため、起動前に `cd ~/Downloads` を実行。
ターミナルは最大化モードで起動。

```bash
/usr/local/bin/neomutt.sh → bin/neomutt.sh
```

### keepass.sh
KeePassXC をパスワードなしで自動起動するラッパー。

`secret-tool` で Gnome keyring からマスターパスワードを取得し、
`--pw-stdin` で KeePassXC に渡すことでパスワード入力を省略。

```bash
/usr/local/bin/keepass.sh → bin/keepass.sh
```

### freerdp.sh
X250 上の Windows にリモートデスクトップ接続するスクリプト。

IP アドレスは変わる場合があるので `cmd → ipconfig` で確認してから変更すること。

```bash
/usr/local/bin/freerdp.sh → bin/freerdp.sh
```

---

## シンボリックリンクの設定

各スクリプトのリンク設定は `Makefile` の対応ターゲットで行います。

| スクリプト | Makefile ターゲット |
|---|---|
| filezilla.sh | `make filezilla` |
| neomutt.sh | `make neomutt` |
| keepass.sh | `make keepassxc` |
| freerdp.sh | `make rdp` |
