# .gnupg

GnuPG の設定ファイルを管理するディレクトリです。

## 管理対象

| ファイル | 内容 |
|---|---|
| `gpg-agent.conf` | gpg-agent の設定 |

**設定ファイルのみ管理します。** 鍵や信頼データは絶対に置かないでください。

置いてはいけないもの:

- `private-keys-v1.d/`（秘密鍵）
- `pubring.kbx`、`trustdb.gpg`（公開鍵・信頼データ）
- `openpgp-revocs.d/`（失効証明書）
- パスフレーズ、鍵 ID、メールアドレスなどの個人情報

## 展開方法

Makefile の `init` ターゲットで、`gpg-agent.conf` のみを `~/.gnupg/` にシンボリックリンクします。

```
mkdir -p ${HOME}/.gnupg && ln -vsf ${PWD}/.gnupg/gpg-agent.conf ${HOME}/.gnupg/gpg-agent.conf
```

`.gnupg/` ディレクトリごとのリンクではないので、この README.md が `~/.gnupg/` に現れることはありません。

## gpg-agent.conf の内容

| 設定 | 意味 |
|---|---|
| `pinentry-program /usr/bin/pinentry-curses` | パスフレーズ入力をターミナル上のテキスト画面で行う |
| `default-cache-ttl 3600` | 最後に使ってから 1 時間、パスフレーズを記憶する |
| `max-cache-ttl 28800` | 最初の入力から 8 時間で必ず忘れる |

## pinentry-curses について

Debian では `gpg-agent` パッケージが `pinentry-curses` に依存しているため、Makefile で個別にインストールする必要はありません。

確認:

```
ls -l /usr/bin/pinentry-curses
```

```
apt-cache rdepends --installed pinentry-curses
```

## 設定変更後の反映

```
gpgconf --kill gpg-agent
```

次に gpg を使うときに、gpg-agent が新しい設定で自動的に起動します。

## 動作確認

リンクの確認:

```
ls -l ~/.gnupg/gpg-agent.conf
```

鍵の確認:

```
gpg --list-keys
```

gpg-agent の状態確認:

```
gpgconf --list-dirs agent-socket
```
