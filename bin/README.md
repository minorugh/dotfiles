# bin/

dotfiles の `bin/` ディレクトリに置かれたシェルスクリプト群です。
各スクリプトは `~/.local/bin/` または `/usr/local/bin/` にシンボリックリンクされ、
Emacs やデスクトップメニューから呼び出されます。

---

## スクリプト一覧

### emacs-toggle
F12キーでEmacsの最小化・復元をトグルするスクリプト。

最小化中のEmacsウィンドウを検出して復元し、表示中なら最小化する。
Xfce4のグローバルショートカット（F12）に登録して使用する。

```bash
/usr/local/bin/emacs-toggle → bin/emacs-toggle
```

### dropbox-watch.pl
サブ機(x250)専用。サスペンド復帰後にDropbox同期が「同期中…」のまま
固まる問題への対策。systemd-logindが発する`PrepareForSleep`シグナルを、
D-Busの正規購読機構（`add_signal_receiver`、eavesdropping方式ではない）
で受信し、復帰の瞬間（false）に即座にDropboxを再起動する。一般ユーザー
権限のsystemd --userサービスとして動作し、sudoは不要。

`dropbox status`の文字列は固まっていても「最新の状態」と誤った自己申告
をすることがあると判明したため、ステータス判定には頼らず、D-Busの
サスペンド/レジューム通知を直接購読する方式を採用している。

以前はcronのgap検知方式（`dropbox-watch.sh`、毎分ポーリング）と併用し、
2分未満の短時間復帰の取りこぼしを補っていたが、実運用でcron側の出番が
一度もなかったこと、両者の二重起動を防ぐためのheartbeat共有ロジックが
かえって複雑さを増していたことから、2026-08-13にsystemd単独運用へ移行。
`dropbox-watch.sh`本体は撤去せず、Gistへバックアップの上、必要であれば
`crontab.sub`に登録行を復活させることでいつでも併用体制に戻せる。

プロセスが死んだ場合はsystemdの`Restart=always`が自動的に再起動する
（`RestartSec=5`）。それ以外の失速パターン（D-Bus購読だけが内部的に
死ぬ等）への対策は、実運用で発生実績がないため見送っている。万一の
最終手段としてEmacsの`my-env-recover`（`<henkan>`→`x`、xmodmap再読込
+ keychain再import + dropbox再起動をまとめて行う手動リカバリ）が
別途控えている。

`/usr/local/bin`等へのリンクは作らず、systemd --userサービスとして
`~/.config/systemd/user/dropbox-watch.service`からdotfilesリポジトリ内の
パスを直接参照して常駐する。

- 依存パッケージ: `libnet-dbus-perl`（`make dropbox-watch` でインストール）
- ログ: `~/.cache/dropbox-watch.log`
- サービス管理: `make -C cron dropbox-watch-stop` / `dropbox-watch-start`
- クリーンリストア: `make dropbox-watch`（ルートMakefile、`baseinstall`に
  組込済み。P1は模擬テスト環境として意図的に有効化）

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
ターミナルは最大化モードで起動。tmux セッション(`mail`)の有無で新規作成/attach/
非表示化(閉じる)をトグルし、蓋閉じ時は外部モニタ側に表示する。

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

### power-menu.sh
全角/半角キーで起動する Emacs/tmux プロセス管理＋電源メニュー。

`fzf` で選択式のメニューを表示し、以下を1キーで実行する。

- `1`: SLEEP（画面オフのみ・プロセス継続）
- `2`: POWEROFF
- `3`: REBOOT
- `4`: XSRV BACKUP toggle（STOP⇔START）
- `5`: CHECK ENV BACKUP（`~/.env_source/check-backup.sh` 実行）
- `6`: VE（.elc削除 + `~/.emacs.d/` を Vim で開く）
- `7`〜`9`, `0`: xsrv/Docker への SSH・コンテナ接続
- `Enter`: 起動中の emacs プロセスを kill

```bash
xfce keyboard shortcut: gnome-terminal --window -- bash -c "power-menu.sh"
```

### tile-toggle.sh
アクティブウィンドウの左右タイル表示をトグルするスクリプト。

`xdotool`/`wmctrl`/`xrandr` でウィンドウの現在位置とモニター構成を判定し、
左タイルなら右へ、それ以外は左へ切り替える。デュアルモニター対応。
F15などのショートカットから呼び出す想定。

```bash
依存: xdotool wmctrl xrandr
```

### make-run.sh
Emacsから `make` ターゲットを安全に実行するための共通ランチャー。

対象ターゲットが Makefile 上で `##!` 付きの場合のみ、かつ Emacs 経由
（`$INSIDE_EMACS` が設定されている）の実行に限り、`gnome-terminal --wait`
で処理を委譲する。対話入力（gpgパスフレーズ等）や破壊的処理を伴う
ターゲットを、Emacsの compile バッファ内で無警戒に実行してしまう事故を
防ぐ。実行ログは一時ファイルに保存され、完了後 `emacsclient` 経由で
Emacs 側の `*compilation-log*` バッファへ流し込まれる（`##>` マーカーに
よる表示制御にも対応）。ターミナルから直接呼んだ場合、および `##!` の
無いターゲットの場合はそのまま `make` を実行するだけで委譲は行わない。

hydra-dired の `my-make`（`c`/`k`/`b`/`m`/`u`/`]`キー）や ivy target
picker（`@`キー、`my-make-ivy-integrated`）の `C-c C-c` から呼ばれる。

```bash
make-run.sh <dir> <target...>
```

## シンボリックリンクの設定

各スクリプトのリンク設定は `Makefile` の対応ターゲットで行います。

| スクリプト | Makefile ターゲット |
|---|---|
| emacs-toggle | `make emacs-toggle` |
| emacs-start.sh | `make emacs-start` |
| filezilla.sh | `make filezilla` |
| neomutt.sh | `make neomutt` |
| keepassxc.sh | `make keepassxc` |
| power-menu.sh | `make power-menu` |
| tile-toggle.sh | `make tile-toggle` |
| make-run.sh | `make make-run` |
| dropbox-watch.pl | `make dropbox-watch` |

