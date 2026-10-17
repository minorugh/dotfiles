# bin/

dotfiles の `bin/` ディレクトリに置かれたシェルスクリプト群です。
各スクリプトは `~/.local/bin/` または `/usr/local/bin/` にシンボリックリンクされ、
Emacs やデスクトップメニューから呼び出されます。

---

## スクリプト一覧

### changelog-rotate.sh

トップ `README.md` の更新履歴の行のうち、`CHANGELOG.md`（全履歴）に未登録のものを先頭に追記し、README は直近10件に切り詰める。
`git/Makefile` の `make git`（P1のみ）から `add` の前に呼ばれる。書式が想定外のときは何もせず警告のみ。

### emacs-toggle
F12キーでEmacsの最小化・復元をトグルするスクリプト。

最小化中のEmacsウィンドウを検出して復元し、表示中なら最小化する。
Xfce4のグローバルショートカット（F12）に登録して使用する。

```bash
/usr/local/bin/emacs-toggle → bin/emacs-toggle
```

### emacs-start.sh
Emacs を最大化状態で起動するラッパー。`.autostart.sh` から呼ばれる。

`zsh -ic` 経由で起動し、`.zshrc` の環境設定を読み込んだ状態で
`/usr/local/bin/emacs --maximized` を実行する。

```bash
/usr/local/bin/emacs-start.sh → bin/emacs-start.sh
```

### dropbox-watch.pl
サスペンド復帰後にDropbox同期が「同期中…」のまま固まる問題への対策。
P1（メイン機）・x250（サブ機）の両機で常駐稼働している。
systemd-logindが発する`PrepareForSleep`シグナルを、
D-Busの正規購読機構（`connect_to_signal`、eavesdropping方式ではない）
で受信し、復帰の瞬間（false）に即座にDropboxを再起動する。一般ユーザー
権限のsystemd --userサービスとして動作し、sudoは不要。

P1では`night-suspend.sh`による毎晩のサスペンドからの復帰時にも動作する。
`night-suspend.sh`自体はDropboxの再起動を行わず、復帰検知と再起動は
このスクリプトが別プロセスで担当する。

`dropbox status`の文字列は固まっていても「最新の状態」と誤った自己申告
をすることがあると判明したため、ステータス判定には頼らず、D-Busの
サスペンド/レジューム通知を直接購読する方式を採用している。

プロセスが死んだ場合はsystemdの`Restart=always`が自動的に再起動する
（`RestartSec=5`）。それ以外の失速パターン（D-Bus購読だけが内部的に
死ぬ等）への対策は、実運用で発生実績がないため見送っている。
なお Emacs の `my-env-recover`（`<henkan>`→`x`）は xmodmap の再読込のみで、dropbox の再起動は行わない。

`/usr/local/bin`等へのリンクは作らず、systemd --userサービスとして
`~/.config/systemd/user/dropbox-watch.service`からdotfilesリポジトリ内の
パスを直接参照して常駐する。

- 依存パッケージ: `libnet-dbus-perl`（`make dropbox-watch` でインストール）
- ログ: `~/.cache/dropbox-watch.log`
- サービス管理: `make -C cron dropbox-watch-reload`（`.pl`修正後の反映用）
- クリーンリストア: `make dropbox-watch`（ルートMakefile、`baseinstall`に
  組込済み）

経緯: 元々はx250専用として作成し、2026-08-15以降にP1でも有効化した。

### filezilla.sh
FileZilla をバックグラウンドで起動するラッパー。

agent は使わない。FileZilla は `~/.ssh/config` の `IdentityFile` を読まないため、
設定 → 接続 → SFTP の「鍵ファイルを追加」で `~/.ssh/id_ed25519_<ホスト名>` を登録しておく（P1・x250 は登録済み）。

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

### keepassxc.sh
KeePassXC をパスワードなしで自動起動するラッパー。

`secret-tool` で Gnome keyring からマスターパスワードを取得し、
`--pw-stdin` で KeePassXC に渡すことでパスワード入力を省略。

```bash
/usr/local/bin/keepassxc.sh → bin/keepassxc.sh
```

### night-suspend.sh
P1（メイン機）専用。深夜01:00に自動サスペンドするスクリプト。
`automerge.sh`/`autobackup.sh`/`xsrv-backup.sh`が実行中の場合はサスペンドを
見送る。RTCアラームによる自動復帰はハードウェア/ファームウェアが非対応と
判明したため断念し、復帰は手動（キー入力／蓋を開ける）運用。

systemd --userの`night-suspend.timer`（毎晩01:00発火）→`night-suspend.service`
（`Type=oneshot`）から呼び出される。旅行等で長期不在にする際は
`power-menu.sh`の6キー（NIGHT SUSPEND toggle）で事前に停止すること。

- ログ: `~/.cache/night-suspend.log`
- サービス管理: `make -C cron night-suspend-reload`（`.service`修正後の反映用）
- 手動動作確認: `.zshrc`の`ns`関数（`systemctl --user start night-suspend.service`）

### power-menu.sh
全角/半角キーで起動する Emacs/tmux プロセス管理＋電源メニュー。

`fzf` で選択式のメニューを表示し、以下を1キーで実行する。

- `1`: SLEEP（画面オフのみ・プロセス継続）
- `2`: POWEROFF
- `3`: REBOOT
- `4`: VE（.elc削除 + `~/.emacs.d/` を Vim で開く）
- `5`: XSRV BACKUP toggle（STOP⇔START。`~/.xsrv-backup-stop` の作成・削除）
- `6`: NIGHT SUSPEND toggle（STOP⇔START）
- `7`: PEEK ENV_GPG（`~/.env_source/check-backup.sh` 実行）
- `8`: SSH XSRV
- `9`: SSH GH
- `0`: docker/httpd のシェルに入る
- `4`〜`0` は P1 のみ。X250 は `1`〜`3` のみ表示される
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
| night-suspend.sh | `make night-suspend` |
