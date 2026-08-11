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

### dropbox-watch.sh
サブ機(x250)専用。サスペンド復帰後にDropbox同期が「同期中…」のまま
固まる問題への対策。`dropbox status`の文字列は固まっていても
「最新の状態」と誤った自己申告をすることがあると判明したため、
ステータス判定には頼らず、cronの実行間隔そのものでサスペンドを検知する
heartbeat方式を採用している。

cron（1分おき、`sleep 30;`を前置）から実行するたびに現在時刻を`heartbeat`
ファイルへ書き込み、前回書き込みからの間隔（gap）が`GAP_THRESHOLD`
（120秒）以上開いていたら「サスペンドがあった」とみなし、`dropbox status`
を確認せず無条件で`pkill -x dropbox; sleep 3; dropbox start -i`を実行する。

2026-08-11、`dropbox-resume-watch.py`（D-Bus即応版、下記参照）と併用開始。
cron側に`sleep 30;`を前置することで、pyが先に処理して`heartbeat`を更新
していればgapが小さく判定されスルーする。pyが失敗した場合のみ、本来の
gap検知で単独フォールバックする（本体ロジックは無改修）。

他のスクリプトと異なり `/usr/local/bin` 等へのリンクは作らず、
`bin/dropbox-watch.sh`をフルパスのまま利用する（cron専用）。

- スクリプト本体はdotfiles管理下（メイン機で編集・git push、サブ機はgit pullのみ）
- cron登録は`dotfiles/cron/crontab.sub`で管理（メイン機で編集→push→サブ機で
  `make cron-update`）。メイン機はサスペンド運用がないため未登録
- ログ: `~/.cache/dropbox-watch.log`
- 状態ファイル: `~/.cache/dropbox-watch.heartbeat`（`dropbox-resume-watch.py`と共有）

### dropbox-resume-watch.py
`dropbox-watch.sh`の補助役。systemd-logindが発する`PrepareForSleep`
シグナルを、D-Busの正規購読機構（`add_signal_receiver`、eavesdropping
方式ではない）で受信し、サスペンド復帰の瞬間に即座にDropboxを再起動する。
`dropbox-watch.sh`は毎分ポーリングのため2分未満の短時間復帰を取りこぼす
弱点があり、それを補うために2026-08-11導入。

再起動処理の完了後に`~/.cache/dropbox-watch.heartbeat`（sh側と共有）を
更新することで、後発のcronジョブに「対応済み」と伝え、二重起動を防ぐ。
watchdog等の自己監視機構は持たない軽量構成（pyが失敗しても、sh側が
通常のgap検知で必ず拾うため）。

`/usr/local/bin`等へのリンクは作らず、systemd --userサービスとして
`~/.config/systemd/user/dropbox-resume-watch.service`から
dotfilesリポジトリ内のパスを直接参照して常駐する。

- 依存パッケージ: `python3-dbus` `python3-gi`（`make dropbox-resume-watch`
  でインストール、多くの場合Dropbox本体・印刷設定等の依存で既に導入済み）
- ログ: `~/.cache/dropbox-watch.log`（sh側と共通）
- 状態ファイル: `~/.cache/dropbox-watch.heartbeat`（sh側と共有）
- サービス管理: `make -C cron dropbox-watch-stop` / `dropbox-watch-start`
- クリーンリストア: `make dropbox-resume-watch`（ルートMakefile、
  `baseinstall`に組込済み。P1は模擬テスト環境として意図的に有効化）

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

### dropbox-watch.sh
サブ機(x250)専用。サスペンド復帰後にDropbox同期が「同期中…」のまま
固まる問題への対策。`dropbox status`の文字列は固まっていても
「最新の状態」と誤った自己申告をすることがあると判明したため、
ステータス判定には頼らず、cronの実行間隔そのものでサスペンドを検知する
heartbeat方式を採用している。

cron（1分おき）から実行するたびに現在時刻を`heartbeat`ファイルへ書き込み、
前回書き込みからの間隔（gap）が`GAP_THRESHOLD`（120秒）以上開いていたら
「サスペンドがあった」とみなし、`dropbox status`を確認せず無条件で
`pkill -x dropbox; sleep 3; dropbox start -i`を実行する。

他のスクリプトと異なり `/usr/local/bin` 等へのリンクは作らず、
`bin/dropbox-watch.sh`をフルパスのまま利用する（cron専用）。

- スクリプト本体はdotfiles管理下（メイン機で編集・git push、サブ機はgit pullのみ）
- cron登録はdotfiles管理外。サブ機では`dotfiles/cron/`を経由せず、
  サブ機上で直接`crontab -e`して登録する（メイン機はサスペンド運用がないため未登録）
- ログ: `~/.cache/dropbox-watch.log`
- 状態ファイル: `~/.cache/dropbox-watch.heartbeat`

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
| dropbox-resume-watch.py | `make dropbox-resume-watch` |

