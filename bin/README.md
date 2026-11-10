# bin/

dotfiles の `bin/` ディレクトリに置かれたシェルスクリプト群です。
各スクリプトは `~/.local/bin/` または `/usr/local/bin/` にシンボリックリンクされ、
Emacs やデスクトップメニューから呼び出されます。

---

## スクリプト一覧

### changelog-rotate.sh

トップ `README.md` の更新履歴の行のうち、`CHANGELOG.md`（全履歴）に未登録のものを先頭に追記し、README は直近10件に切り詰める。
`git/Makefile` の `prep`（P1 の commit 前処理）から呼ばれる。Emacs の「すべて commit」（tig の `A`）と `nightly` が `prep` を実行する。書式が想定外のときは何もせず警告のみ。

### emacs-toggle
F12キーでEmacsの最小化・復元をトグルするスクリプト。

最小化中のEmacsウィンドウを検出して復元し、表示中なら最小化する。
Xfce4のグローバルショートカット（F12）に登録して使用する。

```bash
/usr/local/bin/emacs-toggle → bin/emacs-toggle
```

### emacs-start.sh
Emacs を最大化状態で起動するラッパー。起動方法を1か所にまとめ、複数の呼び出し元から使う。

`zsh -ic` 経由で起動し、`.zshrc` の環境設定を読み込んだ状態で
`/usr/local/bin/emacs --maximized` を実行する。

```bash
/usr/local/bin/emacs-start.sh → bin/emacs-start.sh
```

`emacsclient` の代替エディタ（`--alternate-editor=emacs-start.sh`）に使われるため、直書きにせずラッパーとして残している。

| 呼び出し元 | 役割 |
|---|---|
| `.autostart.sh` | ログイン時に Emacs を起動する（直後に xdotool で最小化） |
| `.local/share/applications/emacsclient.desktop` | ファイルマネージャなどから開くときの代替エディタ |
| `bin/emacs-toggle` | Emacs のウィンドウがなければ起動する |
| `.emacs.d/inits/80-hydra-navication.el` の `my-restart-emacs` | 終了後に起動し直す |
| `bin/power-menu.sh` | 実行中の Emacs プロセスの検出条件に使う |
| Makefile の `autostart` | `make emacs-start` で `/usr/local/bin` へのリンクを張る |

`emacsclient.desktop` は、`emacs-common` が配る標準版の `--alternate-editor=` を `emacs-start.sh` に変えたコピーで、`make emacs-start` でリンクする。

### dropbox-restart.sh
ログイン時に Dropbox のトレイアイコンが出ない問題への対策。`.autostart.sh` から呼ばれる。

新しい Dropbox（2026-10〜）はトレイに AppIndicator を必須とし、起動時の判定に
失敗するとアイコンが出ない（同期は動く）。ログイン直後は xfce4-panel のトレイが
未準備で、その時点で起動した Dropbox が失敗する（推定）。`dropbox.desktop` は
Dropbox が起動のたびに再生成するため、起動を遅らせる設定は戻される。
そこでパネルのトレイ（StatusNotifier host）の登録を待ち、Dropbox を止め、
停止を確認してから起動し直す。

`dropbox stop` は完了まで数秒かかるため、固定の sleep ではなく `pgrep` で
停止を確認してから `dropbox start -i` を実行する。公式サポート外の
Debian + Xfce4 で必要で、Dropbox 側が修正されたら不要になる。

```bash
/usr/local/bin/dropbox-restart.sh → bin/dropbox-restart.sh
```

- ログ: `~/.cache/dropbox-restart.log`（待ち完了時刻と start の実行が残る。待ち中の ServiceUnknown エラーは想定内）
- クリーンリストア: `make dropbox-restart`（`make autostart` の前提に組込済み）

### neomutt.sh
neomutt メールクライアントを gnome-terminal で起動するラッパー。

添付ファイルの保存先を `~/Downloads` にするため、起動前に `cd ~/Downloads` を実行。
ターミナルは最大化モードで起動。tmux セッション(`mail`)の有無で新規作成/attach/
非表示化(閉じる)をトグルし、蓋閉じ時は外部モニタ側に表示する。

```bash
/usr/local/bin/neomutt.sh → bin/neomutt.sh
```

### keepassxc.sh
KeePassXC をパスワード入力なしで自動起動するラッパー。

`secret-tool` で Gnome keyring からマスターパスワードを取得し、
`--pw-stdin` で KeePassXC に渡すことでパスワード入力を省略。

起動前に `keepassxc-cli` でそのパスワードが kdbx を開けるか確認し、開けないときだけ
更新を案内する（キーリングが空、または kdbx が読めないときは確認せずそのまま起動）。

- P1: 「更新しますか？」ダイアログ（zenity）を出す。新パスワードで kdbx が開けたときだけ
  キーリングに登録し、`keyring-backup` で Dropbox に反映する。
- P1以外: 書き込みはしない。`make keyring` を実行する案内（zenity）だけを出す。

マスターパスワードの変更は P1 でのみ行う。メイン機を変えるときは `MAIN_HOSTNAME` も書き換える。
キーリングが空の機械での初回登録のみ手動で行う:
`secret-tool store --label='keepassxc master' type kdb`

```bash
/usr/local/bin/keepassxc.sh → bin/keepassxc.sh
```

### power-menu.sh
全角/半角キーで起動する Emacs/tmux プロセス管理＋電源メニュー。

`fzf` で選択式のメニューを表示し、以下を1キーで実行する。

- `1`: SLEEP（画面オフのみ・プロセス継続）
- `2`: POWEROFF
- `3`: REBOOT
- `4`: NIGHT SUSPEND（dotfiles の `make nightly` で auto commit と push をして、成功したらサスペンド。失敗したらサスペンドせず Enter 待ちで止まる）
- `5`: XSRV BACKUP toggle（STOP⇔START。`~/.xsrv-backup-stop` の作成・削除）
- `6`: VE（.elc削除 + `~/.emacs.d/` を Vim で開く）
- `7`: SSH XSRV
- `8`: SSH GH
- `9`: docker/httpd のシェルに入る
- `4`〜`9` は P1 のみ。X250 は `1`〜`3` のみ表示される
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

### simplenote.sh
Simplenote を app モードで起動するラッパー。

hydra-dired の `n` から呼ばれる `my-simplenote`（`.emacs.d/inits/80-hydra-navication.el`）が、
Simplenote のウィンドウがないときにこのスクリプトで起動する。ウィンドウがあるときは、
開いているノートを howm の新規メモにコピーしてウィンドウを閉じる。

```bash
/usr/local/bin/simplenote.sh → bin/simplenote.sh
```

### tig-git.sh
tig の status・stage 画面から commit と push などを行うランチャー。`.config/tig/config` の C・A・P（status と stage）と U・R・h（どの画面でも）から `>` で呼ばれる。

| 引数 | キー | 動作 |
|---|---|---|
| `commit FILE` | C | カーソル位置のファイルだけを Emacs の `*commit-msg*` で commit |
| `all` | A | 全変更を `*commit-msg*` で commit（dotfiles では先に `make prep`） |
| `push` | P | 未コミットがなければ `make git` で push |
| `unpushed` | U | 未 push のコミット一覧を tig で開く（0件なら「未 push のコミットはありません」を表示） |
| `reword` | R | 直前のコミットのメッセージを `*commit-msg*` で書き換える（push 済みなら「直前のコミットは push 済みです」を表示） |
| `help` | h | キー一覧を tig で表示 |

commit と all は、`emacsclient` で `02-git.el` の `my-commit-file-from-tig` / `my-commit-all-from-tig` を呼び、`*commit-msg*` が閉じるまで待つ。確定（`C-c C-c`）でもキャンセル（`C-c C-k`）でも、閉じたら開始時の端末ウィンドウを `xdotool` で前面に戻す。`>` により tig が再オープンされ、status が最新になる。

push は、未コミットの変更があれば「未コミットの変更があります。先に commit してください」を tig で表示して終わる（`q` で元の tig に戻る）。なければ `GIT_PAGER=cat` で `make git` を実行し、成功したら止まらず、失敗したときだけ Enter 待ちになる。Emacs は経由しない。

tig は外部コマンドをリポジトリのルートで実行するので、スクリプトは `$PWD` がルートであることを前提にしている。

```bash
依存: emacsclient（Emacs サーバー） xdotool make
```

X11 環境が前提。commit のあと端末ウィンドウを前面に戻す処理に `xdotool` を使うので、`xdotool` が入っていないと、スクリプトが途中で止まる。Wayland セッションでは `xdotool` がウィンドウを操作できないため、動かない可能性が高い。

## シンボリックリンクの設定

各スクリプトのリンク設定は `Makefile` の対応ターゲットで行います。

| スクリプト | Makefile ターゲット |
|---|---|
| emacs-toggle | `make emacs-toggle` |
| emacs-start.sh | `make emacs-start` |
| neomutt.sh | `make neomutt` |
| keepassxc.sh | `make keepassxc` |
| power-menu.sh | `make power-menu` |
| tile-toggle.sh | `make tile-toggle` |
| make-run.sh | `make make-run` |
| dropbox-restart.sh | `make dropbox-restart` |
| simplenote.sh | `make simplenote` |
| tig-git.sh | リンクなし（`config` から `dotfiles/bin/` の絶対パスで呼ぶ） |
