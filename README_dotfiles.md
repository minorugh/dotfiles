# dotfiles on Makefile

## スクリーンショット
![Debian12 xfce4 desktop](https://live.staticflickr.com/65535/51395292747_c52f2dc3e8_b.jpg)
![Emacs-30.2](https://minorugh.github.io/img/emacs30.2.png)


## 概要

Debian Linux 用の dotfiles です。
[masasam/dotfiles](https://github.com/masasam/dotfiles) を参考に構築しました。

Makefile による自動化を採用しており、環境の再構築・カスタマイズが簡単にできます。
ThinkPad 2台（P1 メイン機 / X250 サブ機）での運用を想定した分岐処理も含んでいます。

---

## 環境構築の手順

### make 実行前の手動準備

以下の手順は make 実行前に手動で行います。

#### 1. Debian クリーンインストール
インストール USB を netinst iso から作成します（Windows の場合は [Rufus](https://rufus.ie/ja/) を使用）。

USBが見当たらない・使えない場合は`~/Dropbox/backup/make-live-usb/debian-live-usb/README.md` を見て新規作成してください。

#### 2. sudoers への登録
root でログインして実行します。

```bash
gpasswd -a ${USER} sudo
visudo
```

`/etc/sudoers` に以下を追加します。

```
# ユーザー権限の設定
root   ALL=(ALL:ALL) ALL
minoru ALL=(ALL:ALL) NOPASSWD: ALL
%sudo  ALL=(ALL:ALL) NOPASSWD: ALL
```

#### 3. ホームディレクトリ配下を英語表記に変更
一般ユーザーでログインして実行します。

```bash
# Debian12 以降は xdg-user-dirs-gtk 不要
LANG=C xdg-user-dirs-gtk-update --force
sudo apt update
sudo apt install -y nautilus
```

（`git`・`make` はこの後の手順で自動的に導入されるため、ここでは入れません）

#### 4. Dropbox のインストール・秘密鍵とdotfilesの復元・SSH鍵生成

> ⚠️ 2026.09.16〜: 秘密鍵の配布方式を GPG個人鍵から共通パスフレーズ方式に、
> SSH鍵を全機共有から機器ごとの新規生成に変更しました。以降の手順は
> すべて [arch-debian-restore](https://github.com/minorugh/arch-debian-restore)
> リポジトリが担当します。

[arch-debian-restore の README](https://github.com/minorugh/arch-debian-restore) の
手順に沿って、以下を完了させてください。

1. Dropboxのインストール・同期完了
2. `make env-restore`（`~/.env_source`・abookの復元）
3. `make ssh-setup`（このマシン専用のSSH鍵を新規生成、GitHub登録）
4. `make dotfiles`（このリポジトリ自体をSSHでclone）

ここまで完了すると、`~/src/github.com/minorugh/dotfiles` にこのリポジトリが
展開された状態になっているので、以降は本README・本Makefileの手順に戻ります。

```bash
cd ~/src/github.com/minorugh/dotfiles
make baseinstall
```

#### 5. シェルを zsh に変更

```bash
chsh -s /usr/bin/zsh
```

---

### make ターゲット一覧

`make help` で利用可能なターゲットの一覧が表示されます。

主なターゲットは以下の通りです。

| ターゲット | 内容 |
|---|---|
| `make all` | `baseinstall` + `nextinstall` を一括実行 |
| `make baseinstall` | 基本環境の構築（パッケージ・keyring・autostart など） |
| `make nextinstall` | アプリケーション群のインストール |
| `make env-setup` | `dotfiles/env/` を bindfs で `~/.env_source` にマウント（新規ファイル自動反映） |
| `make init-sub` | サブ機のみ: dotfiles の git push を封鎖（`git remote set-url --push origin no-push`）。`baseinstall` に含まれる |
| `make keymap` | CapsLock→Ctrl（`/etc/default/keyboard`＋現セッション）・`.Xmodmap`展開 |
| `make emacs-mozc` | Emacs + Mozc のインストール |
| `make keyring` | Gnome keyring の初期化（seahorse の導入と Dropbox からの復元。Debian 機の P1・X250 共通。Arch は別管理） |
| `make tig` | tig の設定展開 |
| `make autostart` | GUI起動時の mozc・keyring の復元、Emacs・neomutt の自動起動＆最小化（SSH 鍵の自動入力は廃止） |
| `make autobackup` | バックアップスクリプト群の `/usr/local/bin/` へのシンボリックリンク作成 |
| `make cron` | P1: automerge/autobackup のリンク作成 + crontab 反映 / サブ機: crontab 反映のみ |
| `make dropbox-watch` | dropbox-watch.service のリンク作成+有効化（サスペンド復帰後のDropbox自動再起動、両機共通） |
| `make night-suspend` | P1のみ: night-suspend.service/timer のリンク作成+有効化（深夜自動サスペンド、復帰は手動） |
| `make docker-install` | Docker Engine + Compose のインストール |
| `make docker-setup` | Docker 初期セットアップ（polkit 設定含む） |
| `make polkit` | polkit 認証ダイアログ抑制（Docker用） |
| `make filezilla` | FileZilla のインストールと設定 + filezilla.sh のリンク作成 |
| `make keepassxc` | KeePassXC のインストールと自動起動設定 |
| `make hugo` | Hugo（extended版）のインストール |
| `make texlive` | TeX Live のインストール（scheme-medium + 日本語） |
| `make latex` | LaTeX 用スクリプト・スタイルファイルのリンク作成 |
| `make emacs-stable` | Emacs 安定版のソースビルド |
| `make emacs-toggle` | emacs-toggle スクリプトのシンボリックリンク作成 + F12ショートカット登録 |
| `make power-menu` | power-menu.sh のリンク作成 + 全角半角ショートカット登録（電源メニュー） |
| `make tile-toggle` | tile-toggle.sh のリンク作成 + F15ショートカット登録（左右タイル切替） |
| `make make-run` | make-run.sh のリンク作成（Emacs経由のmake実行を安全化） |

`github`・`github-dropbox-cleanup`・`github-remote-add`（個人のリポジトリ群のclone・
保険運用）は2026.09.16に [arch-debian-restore](https://github.com/minorugh/arch-debian-restore)
へ移管しました（`github-remote-add` はその後、削除しています）。

詳細は Makefile 内のコメントを参照してください。

---

## SSH 鍵の仕組み（ssh-agent・keychain は使わない）

2026.09.16〜、SSH鍵は全機共有をやめ、**機器ごとに独立した鍵**
（`~/.ssh/id_ed25519_$(hostname)`）を持つ方式に変更しました。鍵の生成・
GitHub登録は [arch-debian-restore](https://github.com/minorugh/arch-debian-restore)
の `ssh-setup` が担当します。

各鍵はパスフレーズを空で生成しているため、`ssh-agent` に載せる必要がありません。
`~/.ssh/config` の `IdentityFile`（`id_ed25519_%l`）が鍵ファイルを直接使います。
keychain は 2026.09.30〜10.01 に全廃し、P1・X250・Let's note（Arch）の3台で確認しました。

- `.xprofile`: keychain の起動と、`SSH_AUTH_SOCK` を systemd へ伝える行を削除
- `.zshrc`: keychain の節を、ssh-agent は使わない旨の説明に置き換え
- `.autostart.sh`: `pkill ssh-agent` と keychain の起動ブロックをコメントアウト
- `backup/`・`bin/` の6つのスクリプト: `~/.keychain/<ホスト名>-sh` の読み込みを削除

Debian の Xsession が起動時に立てる空の `ssh-agent` は残っていますが、鍵は載せておらず、
害はありません。seahorse（gnome-keyring）は ssh とは別の用途（KeePassXC のマスター
パスワードなど）で使っており、`--components=secrets` だけで動くため、ssh の鍵は扱いません。

動作確認は、`SSH_AUTH_SOCK` を空にして行います。

```bash
SSH_AUTH_SOCK= ssh -o BatchMode=yes -T git@github.com
```

`ssh -T` だけでは、agent の有無は判定できません（agent が死んでいても、鍵ファイルで
通るため）。

FileZilla は `~/.ssh/config` の `IdentityFile` を読みません。agent を使わないため、
FileZilla 側に鍵ファイルの登録が必要です（`編集 → 設定 → 接続 → SFTP → 鍵ファイルを追加`）。
P1・X250 は登録済みです。

各機の鍵は完全に独立しているため、1台が万一侵害されても他機には影響しません。
鍵を紛失・漏洩した場合は、その機の `ssh-setup` を再実行し、GitHub・xserver側で
古い鍵を削除するだけで復旧できます。

---

## キーボード設定（keymap）について

CapsLock→Ctrl・PrtSc→Alt_R・「ろ」キーなどの変換は `make keymap` に集約しています（`/etc/default/keyboard` + `.Xmodmap`）。

xmodmapの設定は稀にXKBリセットで失われることがあるため、以下の2層で保険をかけています。

- **自動**: cron で毎分 `xmodmap ~/.Xmodmap` を再適用（`crontab` 参照）
- **手動**: Emacs の `my-reload-xenv`（以前は `SSH_AUTH_SOCK` の再読込も兼ねていたが、keychain 全廃後は不要）

以前は `keyd`（evdevレベルの変換）も併用していましたが、日本語の「ろ」キー変換に対応できず、xmodmapと機能が重複していたため2026.07.08に廃止しました。

---

## cron 管理について

`cron/` ディレクトリで cron 関連ファイルをまとめて管理しています。

### cron で管理するジョブ（P1のみ）

| 時刻 | スクリプト | 内容 |
|---|---|---|
| 23:40 | `automerge.sh` | 句会パスワードの同期・マージ |
| 23:50 | `autobackup.sh` | 各種バックアップ一式 |
| 0:00 & 07:00〜23:00 毎時 | `xsrv-backup.sh` | xserver 動的ファイルを Dropbox/GH へ rsync |

シャットダウン中にスキップされた場合は `anacron-backup.sh` が起動時に補完する（`/etc/cron.daily/` 経由）。

`make cron` は `hostname` で分岐し、P1 では automerge/autobackup のリンクと `cron/crontab.p1`、サブ機では `cron/crontab.sub` だけを反映します。

#### 緊急停止

xserver トラブル時は `dotfiles/cron/` で `make cron-stop` を実行します。

```bash
cd ~/src/github.com/minorugh/dotfiles/cron
make cron-stop    # xsrv-backup 停止
make cron-start   # xsrv-backup 再開
```

詳細は `cron/README.md` を参照してください。

---

## night-suspend について

P1（メイン機）限定で、深夜01:00に自動サスペンドする仕組みです。

- RTCアラームによる自動復帰はハードウェア/ファームウェアが非対応と判明したため
  断念し、復帰は手動（キー入力／蓋を開ける）運用に確定しています
- 常駐は systemd --user の `night-suspend.timer`（毎晩01:00発火）→
  `night-suspend.service`（`Type=oneshot`）
- 動作確認は `.zshrc` の `ns` 関数で即時実行できます
- `.service` 修正後の反映は `make -C cron night-suspend-reload`
- 旅行等で長期不在にする際は `power-menu.sh` の `5` キー（NIGHT SUSPEND toggle）
  で事前に停止すること

詳細は `bin/README.md`・`cron/README.md` を参照してください。

---

## git 運用について（世代管理）

dotfiles リポジトリ自身の commit / push / pull は `git/` ディレクトリに
実装を分離しています（`docker/` と同様、リストア用のトップレベル
`Makefile` とは関心事を分けるため）。

```bash
make git
make git-fix
```

- `make git`: 変更を auto commit する。P1 は push まで、サブ機は `git pull --ff-only` のみ（push はしない）
- `make git-fix`: サブ機で pull（ff-only）に失敗したときの自動修復。`reset --hard` を使うため dotfiles 専用で、Dropbox 共有のリポジトリ（GH・minorugh.com・arch-debian-restore）には使わない
- サブ機は X250 と Let's note（Arch）。どちらも pull のみで、`init-sub` で push を封鎖している

commit メッセージは `auto: 日時` の機械的な形式に統一し、日々の作業経緯は
別途 changelog 等で記録、git はあくまで世代管理の道具と割り切って運用して
います。過去の差分は [git-peek](https://github.com/minorugh/git-peek) や
`tig` で追跡できます。

詳細（`env-sync` の挙動、P1/サブ機の分岐など）は `git/README.md` を
参照してください。

---

## GitHub private リポジトリの保険運用について

`GH`・`minorugh.com`（private リポジトリ）は、GitHub以外にも自前サーバー・
自前Git GUI（Gitea）へのpushurlを保険として持たせています。この運用自体は
2026.09.16に [arch-debian-restore](https://github.com/minorugh/arch-debian-restore)
へ移管しました。設定用の `github-remote-add` ターゲットは削除済みで、新しいマシンでは
push 先を手動で設定します。考え方・注意点の詳細はそちらのREADMEを参照してください。

---

## 秘密ファイルの管理（~/.env_source）

SSH 鍵・.netrc・.config/hub などの秘密ファイルは `~/.env_source/` で管理します。

```
~/.env_source/
    .ssh/        ← SSH 鍵一式
    .netrc       ← メール認証情報
    .config/hub  ← GitHub トークン
```

- git-crypt 廃止（2026.04.28）に伴い導入
- Dropbox に GPG 暗号化 bundle として保存
- `dotfiles/env/` から `~/.env_source/` へ bindfs でバインドマウントして参照（窓として機能。
  新規ファイルも自動反映され、シンボリックリンクの個別作成は不要）
- マウントは `make env-setup`（初回構築時）と `.autostart.sh`（ログイン毎）が担当
- 更新時は `cd ~/.env_source && make bundle` を実行

サブ機側は `make git`（`git pull --ff-only`）に連動して `env-sync` が自動実行され、
`~/.env_source`（abook を含む）の差分を検知したうえで確認プロンプトを
表示し、同意した場合のみDropbox bundleから同期します。実装や詳細な挙動は
上記「git 運用について（世代管理）」および `git/README.md` を参照してください。

---

## Emacs からの make 実行について

Emacs（`compile`, ivy target picker, hydra-dired 等）から `make` ターゲットを
実行する際は `bin/make-run.sh` を経由します。対話入力（gpgパスフレーズ等）や
破壊的処理を伴う `##!` 付きターゲットは、Emacs 経由の実行に限り自動的に
`gnome-terminal` へ委譲され、安全な場所で実行されます。実行ログは完了後
Emacs 側の `*compilation-log*` バッファに自動で流し込まれます。詳細は
`bin/README.md` を参照してください。

## Emacs 設定

詳細は以下を参照してください。

- [https://minorugh.github.io/.emacs.d](https://minorugh.github.io/.emacs.d/)

---

## 更新履歴

| 日付 | 内容 |
|---|---|
| 2026.10.01 | ssh-agent・keychain を全廃し、マシンごとの空パスフレーズ鍵を `IdentityFile` で直接使う方式に統一（`.xprofile`・`.zshrc`・`.autostart.sh`・スクリプト6か所を修正、3台で確認）。サブ機の git pull を `--ff-only` に変更し、archlinux 分岐を削除（git/Makefile）。README の SSH・git・cron・autostart の記述を実態に合わせて修正 |
| 2026.09.16 | github/github-dropbox-cleanup/github-remote-add を arch-debian-restore へ移管（個人のリポジトリ群管理という関心事を一本化。dotfilesはdotfiles自身の中身・シンボリックリンクに専念する） |
| 2026.09.15 | env-import を arch-debian-restore に置き換え。秘密鍵配布をGPG個人鍵の非対称暗号化から共通パスフレーズの対称暗号化に変更、SSH鍵を全機共有からPCごとの新規独立鍵に変更。ssh ターゲットを廃止（baseinstallの依存からも除去）。abookの個別GPGバックアップ（addressbook_*.gpg）を廃止し、~/.env_source本体への統合のみに一本化 |
| 2026.08.29 | GitHub private リポジトリの保険運用（自前サーバー+自前GUI併用）を整理。対象を精査し直し、誤って登録されていたリポジトリのremoteを削除。github/github-dropbox-cleanup/github-remote-addを##!化 |
| 2026.08.29 | github-remote-add の対象を xsrv-GH/xsrv-minorugh → GH/minorugh.com/env-import に修正。xserverの2スペース（Webサーバー鏡 / bare repoの保険）を整理し、private repo保険はGH/minorugh.com/env-importの3つのみに統一。xsrv-GH/xsrv-minorugh・dotfiles・git-peekの誤ったxserver/Gitea pushurlを削除 |
| 2026.08.29 | github-dropbox-cleanup ターゲット追加（GH/minorugh.com は git clone後 .git 以外の作業ファイルを自動削除。実体は~/Dropbox側にあり、gitdirポインタ経由で参照する構成のため） |
| 2026.08.15 | night-suspend導入（深夜自動サスペンド、systemd --user timer、P1のみ）。RTCアラームによる自動復帰はハードウェア非対応のため断念し手動復帰運用に確定。power-menu.shにNIGHT SUSPENDトグル（5キー）追加、旧9キー（SSH minorugh.com）削除。dropbox-watch.plをP1でも有効化し両機共通運用に変更 |
| 2026.07.31 | git/env-sync/git-fix を git/Makefile に分離、トップレベルはラッパー化（リストア用と日常運用用の関心事を分離、git/README.md 新設） |
| 2026.07.31 | env-sync を導入・本番運用確定（サブ機の git pull連動で ~/.env_source・abook の差分を検知し確認のうえ同期。git ターゲットを ##! 化し、Emacs経由の実行でも対話プロンプトが機能するよう修正）。baseinstall/nextinstall の記載漏れを解消（make-run・tig・hugo を追加）。neomutt-bin を廃止し neomutt ターゲットに統合。「対話実行系」セクションの5ターゲットを ##! 化 |
| 2026.07.30 | Emacsからのmake実行を安全化する bin/make-run.sh を導入（##!付きターゲットのみgnome-terminalへ委譲、結果は*compilation-log*バッファへ自動反映）。bin/スクリプトのMakefileターゲットを整理し tile-toggle.sh を新規登録（従来未登録だった）、keepass.sh を keepassxc.sh にリネーム |
| 2026.07.29 | 秘密ファイル管理（dotfiles/env/ → ~/.env_source）をシンボリックリンクから bindfs バインドマウントに移行、新規ファイルの自動反映に対応。env_local を gpg_bundle_key にリネーム |
| 2026.07.10 | Thunderbird 廃止（neomutt+Gmail Webへ移行）、make thunderbird ターゲット・autostart自動起動・thunderbird-backup 一式を削除 |
| 2026.07.08 | keyd を廃止し xmodmap に一本化（CapsLock/PrtSc等のキー変換を keymap ターゲットに集約）、.xprofile の xmodmap 再適用ウォッチャーを廃止（cron毎分実行＋Emacs手動リロードに統一） |
| 2026.06.16 | xsrv-backup を cron に移行、xsrv-systemd ターゲット廃止、cron-stop/cron-start 追加 |
| 2026.06.07 | melpa-backup.sh 追加・autobackup ターゲットに登録、ELPAパッケージ変更ログ蓄積対応 |
| 2026.05.25 | power-menu.sh の Sleep を xset dpms force off に変更（cron スリープ対策）、anacron-backup.sh 追加 |
| 2026.04.29 | git-crypt 廃止・秘密ファイルを ~/.env_source で管理、env-setup ターゲット追加 |
| 2026.04.11 | devilspie 廃止・Emacs/Thunderbird 自動起動を .autostart.sh に統合、xdotool で最小化 |
| 2026.04.10 | emacs-restore を emacs-toggle に変更（F12キーによるEmacs最小化・復元トグル） |
| 2026.04.07 | xsrv-backup を systemd-user timer に移行、xsrv-systemd ターゲット追加、cron/Makefile 緊急操作パネル整備 |
| 2026.03.26 | keyring ターゲットを全機共通コピー方式に統一、autostart.sh の条件分岐を削除 |
| 2026.03.21 | cron ターゲット追加（automerge/autobackup/crontab 管理）、README 全体見直し |
| 2026.03.19 | polkit ターゲット追加（Docker認証ダイアログ抑制、docker-setup に統合） |
| 2026.03.11 | リストア手順を HTTPS clone 対応に修正、SSH 切り替え手順を追加 |
| 2026.03.10 | SSH/keychain 環境を X250 サブ機に対応、autostart.sh・keyring 周りを整理 |
| 2025.03.09 | Debian12 対応クリーンアップ、sxiv→nsxiv 移行メモ追加 |
| 2024.10.01 | Debian12 対応 |
| 2022.09.22 | Debian11 対応 |
| 2021.11.01 | xserver へのリモートリポジトリ追加（同時 Push 対応） |
| 2021.10.11 | 内容整理 |
| 2021.08.26 | Debian11 / Emacs 27.2 対応 |
| 2021.02.20 | Emacs 27.1 対応 |
| 2021.01.29 | mozc 修正 |
| 2021.01.28 | ThinkPad 2台共有対応 |
| 2020.11.10 | 再構築 |
| 2020.10.27 | 初回コミット |
