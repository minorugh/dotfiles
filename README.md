# dotfiles on Makefile

## スクリーンショット
![Debian12 xfce4 desktop](https://live.staticflickr.com/65535/51395292747_c52f2dc3e8_b.jpg)
![Emacs-30.1](https://live.staticflickr.com/65535/53032684552_3f0767459c_b.jpg)

## 概要

Debian Linux 用の dotfiles です。
[masasam/dotfiles](https://github.com/masasam/dotfiles) を参考に構築しました。

Makefile による自動化を採用しており、環境の再構築・カスタマイズが簡単にできます。
ThinkPad 2台（P1 親機 / X250 サブ機）での運用を想定した分岐処理も含んでいます。

---

## 環境構築の手順

### make 実行前の手動準備

以下の手順は make 実行前に手動で行います。

#### 1. Debian クリーンインストール
インストール USB を netinst iso から作成します（Windows の場合は [Rufus](https://rufus.ie/ja/) を使用）。

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
sudo apt install -y make git nautilus
```

#### 4. Dropbox のインストールと初期設定
Synaptic でリポジトリを設定してからインストールします。

```bash
sudo apt install -y nautilus-dropbox
# メニューから Dropbox を起動して初期設定を完了させる
```

#### 5. GPG 秘密鍵のインポートと dotfiles の展開

> ⚠️ この段階ではまだ SSH が使えないため **HTTPS** でクローンします。

```bash
mkdir -p ~/src/github.com/minorugh
cd ~/src/github.com/minorugh
git clone https://github.com/minorugh/env-import.git
cd env-import
make gpg
make dotfiles
```

`make gpg` でパスフレーズを求められます。
`make dotfiles` は以下を一括実行します：
1. dotfiles を HTTPS でクローン
2. `~/.env_source/` を Dropbox bundle から復元
3. `make baseinstall`（基本環境の構築）

詳細は [env-import](https://github.com/minorugh/env-import) を参照してください。

#### 6. 再起動して SSH に切り替え

```bash
reboot
```

再起動後に SSH 接続を確認してから git remote を切り替えてください。

```bash
ssh -T git@github.com
# → "Hi minorugh! You've successfully authenticated..." と出ればOK

cd ~/src/github.com/minorugh/env-import
make switch-ssh
```

#### 7. シェルを zsh に変更

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
| `make baseinstall` | 基本環境の構築（SSH・パッケージ・keyring など） |
| `make nextinstall` | アプリケーション群のインストール |
| `make env-setup` | `dotfiles/env/` のシンボリックリンク作成（`~/.env_source` から展開） |
| `make emacs-mozc` | Emacs + Mozc のインストール |
| `make keyring` | Gnome keyring の初期化（Dropbox からコピー・全機共通） |
| `make autostart` | GUI起動時の SSH 鍵自動入力・mozc 同期・Emacs/Thunderbird 自動起動＆最小化 |
| `make cron` | P1のみ: automerge/autobackup リンク作成 + crontab バックアップ＆反映 |
| `make xsrv-systemd` | P1のみ: xsrv-backup を systemd-user timer に登録・有効化 |
| `make docker-install` | Docker Engine + Compose のインストール |
| `make docker-setup` | Docker 初期セットアップ（polkit 設定含む） |
| `make polkit` | polkit 認証ダイアログ抑制（Docker用） |
| `make thunderbird` | Thunderbird 設定（Dropbox シンボリックリンク展開） |
| `make filezilla` | FileZilla 設定（keychain ラッパー含む） |
| `make keepassxc` | KeePassXC のインストールと自動起動設定 |
| `make texlive` | TeX Live のインストール（scheme-medium + 日本語） |
| `make latex` | LaTeX 用スクリプト・スタイルファイルのリンク作成 |
| `make emacs-stable` | Emacs 安定版のソースビルド |
| `make emacs-devel` | Emacs 開発版のソースビルド（現在 30.1） |
| `make emacs-toggle` | emacs-toggle スクリプトのシンボリックリンク作成 |

詳細は Makefile 内のコメントを参照してください。

---

## SSH キー・keychain の仕組み

GUI 起動時に `.autostart.sh` が実行され、SSH 鍵の自動入力に加えて Emacs・Thunderbird の自動起動と最小化も行います。

SSH 鍵の自動入力フロー：

1. `secret-tool` で Gnome keyring からパスフレーズを取得
2. `keychain` に渡して `ssh-agent` を起動
3. `.xprofile` 経由で `SSH_AUTH_SOCK` をセッションに伝搬

パスフレーズの登録は P1 で一度だけ行い、Dropbox 経由でサブ機にも反映されます。
`secret-tool store` は両マシンで同時実行しないこと（Dropbox 競合の原因になります）。

---

## cron / systemd-user 管理について

`cron/` ディレクトリで cron 関連ファイルをまとめて管理しています。

### cron で管理するジョブ（P1のみ）

| 時刻 | スクリプト | 内容 |
|---|---|---|
| 23:40 | `automerge.sh` | 句会パスワードの同期・マージ |
| 23:50 | `autobackup.sh` | 各種バックアップ一式 |

### systemd-user timer で管理するジョブ

| 時刻 | スクリプト | 内容 |
|---|---|---|
| 0,9,12,15,18,21:00 | `xsrv-backup.sh` | xserver rsync + git push |

xsrv-backup.sh は cron では SSH エージェント（keychain）を引き継げないため、
ログインセッションの環境を継承できる systemd-user timer で管理しています。

`make cron` は P1 でのみ実行されます（`hostname` による分岐）。
`make xsrv-systemd` で systemd-user への登録・有効化まで自動化されます。

#### 緊急停止

xserver トラブル時は `dotfiles/cron/` で `make` を実行するだけで停止できます。

```bash
cd ~/src/github.com/minorugh/dotfiles/cron
make   # xsrv-backup 緊急停止
```

詳細は `cron/README.md` を参照してください。

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
- `dotfiles/env/` から `~/.env_source/` へのシンボリックで参照（窓として機能）
- 更新時は `cd ~/.env_source && make bundle` を実行

---

## Emacs 設定

詳細は以下を参照してください。

- [https://minorugh.github.io/.emacs.d](https://minorugh.github.io/.emacs.d/)

---

## 更新履歴

| 日付 | 内容 |
|---|---|
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
