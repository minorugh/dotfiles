# Cron 設定リファレンス (dotfiles/cron/)

更新日: 2026-04-07

---

## 1. 目的

- メイン機 (P1) のみ cron スクリプトをセットアップして crontab を管理
- 個別スクリプトのリンク作成と crontab の反映を Makefile で自動化
- サブ機 (X250) ではスキップ
- 既存 crontab はバックアップし、dotfiles の crontab で上書き
- xsrv-backup.sh のみ systemd-user timer で管理（cron では SSH エージェントを引き継げないため）

---

## 2. 現在の crontab (参考)

```crontab
# automerge.sh: 句会パスワードの同期・マージ（毎日 23:40）
40 23 * * * /usr/local/bin/automerge.sh >> /tmp/cron.log 2>&1

# autobackup.sh: Dropbox バックアップ一式（毎日 23:50）
50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
```

注: xmodmap のジョブは `~/.xprofile` に移行済み
注: xsrv-backup.sh は systemd-user timer で管理（後述）

---

## 3. 各ジョブ詳細

### 3.1 automerge.sh (23:40)

- 句会ウェブサイトの会員パスワードファイル 4 種を同期・マージしてサーバーに戻す
- 23:30 に句会を締め切るため、23:40 実行

#### 処理概要

- サーバーから4ファイルをダウンロード（rsync）
- mergepasswd.pl で wmember を再生成
- smember = dmember、mmember = wmember にコピー
- バックアップ zip 作成（`~/Dropbox/passwd/backup/passwd_YYYYMMDD.zip`、7日保持）
- 全4ファイルをサーバーへアップロード（rsync）

#### ログ形式

各Stepの結果を1ブロックで出力。エラー時は詳細も追記。

```
[automerge] START: 2026-03-23 23:40:01
[automerge] Step1 rsync dmember: OK
[automerge] Step1 rsync wmember: OK
[automerge] Step1 rsync smember: OK
[automerge] Step1 rsync mmember: OK
[automerge] Step2 merge: OK
[automerge] Step3 copy smember: OK
[automerge] Step3 copy mmember: OK
[automerge] Step4 git: OK
[automerge] Step4 zip: OK
[automerge] Step5 upload dmember: OK
[automerge] Step5 upload wmember: OK
[automerge] Step5 upload smember: OK
[automerge] Step5 upload mmember: OK
[automerge] END: 2026-03-23 23:40:08 (OK)
```

#### パスワードファイル構成

| ファイル | 句会 | 内容 |
|---|---|---|
| dmember.cgi | 毎日句会 | 自動登録 |
| wmember.cgi | 若鮎句会 | dmember全員 + 若鮎固有メンバー |
| smember.cgi | 吟行句会 | dmemberのコピー |
| mmember.cgi | 月例句会 | wmemberのコピー |

mergepasswd.pl の動作: dmember をベースに wmember を毎回ゼロから再生成。重複判定キーはメールアドレス。

---

### 3.2 autobackup.sh (23:50)

- automerge.sh 完了後に `~/Dropbox/Makefile` の各ターゲットを個別に呼び出してバックアップ一式を実行
- ログ: `/tmp/cron.log`

#### ログ形式

各サブタスクの結果を1ブロックで出力。エラー時は詳細も追記。

```
[autobackup] START: 2026-04-06 23:50:01
[autobackup] melpa: OK
[autobackup] git-push (GH+minorugh.com): OK
[autobackup] mattermost: OK
[autobackup] mozc: OK
[autobackup] keyring: OK
[autobackup] gitea: OK
[autobackup] filezilla: OK
[autobackup] thunderbird: OK
[autobackup] END: 2026-04-06 23:50:30 (OK)
```

#### バックアップ対象（~/Dropbox/Makefile より）

| ターゲット | 内容 |
|---|---|
| `melpa` | Emacs ELPAパッケージをrsync + git push（Gitea・Xserver） |
| `git-push` | GH・minorugh.com・git-peek の変更を日次で自動コミット＆push |
| `mattermost-backup` | Mattermostのconfig/data/DBをDropboxにバックアップ |
| `mozc-backup` | P1の `~/.mozc` をDropboxにバックアップ |
| `keyring-backup` | P1の `~/.local/share/keyrings` をDropboxにバックアップ |
| `gitea-backup` | Gitea data を Dropbox にバックアップ |
| `filezilla-backup` | FileZilla設定をDropboxにバックアップ |
| `thunderbird-backup` | P1の `~/.thunderbird` をDropboxにバックアップ |

詳細は `~/Dropbox/Makefile` のコメント参照。

---

### 3.3 xsrv-backup.sh（systemd-user timer: 0,9,12,15,18,21:00）

- xserver 2ドメインのファイルをローカルリポジトリへ rsync + git commit + push を一括実行
- rsync失敗時は `exit 1` で止まり、commitは走らない
- **cron では SSH エージェント（keychain）を引き継げないため systemd-user timer で管理**
- systemd-user はログインセッションの環境を継承するため正常動作する
- ログ: `/tmp/xsrv-backup.log`（実行のたびに上書き、最新1回分のみ保持）

#### バックアップ対象

| ドメイン | xserver 本番 | ローカル |
|---|---|---|
| gospel-haiku.com | `/home/minorugh/gospel-haiku.com/public_html/` | `~/src/github.com/minorugh/xsrv-GH/` |
| minorugh.com | `/home/minorugh/minorugh.com/public_html/` | `~/src/github.com/minorugh/xsrv-minorugh/` |

#### systemd-user 設定ファイル

`dotfiles/.config/systemd/user/` に実体を置き、`~/.config/systemd/user/` からシンボリックリンクで参照。

```
dotfiles/.config/systemd/user/
  xsrv-backup.service
  xsrv-backup.timer
```

#### 緊急停止・制御

xserver トラブル時は `dotfiles/cron/` で `make` を実行するだけで停止できる。

```bash
cd ~/src/github.com/minorugh/dotfiles/cron
make            # 緊急停止（xsrv-backup.timer を stop）
make xsrv-start # 再開
make xsrv-status # 状態確認
make xsrv-log   # ログ表示
make xsrv-run   # 手動で今すぐ1回実行
```

Emacs の hydra から `make -k` 一発で緊急停止できるよう設定済み。

#### ログ形式

```
2026-04-06 13:00:01 [xsrv-backup] START
2026-04-06 13:00:05 rsync done: gospel-haiku.com
2026-04-06 13:00:08 rsync done: minorugh.com
2026-04-06 13:00:08 [xsrv-backup] END
```

#### xsrv-GH の .gitignore

容量超過のため以下を git 管理から除外（ローカルには rsync で保持）:

```
doc/
img/
topimg/
zc/
*.jpg *.jpeg *.gif *.png（大小文字両方）
```

---

## 4. Makefile: cron / systemd-user スクリプト管理

### dotfiles/Makefile の関連ターゲット

```makefile
cron: ## メイン機 (P1) のみ実行: automerge/autobackup のリンク作成 + crontab バックアップ＆反映

xsrv-systemd: ## systemd-user で xsrv-backup を登録・有効化（P1のみ）
```

### dotfiles/cron/Makefile（緊急操作パネル）

```makefile
all: xsrv-stop           # make -k で緊急停止

crontab:                 # crontab をバックアップして反映
xsrv-stop:               # xsrv-backup 緊急停止
xsrv-start:              # xsrv-backup 再開
xsrv-status:             # xsrv-backup 状態確認
xsrv-log:                # xsrv-backup ログ表示
xsrv-run:                # xsrv-backup 今すぐ手動実行
```

---

## 5. cron/ ディレクトリ構成

```
cron/
  README.md              # このファイル
  Makefile               # 緊急操作パネル（make -k で xsrv 緊急停止）
  crontab                # P1 に適用する crontab 本体（Git 管理）
  automerge.sh           # 句会パスワード同期・マージ
  autobackup.sh          # ~/Dropbox/Makefile を呼び出すラッパー
  mattermost-backup.sh   # Mattermost DB バックアップ
  mozc-backup.sh         # Mozc 辞書バックアップ
  thunderbird-backup.sh  # Thunderbird バックアップ
  filezilla-backup.sh    # FileZilla 設定バックアップ
  gitea-backup.sh        # Gitea data バックアップ
  xsrv-backup.sh         # xserver → ローカルへ rsync + git commit + push
```

crontab を更新した場合は以下で Git 管理ファイルに反映すること：

```bash
crontab -l > ${PWD}/cron/crontab
git add cron/crontab && git commit -m "update crontab"
```

---

## 6. 運用のポイント

- `cron` ターゲットは `.PHONY` 指定済みで毎回実行可能
- `automerge`/`autobackup` のリンクは常に上書き
- crontab バックアップは日付付きで `cron/` 配下に保存（`crontab.backup.YYYYMMDD`）
- サブ機では `cron` ターゲットはスキップされ、既存 crontab は保持される
- xsrv-backup.sh は systemd-user timer で管理（cron 不可のため）
- xsrv 緊急停止は `dotfiles/cron/` で `make` または `make -k` を実行
- xsrv の commitログが増えすぎたら `make -f ~/Dropbox/Makefile xsrv-reset` で手動リセット

---

## 7. 推奨手順（新規 PC リストア時）

```bash
cd ~/src/github.com/minorugh/dotfiles
make cron         # P1のみ: 個別スクリプトリンク作成 + crontab バックアップ＆反映
make xsrv-systemd # P1のみ: systemd-user に xsrv-backup を登録・有効化
```

- `make cron` 実行後、`cron/crontab.backup.YYYYMMDD` が生成される
- crontab の内容は `dotfiles/cron/crontab` のものに上書きされる
- `make xsrv-systemd` で service/timer のシンボリックリンク展開 + `systemctl --user enable --now` が走る
