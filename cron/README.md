# Cron 設定リファレンス (dotfiles/cron/)

更新日: 2026-03-26

---

## 1. 目的

- メイン機 (P1) のみ cron スクリプトをセットアップして crontab を管理
- 個別スクリプトのリンク作成と crontab の反映を Makefile で自動化
- サブ機 (X250) ではスキップ
- 既存 crontab はバックアップし、dotfiles の crontab で上書き

---

## 2. 現在の crontab (参考)

```crontab
# automerge.sh: 句会パスワードの同期・マージ（毎日 23:40）
40 23 * * * /usr/local/bin/automerge.sh >> /tmp/cron.log 2>&1

# autobackup.sh: Dropbox バックアップ一式（毎日 23:50）
50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
```

注: xmodmap のジョブは `~/.xprofile` に移行済み

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

- automerge.sh 完了後に `~/Dropbox/makefile` の各ターゲットを個別に呼び出してバックアップ一式を実行
- ログ: `/tmp/cron.log`

#### ログ形式

各サブタスクの結果を1ブロックで出力。エラー時は詳細も追記。

```
[autobackup] START: 2026-03-23 23:50:01
[autobackup] melpa: OK
[autobackup] git-push (GH+minorugh.com): OK
[autobackup] mattermost: OK
[autobackup] mozc: OK
[autobackup] thunderbird: OK
[autobackup] keyring: OK
[autobackup] END: 2026-03-23 23:50:20 (OK)
```

#### バックアップ対象（~/Dropbox/makefile より）

| ターゲット | 内容 |
|---|---|
| `melpa` | Emacs ELPAパッケージをtar.gzでスナップショット保存（最古世代を自動削除） |
| `git-push` | GH・minorugh.com の変更を日次で自動コミット＆GitHub/Xserverへ同時push |
| `mattermost-backup` | Mattermostのconfig/data/DBをDropboxにバックアップ（7世代保持） |
| `mozc-backup` | P1の `~/.mozc` をDropboxにバックアップ |
| `thunderbird-backup` | P1の `~/.thunderbirdy` をDropboxにバックアップ |
| `keyring-backup` | P1の `~/.local/share/keyrings` をDropboxにバックアップ |

詳細は `~/Dropbox/makefile` のコメント参照。

---

## 4. Makefile: cron スクリプト管理

- 個別スクリプトのリンク作成 + crontab バックアップ → dotfiles crontab 反映
- メイン機 (P1) のみ実行、サブ機 (X250) では呼び出さない

```makefile
.PHONY: cron automerge autobackup

automerge: ## automerge.sh のシンボリックリンク作成（cron自動実行用、P1のみ）
	sudo ln -vsfn ${PWD}/cron/automerge.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/automerge.sh

autobackup: ## autobackup.sh / mattermost-backup.sh / mozc-backup.sh のシンボリックリンク作成（cron自動実行用、P1のみ）
	sudo ln -vsfn ${PWD}/cron/autobackup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/autobackup.sh
	sudo ln -vsfn ${PWD}/cron/mattermost-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/mattermost-backup.sh
	sudo ln -vsfn ${PWD}/cron/mozc-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/mozc-backup.sh
	sudo ln -vsfn ${PWD}/cron/thunderbird-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/thunderbird-backup.sh

cron: ## メイン機 (P1) のみ実行: automerge/autobackup のリンク作成 + crontab バックアップ＆反映
	@if [ "$$(hostname)" = "P1" ]; then \
		$(MAKE) automerge; \
		$(MAKE) autobackup; \
		BACKUP_FILE=${PWD}/cron/crontab.backup.$$(date +%Y%m%d); \
		crontab -l > $$BACKUP_FILE || true; \
		crontab ${PWD}/cron/crontab; \
	fi
```

- `.PHONY` 指定済みのため `make cron` は毎回実行される
- `autobackup` ターゲットは `mattermost-backup.sh` と `mozc-backup.sh` のリンクも同時に作成する

---

## 5. cron/ ディレクトリ構成

```
cron/
  README.md              # このファイル
  crontab                # P1 に適用する crontab 本体（Git 管理）
  automerge.sh           # 句会パスワード同期・マージ
  autobackup.sh          # ~/Dropbox/makefile を呼び出すラッパー
  mattermost-backup.sh   # Mattermost DB バックアップ
  mozc-backup.sh         # Mozc 辞書バックアップ
  thunderbird-backup.sh  # thunderbird バックアップ
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

---

## 7. 推奨手順（新規 PC リストア時）

```bash
git clone <dotfilesリポジトリ>
cd dotfiles
make cron  # P1のみ: 個別スクリプトリンク作成 + crontab バックアップ＆反映
```

- 実行後、`cron/crontab.backup.YYYYMMDD` が生成される
- crontab の内容は `dotfiles/cron/crontab` のものに上書きされる
