# Cron 設定リファレンス (dotfiles/crontab用)

更新日: 2026-03-16

---

## 1. 目的

- メイン機 (P1) のみ cron スクリプトをセットアップして crontab を管理  
- 個別スクリプトのリンク作成と crontab の反映を Makefile で自動化  
- サブ機 (x250) ではスキップ  
- 既存 crontab はバックアップし、dotfiles の crontab で上書き  

---

## 2. 現在の crontab (参考)

```crontab
# mutomerge.sh: 句会パスワードの同期・マージ（毎日 23:40）
40 23 * * * /usr/local/bin/automerge.sh >> /tmp/cron.log 2>&1

# autobackup.sh: Dropbox バックアップ一式（毎日 23:50）
50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1

```
注: xmodmap のジョブは `~/.xprofile` に移行済み

----

## 3. 各ジョブ詳細
### 3.1 automerge.sh (23:40)

- 句会ウェブサイトの会員パスワードファイル 4 種を同期・マージしてサーバーに戻す
- 23:30に句会を締め切るため、23:40 実行

#### 処理概要:

- サーバーから4ファイルをダウンロード（rsync）
- mergepasswd.pl で wmember を再生成
- smember = dmember、mmember = wmember にコピー
- バックアップ zip 作成（~/Dropbox/GH/reg/backup/passwd_YYYYMMDD.zip、90日保持）
- 全4ファイルをサーバーへアップロード（rsync）

#### パスワードファイル構成

| ファイル | 句会 | 内容 |
|---|---|---|
| dmember.cgi | 毎日句会 | 自動登録 |
| wmember.cgi | 若鮎句会 | dmember全員 + 若鮎固有メンバー |
| smember.cgi | 吟行句会 | dmemberのコピー |
| mmember.cgi | 月例句会 | wmemberのコピー |

mergepasswd.pl の動作: dmemberをベースにwmemberを毎回ゼロから再生成。重複判定キーはメールアドレス。

----

### 3.2 autobackup.sh (23:50)

- automerge.sh 完了後に Dropbox バックアップ一式を実行
- ログ: `/tmp/autobackup.log`

----

#### バックアップ対象

| ターゲット | 内容 |
|---|---|
| `melpa` | Emacs ELPAパッケージをtar.gzでスナップショット保存 |
| `dotfiles` | dotfilesをXserverへハードリンク方式でインクリメンタルバックアップ |
| `git-commit` | GH・minorugh.com の変更を日次で自動コミット |

詳細は `~/Dropbox/makefile` のコメント参照。
PS:20260315 dotfiles は自動バックアップグループから外した。


## 4. Makefile: cron スクリプト管理

- 個別スクリプトのリンク作成 + crontab バックアップ → dotfiles crontab 反映
- メイン機 (P1) のみ実行
- サブ機 (x250) では呼び出さない

``` makefile
.PHONY: cron automerge autobackup

# 個別スクリプトのリンク作成
automerge: ## automerge.sh のシンボリックリンク作成（メイン機のみ）
	sudo ln -vsfn ${PWD}/cron/automerge.sh /usr/local/bin/automerge.sh
	sudo chmod +x /usr/local/bin/automerge.sh

autobackup: ## autobackup.sh のシンボリックリンク作成（メイン機のみ）
	sudo ln -vsfn ${PWD}/cron/autobackup.sh /usr/local/bin/autobackup.sh
	sudo chmod +x /usr/local/bin/autobackup.sh

# cron ターゲット
cron: ## メイン機 (P1) のみ実行: automerge/autobackup のリンク作成 + crontab バックアップ＆反映
	@if [ "$$(hostname)" = "P1" ]; then \
		$(MAKE) automerge; \
		$(MAKE) autobackup; \
		BACKUP_FILE=${PWD}/cron/crontab.backup.$$(date +%Y%m%d_%H%M%S); \
		crontab -l > $$BACKUP_FILE || true; \
		crontab ${PWD}/cron/crontab; \
	fi

```
- .PHONY を宣言することで、Make は 毎回コマンドを実行

これで make cron を P1 で叩いたときに正しくスクリプト作成＆ crontab 反映されるようになる

----

## 5. 運用のポイント

- cron ターゲットは .PHONY 指定済みで毎回実行可能
- automerge/autobackup のリンクは常に上書き
- crontab バックアップは日付付きで dotfiles 配下に保存
- サブ機では cron ターゲットはスキップされ、既存 crontab は保持される

## 6. 推奨手順 (新規 PC リストア時)

``` bash
git clone <dotfilesリポジトリ>
cd dotfiles
make cron  # P1のみ、個別スクリプトリンク作成 + crontab反映 +バックアップ
```

- 実行後、${PWD}/cron/crontab.backup.YYYYMMDD_HHMMSS が生成される
- crontab の内容は dotfiles/crontab のものに上書きされる


---

