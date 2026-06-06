# バックアップ設定リファレンス (dotfiles/backup/)

更新日: 2026-06-06

---

## 1. 目的

- メイン機 (P1) の各種データを毎夜 Dropbox へ自動バックアップする
- `autobackup.sh` がオーケストレーターとして `Makefile` の各ターゲットを順次呼び出す
- 個別スクリプトのシンボリックリンク作成は `dotfiles/Makefile` の `autobackup` ターゲットで管理
- シャットダウン中にスキップされたジョブは `anacron-backup.sh` が起動時に補完する

---

## 2. ディレクトリ構成

```
backup/
  README.md              # このファイル
  Makefile               # 夜間自動バックアップ（autobackup.sh から呼び出される）
  autobackup.sh          # オーケストレーター（cron から毎日 23:50 に実行）
  anacron-backup.sh      # anacron用補完スクリプト（起動後5分以内に実行）
  mozc-backup.sh         # Mozc 辞書バックアップ
  thunderbird-backup.sh  # Thunderbird バックアップ
  filezilla-backup.sh    # FileZilla 設定バックアップ
  gitea-backup.sh        # Gitea data バックアップ
  abook-backup.sh        # abook アドレス帳バックアップ
```

---

## 3. autobackup.sh (毎日 23:50)

`dotfiles/backup/Makefile` の各ターゲットを個別に呼び出してバックアップ一式を実行する。

#### ログ形式

各サブタスクの結果を1ブロックで出力。エラー時は詳細も追記。

```
[autobackup] START: 2026-04-06 23:50:01
[autobackup] melpa: OK
[autobackup] git-push (GH+minorugh.com): OK
[autobackup] mozc: OK
[autobackup] keyring: OK
[autobackup] zsh-history: OK
[autobackup] gitea: OK
[autobackup] filezilla: OK
[autobackup] thunderbird: OK
[autobackup] abook: OK
[autobackup] readmes: OK
[autobackup] END: 2026-04-06 23:50:30 (OK)
```

ログ出力先: `/tmp/cron.log`

---

## 4. anacron-backup.sh（起動時補完）

シャットダウン中にcronがスキップした場合に備え、起動後5分以内に自動実行される。
前回実行から1日以上経過していない場合はスキップされる。

```
/etc/cron.daily/anacron-backup -> dotfiles/backup/anacron-backup.sh
```

---

## 5. Makefile ターゲット一覧

手動実行: `make -f ~/src/github.com/minorugh/dotfiles/backup/Makefile <ターゲット名>`

```makefile
melpa:               # ELPAを Dropbox/backup/elpa へ rsync + git push
git-push:            # GH・minorugh.com を日次自動コミット＆push
mozc-backup:         # ~/.mozc を Dropbox にバックアップ
keyring-backup:      # ~/.local/share/keyrings を Dropbox にバックアップ
zsh-history-backup:  # ~/.zsh_history を Dropbox にバックアップ
gitea-backup:        # Gitea data を Dropbox にバックアップ
filezilla-backup:    # FileZilla設定を Dropbox にバックアップ
thunderbird-backup:  # ~/.thunderbird を Dropbox にバックアップ
abook-backup:        # ~/.abook/addressbook を GPG 暗号化して Dropbox にバックアップ
readmes-backup:      # 各所のREADMEをDropbox/READMESにコピー
```

#### バックアップ対象詳細

| ターゲット | バックアップ先 |
|---|---|
| `melpa` | `~/Dropbox/backup/elpa/` + git push（Gitea・Xserver） |
| `git-push` | GH・minorugh.com の変更を日次で自動コミット＆push |
| `mozc-backup` | `~/Dropbox/backup/mozc/` |
| `keyring-backup` | `~/Dropbox/backup/keyrings/` |
| `zsh-history-backup` | `~/Dropbox/backup/env/zsh/` |
| `gitea-backup` | `~/Dropbox/backup/gitea/` |
| `filezilla-backup` | `~/Dropbox/backup/filezilla/` |
| `thunderbird-backup` | `~/Dropbox/backup/thunderbird/` |
| `abook-backup` | `~/Dropbox/backup/abook/` |
| `readmes-backup` | `~/Dropbox/READMES/` |
---

## 6. シンボリックリンク管理

`dotfiles/Makefile` の `autobackup` ターゲットで `/usr/local/bin/` および `/etc/cron.daily/` へのリンクを一括作成。

```bash
cd ~/src/github.com/minorugh/dotfiles
make autobackup
```

---

## 7. 推奨手順（新規 PC リストア時）

```bash
cd ~/src/github.com/minorugh/dotfiles
make autobackup   # /usr/local/bin/ へのシンボリックリンク作成 + anacron登録
make cron         # crontab に autobackup.sh の実行登録
```
