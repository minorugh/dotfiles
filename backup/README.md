# バックアップ設定リファレンス (dotfiles/backup/)

更新日: 2026-07-10

---

## 1. 目的

- メイン機 (P1) の各種データを毎夜 Dropbox へ自動バックアップする
- `autobackup.sh` がオーケストレーターとして `Makefile` の各ターゲットを順次呼び出す
- 個別スクリプトのシンボリックリンク作成は `dotfiles/Makefile` の `autobackup` ターゲットで管理
- シャットダウン等で定時実行が飛んだ場合は、`--check` 機構（日付フラグ方式）により
  翌朝 5-12時の毎時チェックで自動的に補完される（旧 anacron 方式は廃止）

---

## 2. ディレクトリ構成

```
backup/
  README.md              # このファイル
  Makefile               # 夜間自動バックアップ（autobackup.sh から呼び出される）
  autobackup.sh           # オーケストレーター（cron から毎日 23:50 に実行 + --check フォールバック）
  automerge.sh             # 句会パスワード同期・マージ（cron から毎日 23:40 に実行 + --check フォールバック）
  xsrv-backup.sh           # xserver 動的ファイルを Dropbox/GH へ rsync（毎時）
  emacs-trash-sweep.sh     # Emacs ゴミ箱をシステムゴミ箱へ移送（月1回）
  melpa-backup.sh         # ELPAバックアップ（rsync + PAST世代退避 + CHANGELOG出力）
  mozc-backup.sh          # Mozc 辞書バックアップ
  filezilla-backup.sh     # FileZilla 設定バックアップ
  gitea-backup.sh         # Gitea data バックアップ
  abook-backup.sh         # abook アドレス帳バックアップ
```

注: `automerge.sh` / `xsrv-backup.sh` / `emacs-trash-sweep.sh` は元々 `dotfiles/cron/` にあったが、
2026-06-19 に「自動実行されるバックアップ処理本体」として性質が一致するため `backup/` へ統合した。
`cron/` は crontab 設定および手動操作（mente）パネルとしての役割に純化している。
詳細は `dotfiles/cron/README.md` を参照。

---

## 3. autobackup.sh (毎日 23:50 本実行 + 5-12時毎時 --check フォールバック)

`dotfiles/backup/Makefile` の各ターゲットを個別に呼び出してバックアップ一式を実行する。

#### 本実行（cron 23:50、引数なし）

フラグの状態に関わらず無条件に実行する。成功時（全ターゲットエラーなし）のみ、
成功フラグ（`~/.cache/autobackup/last-success`）に当日の日付（YYYYMMDD）を書き込む。

```
[autobackup] START: 2026-04-06 23:50:01
[autobackup] melpa: OK
[autobackup] git-push (GH+minorugh.com): OK
[autobackup] mozc: OK
[autobackup] keyring: OK
[autobackup] zsh-history: OK
[autobackup] gitea: OK
[autobackup] filezilla: OK
[autobackup] abook: OK
[autobackup] readmes: OK
[autobackup] END: 2026-04-06 23:50:30 (OK)
```

#### フォールバック（cron 5-12時 毎時、`--check` 付き）

成功フラグの日付を見て、以下のように判定する。

| フラグの日付 | 動作 |
|---|---|
| 今日 | 何もせず無言で終了（既にその日の分は完了済み） |
| 昨日 | フラグを今日に書き換え、skip ログをだけ出して終了（Skip判定が成功している証拠） |
| それ以外（一昨日以前・未記録） | 通常実行する（本実行が実施されなかった場合の補完） |

「昨日」判定時のログ例（その日最初のチェック時にのみ出力、以降は無言）：

```
[autobackup] skip: 2026-06-20 05:05
```

ログ出力先: `/tmp/cron.log`

---

## 4. automerge.sh (毎日 23:40 本実行 + 5-12時毎時 --check フォールバック)

句会ウェブサイトの会員パスワードファイルを同期・マージしてサーバーに戻す。
`--check` 機構は autobackup.sh と同じ仕組み（成功フラグ: `~/.cache/autobackup/automerge-last-success`）。
詳細は `dotfiles/cron/README.md` の 3.1 を参照。

---

## 5. Makefile ターゲット一覧

手動実行: `make -f ~/src/github.com/minorugh/dotfiles/backup/Makefile <ターゲット名>`

```makefile
melpa:               # ELPAを Dropbox/backup/elpa へ rsync、消えるバージョンはPASTへ世代退避
git-push:            # GH・minorugh.com・xsrv-GH・xsrv-minorugh を日次自動コミット＆push
mozc-backup:         # ~/.mozc を Dropbox にバックアップ
keyring-backup:      # ~/.local/share/keyrings を Dropbox にバックアップ
zsh-history-backup:  # ~/.zsh_history を Dropbox にバックアップ
gitea-backup:        # Gitea data を Dropbox にバックアップ
filezilla-backup:    # FileZilla設定を Dropbox にバックアップ
abook-backup:        # ~/.abook/addressbook を GPG 暗号化して Dropbox にバックアップ
readmes-backup:      # 各所のREADMEをDropbox/READMESにコピー
```

#### バックアップ対象詳細

| ターゲット | バックアップ先 |
|---|---|
| `melpa` | `~/Dropbox/backup/elpa/` へrsyncミラー。消えるバージョンは `PAST/<パッケージ名>/` へ退避（最大5世代）。変更ログを `~/Dropbox/backup/elpa/LOG/` に蓄積 |
| `git-push` | GH・minorugh.com・xsrv-GH・xsrv-minorugh の変更を日次で自動コミット＆push |
| `mozc-backup` | `~/Dropbox/backup/mozc/` |
| `keyring-backup` | `~/Dropbox/backup/keyrings/` |
| `zsh-history-backup` | `~/Dropbox/backup/env/zsh/` |
| `gitea-backup` | `~/Dropbox/backup/gitea/` |
| `filezilla-backup` | `~/Dropbox/backup/filezilla/` |
| `abook-backup` | `~/Dropbox/backup/abook/` |
| `readmes-backup` | `~/Dropbox/READMES/` |

---

## 6. シンボリックリンク管理

`dotfiles/Makefile` の `autobackup` / `automerge` / `emacs-trash` ターゲットで
`/usr/local/bin/` へのリンクを一括作成。

```bash
cd ~/src/github.com/minorugh/dotfiles
make autobackup
make automerge
make emacs-trash
```

---

## 7. 推奨手順（新規 PC リストア時）

```bash
cd ~/src/github.com/minorugh/dotfiles
make autobackup   # autobackup.sh + 各種backup.sh のシンボリックリンク作成
make automerge    # automerge.sh のシンボリックリンク作成
make emacs-trash  # emacs-trash-sweep.sh のシンボリックリンク作成
make cron         # crontab に本実行・フォールバック一式を反映
```
