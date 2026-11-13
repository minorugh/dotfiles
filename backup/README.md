# バックアップ設定リファレンス (dotfiles/backup/)

更新日: 2026-11-13

---

## 1. 目的

- メイン機 (P1) の各種データを毎夜 Dropbox へ自動バックアップする
- `autobackup.sh` がオーケストレーターとして `Makefile` の各ターゲットを順次呼び出す
- 個別スクリプトのシンボリックリンク作成は `dotfiles/Makefile` の `autobackup` ターゲットで管理
- シャットダウンや NIGHT SUSPEND で定時実行が飛んだ場合は、`--check` 機構（成功時刻フラグ方式）により
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
  mozc-backup.sh          # Mozc 辞書バックアップ
  filezilla-backup.sh     # FileZilla 設定バックアップ
```

注: `automerge.sh` / `xsrv-backup.sh` は元々 `dotfiles/cron/` にあったが、
2026-06-19 に「自動実行されるバックアップ処理本体」として性質が一致するため `backup/` へ統合した。
`cron/` は crontab 設定および手動操作（mente）パネルとしての役割に純化している。
詳細は `dotfiles/cron/README.md` を参照。

---

## 3. autobackup.sh (毎日 23:50 本実行 + 5-12時毎時 --check フォールバック)

`dotfiles/backup/Makefile` の各ターゲットを個別に呼び出してバックアップ一式を実行する。

#### 本実行（cron 23:50、引数なし）

フラグの状態に関わらず無条件に実行する。成功時（全ターゲットエラーなし）のみ、
成功フラグ（`~/.cache/autobackup/last-success`）に成功時刻（epoch 秒）を書き込む。

```
[autobackup] START: 2026-04-06 23:50:01
[autobackup] git-push (GH+minorugh.com): OK
[autobackup] mozc: OK
[autobackup] keyring: OK
[autobackup] kdbx-gdrive: OK
[autobackup] zsh-history: OK
[autobackup] filezilla: OK
[autobackup] readmes: OK
[autobackup] gsync: OK
[autobackup] END: 2026-04-06 23:50:30 (OK)
```

#### フォールバック（cron 5-12時 毎時、`--check` 付き）

成功フラグの時刻を、昨夜の本実行の予定時刻（昨日 23:50）と比べて判定する。

| 前回成功の時刻 | 動作 |
|---|---|
| 昨夜 23:50 以降 | skip ログだけ出して終了（昨夜の本実行、または今朝の補完が成功済み） |
| それ以前・未記録 | 通常実行する（本実行が実施されなかった場合の補完） |

skip 分岐ではフラグを書き換えない。フラグを書くのは、実行が成功したときだけ。
以前は日付フラグ方式だったが、毎晩 NIGHT SUSPEND すると、前日朝の補完が書いた日付で
翌朝が skip され、補完が2日に1回になるため、2026-11-12 に成功時刻方式に変えた。

skip のログ例（5-12時の毎時チェックのたびに出る）：

```
[autobackup] skip: 2026-11-13 08:05
```

ログ出力先: `/tmp/cron.log`

---

## 4. automerge.sh (毎日 23:40 本実行 + 5-12時毎時 --check フォールバック)

句会ウェブサイトの会員パスワードファイルを同期・マージしてサーバーに戻す。
`--check` 機構は autobackup.sh と同じ成功時刻方式（成功フラグ: `~/.cache/autobackup/automerge-last-success`）。
判定の基準は、昨夜の本実行の予定時刻（昨日 23:40）。
詳細は `dotfiles/cron/README.md` の 3.1 を参照。

---

## 5. Makefile ターゲット一覧

手動実行: `make -f ~/src/github.com/minorugh/dotfiles/backup/Makefile <ターゲット名>`

```makefile
git-push:            # GH・minorugh.com・xsrv-GH・xsrv-minorugh を日次自動コミット＆push
mozc-backup:         # ~/.mozc を Dropbox にバックアップ
keyring-backup:      # ~/.local/share/keyrings を Dropbox にバックアップ（keepassxc.sh がマスターパスワード更新後にも呼ぶ）
zsh-history-backup:  # ~/.zsh_history を Dropbox にバックアップ
filezilla-backup:    # FileZilla設定を Dropbox にバックアップ
readmes-backup:      # 各所のREADMEをDropbox/READMESにコピー
gsync:              # GitHub と xserver の HEAD が一致しているか確認（4 リポジトリ）
```

#### バックアップ対象詳細

| ターゲット | バックアップ先 |
|---|---|
| `git-push` | GH・minorugh.com・xsrv-GH・xsrv-minorugh の変更を日次で自動コミット＆push |
| `mozc-backup` | `~/Dropbox/backup/mozc/` |
| `keyring-backup` | `~/Dropbox/backup/keyrings/` |
| `zsh-history-backup` | `~/Dropbox/backup/env/zsh/` |
| `filezilla-backup` | `~/Dropbox/backup/filezilla/` |
| `readmes-backup` | `~/Dropbox/READMES/` |
| `gsync` | なし（確認のみ。コピーはしない） |

dotfiles は `git-push` に含めない。dotfiles の auto commit と push は、`git/Makefile` の `git-auto` を
power-menu の NIGHT SUSPEND（`4`）と Emacs の `my-make-git` から呼んで行う（詳細は `git/README.md`）。

#### gsync（GitHub と xserver の同期チェック）

- 対象は GH・minorugh.com・cl・piclink の4リポジトリ（`~/src/github.com/minorugh/GH`、`~/src/github.com/minorugh/minorugh.com`、`~/src/github.com/minorugh/cl`、`~/src/github.com/minorugh/piclink`）
- 各リポジトリで、`origin` の HEAD と、`remote.origin.pushurl` の2つ目（xserver）の HEAD を `git ls-remote` で比べる。前提は、`pushurl` を GitHub と xserver の2本登録していること
- 全部一致なら終了コード 0。ずれ・通信失敗・`pushurl` 未設定のどれかがあれば、そのリポジトリを `NG` と表示して終了コード 1 になる（空と空は一致扱いにしない）
- `autobackup.sh` が最後に `run_target "gsync" gsync` で呼ぶ。NG なら ERROR として `/tmp/cron.log` に残り、成功フラグは更新されない
- 手動では `backup/` で `make gsync`
- 確認するのは HEAD（main）だけ。リポジトリを増やすときは Makefile の `for` の行にパスを足す

---

## 6. シンボリックリンク管理

`dotfiles/Makefile` の `autobackup` / `automerge` ターゲットで
`/usr/local/bin/` へのリンクを一括作成。

```bash
cd ~/src/github.com/minorugh/dotfiles
make autobackup
make automerge
```

---

## 7. 推奨手順（新規 PC リストア時）

```bash
cd ~/src/github.com/minorugh/dotfiles
make autobackup   # autobackup.sh + 各種backup.sh のシンボリックリンク作成
make automerge    # automerge.sh のシンボリックリンク作成
make cron         # crontab に本実行・フォールバック一式を反映
```
