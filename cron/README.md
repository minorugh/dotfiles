# cron 設定リファレンス

更新日: 2026-03-12

---

## 現在のcrontab

```crontab
# xmodmap: キーバインド再適用（毎時0分）
0  *  * * * DISPLAY=:0 /usr/bin/xmodmap /home/minoru/.Xmodmap >> /tmp/xmodmap.log 2>&1

# 句会パスワードの同期・マージ・バックアップ
40 23 * * * /usr/local/bin/myjob.sh >> /tmp/myjob.log 2>&1

# Dropbox バックアップ一式（ELPAスナップショット、dotfilesバックアップ、git自動コミット）
50 23 * * * make -f /home/minoru/Dropbox/makefile >> /tmp/myjob.log 2>&1
```

---

## 各ジョブの詳細

### 1. xmodmap（毎時0分）

ログイン後にxmodmapが無効になることがあるため、毎時cronで再適用する。
ログ `/tmp/xmodmap.log` は正常時は空。エラー時のみメッセージが記録される。

### 2. myjob.sh（23:40）

句会ウェブサイトの会員パスワードファイル4種を同期・マージしてサーバーに戻す。
23:30に句会を締め切るため、23:40に実行。

```
Step1: サーバーから4ファイルをダウンロード（rsync）
Step2: mergepasswd.pl で wmember を再生成
Step3: smember = dmember、mmember = wmember にコピー
Step4: バックアップzip作成（~/Dropbox/GH/reg/backup/passwd_YYYYMMDD.zip、90日保持）
Step5: 全4ファイルをサーバーへアップロード（rsync）
```

スクリプト本体: `~/src/github.com/minorugh/dotfiles/cron/myjob.sh` → `/usr/local/bin/myjob.sh`
ログ: `/tmp/myjob.log`

#### パスワードファイルの構成

| ファイル | 句会 | 内容 |
|---|---|---|
| dmember.cgi | 毎日句会 | 自動登録 |
| wmember.cgi | 若鮎句会 | dmember全員 + 若鮎固有メンバー |
| smember.cgi | 吟行句会 | dmemberのコピー |
| mmember.cgi | 月例句会 | wmemberのコピー |

mergepasswd.pl の動作: dmemberをベースにwmemberを毎回ゼロから再生成。重複判定キーはメールアドレス。

### 3. make -f ~/Dropbox/makefile（23:50）

myjob.sh 完了後に続けてDropboxバックアップ一式を実行。
ログ: `/tmp/myjob.log`（myjob.shと同じファイルに追記）

| ターゲット | 内容 |
|---|---|
| `melpa` | Emacs ELPAパッケージをtar.gzでスナップショット保存 |
| `dotfiles` | dotfilesをXserverへハードリンク方式でインクリメンタルバックアップ |
| `git-commit` | GH・minorugh.com の変更を日次で自動コミット |

詳細は `~/Dropbox/makefile` のコメント参照。

---

## トラブルシューティング記録

**ログが出ない（cronジョブが起動しない）**
crontabの行末に余分なシングルクォート `'` が混入していた。削除で解決。

**cronからrsyncでSSH認証失敗（Permission denied (publickey)）**
2点の修正で解決:
- `~/.ssh/config` の `IdentityFile` を `~` からフルパス `/home/minoru/.ssh/id_rsa` に変更
- keychainを導入し、`myjob.sh` 先頭で `source $HOME/.keychain/$HOSTNAME-sh` を実行

**シェバンなし**
旧 `myjob.sh` に `#!/bin/bash` がなく動作が不定だった。現スクリプトで解決済み。
