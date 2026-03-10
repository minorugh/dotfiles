# cron設定作業記録

作成日: 2026-03-08

---

## 1. xmodmap の自動実行

### 背景

Debian起動時にstartup設定でxmodmapを実行していたが、ある時点から効かなくなることがあり、その都度手動で実行していた。

### 解決策

crontabで定期実行することにした。

```crontab
* * * * * DISPLAY=:0 /usr/bin/xmodmap /home/minoru/.Xmodmap >> /tmp/xmodmap.log 2>&1
```

### 経緯

- 当初は毎分実行で設定
- 動作確認後、毎分実行は気持ち悪いので毎時に変更
- `$HOME` をフルパス `/home/minoru` に変更（cron環境での展開問題を回避）

### 最終設定

```crontab
0 * * * * DISPLAY=:0 /usr/bin/xmodmap /home/minoru/.Xmodmap >> /tmp/xmodmap.log 2>&1
```

ログは正常時は空ファイル。エラー時のみメッセージが記録される。

---

## 2. パスワードファイルの自動同期・マージ

### 背景

俳句ウェブサイトのネット句会システムにはメンバー登録用パスワードファイルが4種類ある。

| ファイル | 句会 | 登録方法 |
|---|---|---|
| dmember.cgi | 毎日句会 | 参加者が自動登録 |
| wmember.cgi | 若鮎句会 | 参加者が自動登録 |
| smember.cgi | 吟行句会 | dmemberと同内容 |
| mmember.cgi | 月例句会 | wmemberと同内容 |

設置場所:
- サーバー: `xsrv:/home/minorugh/gospel-haiku.com/passwd/`
- ローカル: `~/Dropbox/GH/reg/passwd/`

### ファイル構成の仕様

- 毎日句会メンバー（dmember）は若鮎句会（wmember）にも参加できる
- 若鮎句会専用メンバーはdmemberには登録されていない
- smember = dmemberのコピー（システム衝突回避のため）
- mmember = wmemberのコピー（システム衝突回避のため）

### 方針

サーバー側では操作せず、ローカルで全作業を完結させてからサーバーに同期する。

### 実装スクリプト

**mergepasswd.pl** （`~/Dropbox/GH/reg/passwd/mergepasswd.pl`）

dmemberをwmemberにマージする。動作は以下の通り：

1. dmember.cgiを全件読み込む
2. wmember.cgiから「若鮎固有メンバー」（dmemberにいないメールアドレス）だけ抽出
3. 新wmember.cgi = 若鮎固有メンバー + dmember全員 として書き出す

毎回ゼロから再生成するため、dmemberの増減が自動的にwmemberに反映される。重複判定キーはメールアドレス。

**myjob.sh** （`~/src/github.com/minorugh/dotfiles/cron/myjob.sh` → `/usr/local/bin/myjob.sh`）

```
Step1: サーバーから4ファイルをダウンロード（rsync）
Step2: mergepasswd.pl でwmemberを再生成
Step3: smember = dmember、mmember = wmember にコピー
Step4: バックアップzip作成（~/Dropbox/GH/reg/backup/passwd_YYYYMMDD.zip、90日保持）
Step5: 全4ファイルをサーバーへアップロード（rsync）
```

### cron設定

```crontab
40 23 * * * /usr/local/bin/myjob.sh >> /tmp/myjob.log 2>&1
```

23:30に句会を締め切るため、23:40に実行。

### トラブルシューティング記録

**問題1: ログが出ない（cronジョブが起動しない）**

crontabの行末に余分なシングルクォート `'` が混入していた。削除で解決。

**問題2: cronからrsyncでSSH認証失敗**

`Permission denied (publickey)` エラー。原因は以下の2点：

1. `~/.ssh/config` の `IdentityFile ~/.ssh/id_rsa` の `~` がcron環境で展開されない
   → フルパス `/home/minoru/.ssh/id_rsa` に変更（dotfilesの元ファイルを修正）

2. 秘密鍵にパスフレーズが設定されており、cron環境ではssh-agentが使えない
   → keychainを導入。`~/.zshrc` でコメントアウトされていたkeychain設定を有効化
   → `myjob.sh` の先頭に `source $HOME/.keychain/$HOSTNAME-sh` を追加

**問題3: シェバンなし**

旧`myjob.sh`に `#!/bin/bash` がなく、cron環境で動作が不定だった。新スクリプトで解決済み。

### 最終的なcrontab

```crontab
40 23 * * * /usr/local/bin/myjob.sh >> /tmp/myjob.log 2>&1
0  *  * * * DISPLAY=:0 /usr/bin/xmodmap /home/minoru/.Xmodmap >> /tmp/xmodmap.log 2>&1
```
