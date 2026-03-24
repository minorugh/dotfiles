# Docker 開発環境 構築手順


![開発環境構成図](https://minorugh.github.io/img/docker-block-diagram.jpg) 


## 1. 必要ソフト

```
git
docker
docker compose
Dropbox
```

Docker のインストールは dotfiles Makefile から行う：

```bash
make docker-install   # Docker Engine + Compose のインストール
make docker-setup     # データディレクトリ作成 + 権限設定
```

> `docker-install` 完了後、グループ反映のため一度ログアウトすること。

---

## 2. Dropbox 同期確認

`~/Dropbox/backup/` 以下が存在すること：

```
mattermost/
  config/
  data/
  logs/
  plugins/
  db/
gitea/
  git/
  gitea/
  ssh/
```

---

## 3. データ保存場所

| データ | 場所 | 備考 |
|--------|------|------|
| Mattermost config | `~/Dropbox/backup/mattermost/config` | Dropbox管理 |
| Mattermost data | `~/Dropbox/backup/mattermost/data` | Dropbox管理 |
| Mattermost logs | `~/Dropbox/backup/mattermost/logs` | Dropbox管理 |
| Mattermost plugins | `~/Dropbox/backup/mattermost/plugins` | Dropbox管理 |
| Mattermost DB | `~/Dropbox/backup/mattermost/db` | Dropbox管理 |
| DBバックアップ | `~/Dropbox/backup/mattermost/*.sql` `*.tar.gz` | Dropbox管理・7世代保持 |
| Gitea データ | `~/Dropbox/backup/gitea` | Dropbox管理 |

---

## 4. dotfiles 取得

```bash
git clone <repo>
cd dotfiles
```

---

## 5. Docker 起動確認

```bash
docker --version
docker compose version
```

---

## 6. Gitea 起動（ポート 3000）

```bash
make gitea
```

確認：`http://localhost:3000`

---

## 7. Mattermost 起動（ポート 8065）

```bash
make mattermost
```

確認：`http://localhost:8065`

> `make mattermost` は起動前に以下の権限設定を自動実行する：
> ```
> sudo chown -R 2000:2000 ~/Dropbox/backup/mattermost
> ```
> この権限設定を省くとコンテナが `(unhealthy)` になるため必須。

---

## 8. httpd 起動（ポート 8080）

```bash
make httpd
```

確認：`http://localhost:8080`

---

## 9. hosts 設定

`/etc/hosts` に追記：

```
127.0.0.1 site1.local
127.0.0.1 site2.local
```

動作確認：

```
http://site1.local:8080
http://site2.local:8080
```

CGI / SSI が動作すれば成功。

---

## 10. DBバックアップ設定

バックアップスクリプト（cron 自動実行）：

- スクリプト：`dotfiles/cron/mattermost-backup.sh` → `/usr/local/bin/` にシンボリックリンク
- バックアップ先：`~/Dropbox/backup/mattermost/`
- 保持期間：7世代（古いものは自動削除）
- 実行タイミング：`autobackup.sh` 経由で毎日 23:50
- バックアップ内容：`YYYYMMDD.tar.gz`（config+data）と `mattermost_YYYYMMDD.sql`（pg_dump）

手動バックアップ（試運転・緊急時）：

```bash
make mattermost-backup
```

実行後、最新バックアップファイルの一覧を自動表示する。

### リストア手順

> ⚠️ Mattermost が起動した状態でリストアするとテーブルが重複してデータが復元されない。
> 必ず以下の手順で postgres だけ起動した状態でリストアすること。

```bash
# 1. Mattermost のみ停止（postgres は残す）
cd ~/src/github.com/minorugh/dotfiles/docker/mattermost
docker compose stop mattermost

# 2. DB を完全削除して postgres を再起動（空のDBを作らせる）
docker compose stop postgres
sudo rm -rf ~/Dropbox/backup/mattermost/db
docker compose up -d postgres
sleep 10

# 3. 空のDBにリストア
docker exec -i mattermost-postgres psql -U mattermost mattermost \
  < ~/Dropbox/backup/mattermost/mattermost_YYYYMMDD.sql

# 4. Mattermost 起動
docker compose up -d mattermost
```

---

## 11. Mattermost スマホアクセス（自宅WiFi内）

- アクセス先：`http://192.168.10.109:8065`
- 対応：Mattermost Android アプリ（Pixel 8）
- 運用方針：P1 単機運用、自宅WiFi内のみ
- 用途：スマホ↔PC 間のクリップボード代わり
- 通知：HTTP 運用のため未対応（現状許容）

---

## 12. Makefile ターゲット一覧

```bash
make gitea              # Gitea 起動
make gitea-down         # Gitea 停止
make gitea-log          # Gitea ログ表示
make mattermost         # Mattermost 起動（権限設定含む）
make mattermost-down    # Mattermost 停止
make mattermost-log     # Mattermost ログ表示
make mattermost-backup  # DB手動バックアップ
make httpd              # Apache httpd 起動
make httpd-down         # Apache httpd 停止
make httpd-log          # Apache httpd ログ表示
make docker-start       # 全サービス起動
make docker-stop        # 全サービス停止
make docker-ps          # 起動中コンテナ一覧
```
