# Docker 開発環境 構築手順

Apache httpd（CGI / SSI 対応）をローカルで動かすための Docker 環境。

| ポート | サイト |
|--------|--------|
| 8080 | GH（gospel-haiku.com） |
| 8081 | minorugh.com |

---

## 1. 必要ソフト

```
git
docker
docker compose
Dropbox
```

Docker のインストールは dotfiles Makefile から行う：

```bash
make docker-install
make docker-setup
```

> `docker-install` 完了後、グループ反映のため一度ログアウトすること。

---

## 2. Dropbox 同期確認

httpd のドキュメントルートと passwd は Dropbox 配下をマウントしている。以下が存在すること：

```
~/Dropbox/GH/
~/Dropbox/minorugh.com/
~/Dropbox/passwd/
```

---

## 3. dotfiles 取得

```bash
git clone <repo>
cd dotfiles/docker
```

---

## 4. Docker 起動確認

```bash
docker --version
docker compose version
```

---

## 5. httpd 起動（ポート 8080 / 8081）

```bash
make httpd
```

確認：

```
http://localhost:8080
http://localhost:8081
```

停止・ログ表示：

```bash
make httpd-down
make httpd-log
```

---

## 6. hosts 設定

`/etc/hosts` に追記：

```
127.0.0.1 gh.local
127.0.0.1 minorugh.local
```

動作確認：

```
http://gh.local:8080
http://minorugh.local:8081
```

CGI / SSI が動作すれば成功。

---

## 7. Makefile ターゲット一覧

`docker/` 直下で実行する。`dotfiles/` 直下からは `make -f docker/Makefile <target>`。

| ターゲット | 内容 |
|------------|------|
| `httpd` | Apache httpd 起動 |
| `httpd-down` | Apache httpd 停止 |
| `httpd-log` | Apache httpd ログ表示 |
| `docker-start` | 全 Docker サービス起動 |
| `docker-stop` | 全 Docker サービス停止 |
| `docker-ps` | 起動中コンテナ一覧 |
