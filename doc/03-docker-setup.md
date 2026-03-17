# Docker 開発環境 構築手順

## 1 必要ソフト

インストール

```
git
docker
docker compose
Dropbox
```

---

## 2 Dropbox同期

```
~/Dropbox
```

以下が存在すること

```
site1
site2
docker-data
```

---

## 3 dotfiles取得

```
git clone <repo>
cd dotfiles
```

---

## 4 Docker起動確認

```
docker --version
docker compose version
```

---

## 5 Gitea起動

```
make gitea
```

確認

```
http://localhost:3000
```

---

## 6 Mattermost起動

```
make mattermost
```

確認

```
http://localhost:8065
```

---

## 7 httpd起動

```
make httpd
```

確認

```
http://localhost:8080
```

---

## 8 hosts設定

```
127.0.0.1 site1.local
127.0.0.1 site2.local
```

---

## 9 動作確認

```
http://site1.local:8080
http://site2.local:8080
```

CGI / SSI が動作すれば成功。
