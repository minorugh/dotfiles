# Docker 開発・運用環境設計メモ

## 全体構成

<!-- <img width="100%" src="file:///home/minoru/src/github.com/minorugh/dotfiles/doc/images/docker-architecture.png"> -->
<img width="100%" src="https://raw.githubusercontent.com/minorugh/dotfiles/main/doc/images/docker-architecture.png">

## 目次

1. 目的
2. 現在の運用構成
3. Docker導入の目的
4. 基本構成
5. 複数サイト構成
6. Apache / CGI / SSI 設定
7. Dockerディレクトリ構成
8. docker-compose 構成
9. Makefile 管理
10. 常時起動方針
11. compose 分離方針
12. ポート設計
13. 構築手順
14. ディレクトリ役割
15. 設計思想
16. 環境復元手順

---

# 1. 目的

ローカル環境に Docker を利用した開発環境を構築する。

主目的

* Webサイトのローカル動作確認
* CGI / SSI / .htaccess テスト
* 環境の再構築を容易にする

---

# 2. 現在の運用構成

```text
Dropbox
   │
   ├─ site1
   └─ site2
        ↓
   SFTP upload
        ↓
   本番サーバー
```

技術構成

```
HTML
Perl CGI (.cgi .pl)
SSI (.shtml)
UTF-8
```

---

# 3. Docker導入の目的

Docker を導入することで

```
Dropbox
   ├─ Docker (ローカルテスト)
   └─ SFTP upload
        ↓
      本番
```

という構造にする。

メリット

* upload 前にテスト
* 本番環境を壊さない
* 編集 → 即確認

---

# 4. 基本構成

```
Dropbox = データ
dotfiles = 設定
Docker   = 実行
Makefile = 管理
```

---

# 5. 複数サイト構成

```
Dropbox/
   site1/
   site2/
```

ローカル VirtualHost

```
site1.local
site2.local
```

hosts

```
127.0.0.1 site1.local
127.0.0.1 site2.local
```

---

# 6. Apache CGI / SSI 設定

```
Options +ExecCGI +Includes
AddHandler cgi-script .cgi .pl
AddType text/html .shtml
AddOutputFilter INCLUDES .shtml
AllowOverride All
```

これにより

* CGI
* SSI
* .htaccess

がすべてのディレクトリで動作。

---

# 7. ディレクトリ構成

```
Dropbox/
   site1/
   site2/
   docker-data/
      gitea/
      mattermost/

dotfiles/
   docker/
      gitea/
      mattermost/
      httpd/
   Makefile
```

---

# 8. docker-compose 構成

例

```
services:
  httpd:
    image: httpd:2.4
    ports:
      - "8080:80"
    volumes:
      - ~/Dropbox/site1:/var/www/site1
      - ~/Dropbox/site2:/var/www/site2
```

---

# 9. Makefile 管理

```
gitea:
	cd docker/gitea && docker compose up -d

mattermost:
	cd docker/mattermost && docker compose up -d

httpd:
	cd docker/httpd && docker compose up -d
```

---

# 10. 全サービス起動

```
start: gitea mattermost httpd
```

```
make start
```

---

# 11. 常時起動方針

コンテナは常時起動とする。

対象

```
gitea
mattermost
httpd
```

理由

* 開発環境を常に利用可能
* 起動待ち時間を削減
* サービス統合運用

---

# 12. compose 分離方針

各サービスは独立した compose とする。

```
docker/
   gitea/
   mattermost/
   httpd/
```

理由

* 障害分離
* 個別再起動
* ログ管理

---

# 13. ポート設計

```
httpd       8080
Gitea       3000
Mattermost  8065
```

---

# 14. 構築手順

順序

```
1 Gitea
2 Mattermost
3 httpd
```

各段階で確認

```
起動
再起動
PC再起動後の復元
```

---

# 15. 設計思想

```
設定   → Git (dotfiles)
データ → Dropbox
実行   → Docker
管理   → Makefile
```

目的

* 再構築可能な環境
* PC変更時の迅速復元
* 安定した開発環境

---

# 16. 環境復元手順

新PCでの復元

### 1 必要ソフト

```
git
docker
docker compose
Dropbox
```

---

### 2 設定取得

```
git clone <dotfiles>
cd dotfiles
```

---

### 3 サービス起動

```
make start
```

---

### 4 動作確認

```
http://localhost:3000   (Gitea)
http://localhost:8065   (Mattermost)
http://localhost:8080   (httpd)
```

すべて正常なら復元完了。
