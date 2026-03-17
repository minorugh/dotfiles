<h1>Docker 開発・運用環境設計メモ</h1>


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
<h2>Table of Contents</h2>

- [1. 目的](#1-)
- [2. 全体構成（図）](#2-)
- [3. 現在の運用構成](#3-)
- [4. Docker導入の目的](#4-docker)
- [5. 基本構成](#5-)
- [6. 複数サイト構成](#6-)
- [7. Apache CGI / SSI 設定](#7-apache-cgi--ssi-)
- [8. ディレクトリ構成](#8-)
- [9. docker-compose 構成](#9-docker-compose-)
- [10. Makefile 管理](#10-makefile-)
- [11. 全サービス起動](#11-)
- [12. 常時起動方針](#12-)
- [13. compose 分離方針](#13-compose-)
- [14. ポート設計](#14-)
- [15. 構築手順](#15-)
- [16. 設計思想](#16-)
- [17. 環境復元手順](#17-)
  - [1 必要ソフト](#1-)
  - [2 設定取得](#2-)
  - [3 サービス起動](#3-)
  - [4 動作確認](#4-)

<!-- markdown-toc end -->

# 1. 目的

ローカル環境に Docker を利用した開発環境を構築する。

主目的

* Webサイトのローカル動作確認
* CGI / SSI / .htaccess テスト
* 環境の再構築を容易にする

---

# 2. 全体構成（図）

<img width="100%" src="https://raw.githubusercontent.com/minorugh/dotfiles/main/doc/images/web-development.png">
<b>Web Development and Deployment Architecture</b><br>
<sub>2026-03-17</sub>



# 3. 現在の運用構成

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

# 4. Docker導入の目的

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

# 5. 基本構成

```
Dropbox = データ
dotfiles = 設定
Docker   = 実行
Makefile = 管理
```

---

# 6. 複数サイト構成

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

# 7. Apache CGI / SSI 設定

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

# 8. ディレクトリ構成

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

# 9. docker-compose 構成

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

# 10. Makefile 管理

```
gitea:
	cd docker/gitea && docker compose up -d

mattermost:
	cd docker/mattermost && docker compose up -d

httpd:
	cd docker/httpd && docker compose up -d
```

---

# 11. 全サービス起動

```
start: gitea mattermost httpd
```

```
make start
```

---

# 12. 常時起動方針

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

# 13. compose 分離方針

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

# 14. ポート設計

```
httpd       8080
Gitea       3000
Mattermost  8065
```

---

# 15. 構築手順

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

# 16. 設計思想

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

# 17. 環境復元手順

新PCでの復元

## 1 必要ソフト

```
git
docker
docker compose
Dropbox
```

---

## 2 設定取得

```
git clone <dotfiles>
cd dotfiles
```

---

## 3 サービス起動

```
make start
```

---

## 4 動作確認

```
http://localhost:3000   (Gitea)
http://localhost:8065   (Mattermost)
http://localhost:8080   (httpd)
```

すべて正常なら復元完了。
