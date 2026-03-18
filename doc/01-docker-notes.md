# Docker を使ったローカル Web テスト環境メモ

## 目次

- [1. 目的](#目的)
- [2. 現在の運用構成](#現在の運用構成)
- [3. Docker導入の目的](#docker導入の目的)
- [4. Dropbox を Web root として使う](#dropbox-を-web-root-として使う)
- [5. 複数サイトの構成](#複数サイトの構成)
- [6. Apache設定（Xserverに近づける）](#apache設定xserverに近づける)
- [7. CGI の注意点](#cgi-の注意点)
  - [Perlパス](#perlパス)
  - [実行権限](#実行権限)
  - [改行コード](#改行コード)
- [8. Dockerfile例](#dockerfile例)
- [9. Dockerディレクトリ構成](#dockerディレクトリ構成)
- [10. docker-compose例](#docker-compose例)
- [11. 運用フロー](#運用フロー)
- [12. Docker導入の難易度](#docker導入の難易度)
- [13. 期待できる効果](#期待できる効果)
- [14. 今後の拡張](#今後の拡張)
- [Docker ローカル環境設計メモ（追補）](#docker-ローカル環境設計メモ追補)
  - [15. コンテナ運用方針](#コンテナ運用方針)
- [16. docker-compose を分離する理由](#docker-compose-を分離する理由)
- [17. 起動管理（Makefile）](#起動管理makefile)
- [18. 全サービス起動](#全サービス起動)
- [19. 停止](#停止)
- [20. 環境復元](#環境復元)
- [21. データ配置](#データ配置)
- [22. httpd の役割](#httpd-の役割)
- [23. VirtualHost](#virtualhost)
- [24. CGI / SSI 設定](#cgi/ssi-設定)
- [25. 設計思想](#設計思想)
- [26. 構築方針](#構築方針)
- [27. 設計原則](#設計原則)
- [28. ポート設計](#ポート設計)
- [29. ディレクトリ構成](#ディレクトリ構成)
- [30. ディレクトリ役割](#ディレクトリ役割)
  - [Dropbox](#dropbox)
  - [dotfiles](#dotfiles)
- [31. 設計意図](#設計意図)
- [32. httpd の役割](#httpd-の役割)
- [33. compose 分離方針](#compose-分離方針)
- [34. 環境復元手順（Disaster Recovery）](#環境復元手順disaster-recovery)
- [35. 前提条件](#前提条件)
- [36. 設定の取得](#設定の取得)
- [37. Dockerサービス起動](#dockerサービス起動)
- [38. データ復元](#データ復元)
- [39. Webサイトデータ](#webサイトデータ)
- [40. 動作確認](#動作確認)
- [41. 設計思想](#設計思想)

# 1. 目的

現在の本番環境とは別に、ローカルで CGI / HTML サイトを動かして
動作確認できる環境を作る。

ポイント：

* Dropbox 上のサイトデータを **そのまま使用**
* upload せず **ローカルで動作確認**
* 本番への deploy 方法（SFTP 自動 upload）は **変更しない**

---

# 2. 現在の運用構成

```
Dropbox
   │
   ├─ site1
   └─ site2
```

本番運用

```
Dropbox
   ↓
SFTP 自動 upload
   ↓
Xserver
```

サイト技術

```
HTML
Perl CGI (.cgi .pl)
SSI (.shtml)
文字コード UTF-8
PHP 未使用
```

---

# 3. Docker導入の目的

Docker を追加することで

```
Dropbox
   ├─ Docker (ローカルテスト)
   └─ SFTP upload
         ↓
       本番サーバー
```

という構造にする。

メリット

* upload 前に動作確認
* CGI / SSI / .htaccess テスト
* 本番環境を壊さない
* 編集 → ブラウザ更新 で確認可能

---

# 4. Dropbox を Web root として使う

Docker では **bind mount** を使う。

例

```
docker run -p 8080:80 \
-v ~/Dropbox/site1:/var/www/site1 \
-v ~/Dropbox/site2:/var/www/site2 \
local-apache
```

意味

```
Dropbox/site1 → Docker /var/www/site1
Dropbox/site2 → Docker /var/www/site2
```

つまり

```
Dropbox = DocumentRoot
```

となる。

編集 → 保存 → ブラウザ更新
だけで反映される。

upload 不要。

---

# 5. 複数サイトの構成

Dropbox

```
Dropbox/
   site1/
   site2/
```

Apache VirtualHost

```
site1.local → /var/www/site1
site2.local → /var/www/site2
```

hosts 設定

```
127.0.0.1 site1.local
127.0.0.1 site2.local
```

ブラウザ

```
http://site1.local:8080
http://site2.local:8080
```

---

# 6. Apache設定（Xserverに近づける）

本番環境では

```
どのディレクトリでも CGI / SSI が実行可能
```

Dockerでも同様にする。

Apache設定例

```
<Directory "/var/www">
    Options +ExecCGI +Includes
    AddHandler cgi-script .cgi .pl
    AddType text/html .shtml
    AddOutputFilter INCLUDES .shtml
    AllowOverride All
</Directory>
```

これで

```
CGI
SSI
.htaccess
```

が全ディレクトリで動作。

---

# 7. CGI の注意点

## Perlパス

CGI先頭

```
#!/usr/bin/perl
```

本番サーバーと一致させる。

確認

```
which perl
```

---

## 実行権限

```
chmod 755 script.cgi
```

---

## 改行コード

CGIは

```
LF
```

である必要がある。

CRLF の場合

```
500 Internal Server Error
```

になる。

---

# 8. Dockerfile例

Perl を入れる。

```
FROM httpd:2.4

RUN apt-get update \
 && apt-get install -y perl

COPY httpd.conf /usr/local/apache2/conf/httpd.conf
```

---

# 9. Dockerディレクトリ構成

```
Dropbox/
   site1/
   site2/

docker/
   Dockerfile
   httpd.conf
   docker-compose.yml
```

---

# 10. docker-compose例

```
services:
  web:
    build: .
    ports:
      - "8080:80"
    volumes:
      - ~/Dropbox/site1:/var/www/site1
      - ~/Dropbox/site2:/var/www/site2
```

起動

```
docker compose up
```

---

# 11. 運用フロー

従来

```
編集
 ↓
upload
 ↓
確認
```

Docker導入後

```
編集
 ↓
ブラウザ更新
 ↓
確認
 ↓
問題なし
 ↓
SFTP upload
 ↓
本番
```

---

# 12. Docker導入の難易度

| 項目       | 難易度 |
| -------- | --- |
| Docker導入 | ★   |
| Apache設定 | ★★  |
| CGI対応    | ★★  |
| 本番との差調整  | ★★★ |

半日〜1日程度で構築可能。

---

# 13. 期待できる効果

* upload不要のローカルテスト
* CGI開発が高速化
* .htaccess の安全な検証
* 本番トラブルの減少

---

# 14. 今後の拡張

将来的には

```
git pull
docker compose up
```

だけで

ローカル本番環境を再現する運用も可能。

# Docker ローカル環境設計メモ（追補）

## 15. コンテナ運用方針

Docker コンテナは **常時起動**を前提とする。

理由

* 開発環境を常に利用可能にする
* 再起動による待ち時間を減らす
* Web / Git / Chat のサービスを常時利用する

対象サービス

* Gitea
* Mattermost
* httpd (ローカル Web テスト)

---

# 16. docker-compose を分離する理由

各サービスは **個別の compose 環境として分離する。**

構造

```
dotfiles/
   docker/
      gitea/
         docker-compose.yml
      mattermost/
         docker-compose.yml
      httpd/
         docker-compose.yml
```

メリット

* 障害の影響範囲を限定できる
* 個別再起動が可能
* ログ確認が容易
* サービス追加が簡単

例

```
docker compose -f docker/gitea/docker-compose.yml restart
```

---

# 17. 起動管理（Makefile）

各サービスの起動は Makefile から行う。

例

```makefile
gitea:
	cd docker/gitea && docker compose up -d

mattermost:
	cd docker/mattermost && docker compose up -d

httpd:
	cd docker/httpd && docker compose up -d
```

---

# 18. 全サービス起動

```makefile
start: gitea mattermost httpd
```

実行

```
make start
```

---

# 19. 停止

```makefile
stop:
	cd docker/gitea && docker compose down
	cd docker/mattermost && docker compose down
	cd docker/httpd && docker compose down
```

---

# 20. 環境復元

新しいPCでの復元手順

```
git clone dotfiles
make start
```

これにより

* Gitea
* Mattermost
* httpd

が起動する。

データは Dropbox 上のディレクトリを使用するため自動復元される。

---

# 21. データ配置

Dropbox 側には永続データのみ配置する。

```
Dropbox/
   site1/
   site2/
   docker-data/
      gitea/
      mattermost/
```

役割

* site1 / site2 : Web サイトデータ
* docker-data : Docker 永続データ

---

# 22. httpd の役割

httpd コンテナは

```
Dropbox/site1
Dropbox/site2
```

を **DocumentRoot としてマウント**し
ローカル Web テスト環境を提供する。

例

```
~/Dropbox/site1 → /var/www/site1
~/Dropbox/site2 → /var/www/site2
```

---

# 23. VirtualHost

ローカルドメイン

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

# 24. CGI / SSI 設定

Apache 設定

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

がすべてのディレクトリで動作する。

---

# 25. 設計思想

本環境は次の原則で構成される。

```
設定   → Git (dotfiles)
データ → Dropbox
実行   → Docker
管理   → Makefile
```

目的

* 環境の完全再構築
* PC変更時の即時復元
* 安定した開発環境
* サービスの分離運用

# 26. 構築方針

本環境は **段階的構築**を基本方針とする。

一度に全サービスを構築するのではなく、
**一つずつ確実に構築・検証する。**

想定手順

```id="w57s34"
1 Gitea
2 Mattermost
3 httpd
```

各段階で次の確認を行う。

```id="s9bhkp"
1 起動できる
2 再起動できる
3 PC再起動後も復元できる
```

---

# 27. 設計原則

本環境は次の原則で構成する。

```id="0owhgs"
設定   → Git (dotfiles)
データ → Dropbox
実行   → Docker
管理   → Makefile
```

この構造により

* 環境の完全再構築
* PC変更時の迅速な復元
* 安定した開発環境

を実現する。

---

# 28. ポート設計

複数サービスを常時起動するため
**ポート番号を事前に整理しておく。**

例

```id="h9suhc"
httpd       8080
Gitea       3000
Mattermost  8065
```

ポート番号を固定することで

* 設定変更を防ぐ
* サービス識別を容易にする
* トラブル時の調査を簡単にする

# 29. ディレクトリ構成

本環境では **設定とデータを分離**する。

```id="u7y4j0"
Dropbox/
   site1/
   site2/
   docker-data/
      gitea/
      mattermost/

dotfiles/
   docker/
      gitea/
         docker-compose.yml
      mattermost/
         docker-compose.yml
      httpd/
         docker-compose.yml
         httpd.conf
         Dockerfile
   Makefile
```

---

# 30. ディレクトリ役割

## Dropbox

永続データを保存する。

```id="g8hl9i"
site1
site2
```

Webサイトデータ。

```id="td69an"
docker-data
```

Dockerコンテナの永続データ。

例

```id="mbpknp"
docker-data/gitea
docker-data/mattermost
```

---

## dotfiles

環境構築用設定を保存する。

```id="n9yydv"
docker/
```

Docker設定ファイル。

```id="uzv9ky"
Makefile
```

サービス起動・管理用。

---

# 31. 設計意図

本構成は次の分離を目的とする。

```id="86leav"
設定   → Git (dotfiles)
データ → Dropbox
```

これにより

* 環境の再構築
* PC変更時の復元
* 設定変更の履歴管理

が容易になる。

---

# 32. httpd の役割

httpd コンテナは

```id="z7atog"
Dropbox/site1
Dropbox/site2
```

を **DocumentRoot としてマウント**し
ローカル Web テスト環境を提供する。

例

```id="hsop6x"
~/Dropbox/site1 → /var/www/site1
~/Dropbox/site2 → /var/www/site2
```

---

# 33. compose 分離方針

各サービスは **独立した docker-compose 環境**とする。

理由

* 障害の相互影響を防ぐ
* 個別再起動を可能にする
* サービス管理を簡単にする

対象サービス

```id="5go8z9"
gitea
mattermost
httpd
```
# 34. 環境復元手順（Disaster Recovery）

PC故障や新しいPCへの移行時は
次の手順で環境を復元する。

---

# 35. 前提条件

次のソフトウェアをインストールする。

```id="d6ox3a"
git
docker
docker compose
```

Dropbox クライアントをインストールし
データを同期する。

---

# 36. 設定の取得

dotfiles を取得する。

```id="8x00ri"
git clone <dotfiles repository>
cd dotfiles
```

---

# 37. Dockerサービス起動

Makefile を利用してサービスを起動する。

```id="gheq85"
make start
```

これにより次のサービスが起動する。

```id="h1o6kw"
gitea
mattermost
httpd
```

---

# 38. データ復元

永続データは Dropbox に保存されているため
自動的に利用される。

対象ディレクトリ

```id="2c20ra"
Dropbox/docker-data/gitea
Dropbox/docker-data/mattermost
```

---

# 39. Webサイトデータ

Webサイトは Dropbox のディレクトリを
直接利用する。

```id="9h7ycy"
Dropbox/site1
Dropbox/site2
```

httpd コンテナはこれらを
DocumentRoot としてマウントする。

---

# 40. 動作確認

ブラウザで次を確認する。

```id="0j7so8"
http://localhost:3000   (Gitea)
http://localhost:8065   (Mattermost)
http://localhost:8080   (httpd)
```

すべて正常に表示されれば
環境復元は完了。

---

# 41. 設計思想

本環境は次の原則で構成される。

```id="l0n6s9"
設定   → Git (dotfiles)
データ → Dropbox
実行   → Docker
管理   → Makefile
```

この構造により
短時間で環境を復元できる。

