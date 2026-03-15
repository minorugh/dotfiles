# Docker 完全運用・学習ガイド（Mattermost & Gitea）

**日付:** 2026-03-15  
**対象マシン:** メイン機 P1 / サブ機 x250  
**目的:** Mattermost と Gitea を別 docker-compose で管理、dotfiles + Dropbox で設定・データ管理、常時起動

---

## 1. はじめに

このガイドでは以下を学習・理解した上で構築できます。

- Docker / docker-compose の基本構造とボリューム管理
- dotfiles と Dropbox を使った設定・データ分離
- Mattermost 既存環境の安全な移行
- Gitea 新規構築手順
- 常時起動設定(systemd)
- サブ機での再現と競合回避

> 目的は、**「何を、なぜ、どうやってやるか」**を理解してから構築することです。

---

## 2. ディレクトリ構成

### dotfiles（設定管理）
- docker-compose.yml、.env、config フォルダを保存
- バージョン管理可能（git）

### Dropbox（データ共有）
- DBやアップロードファイル、リポジトリなど永続データを置く
- メイン機・サブ機で共有可能
- DBは常に1台のみ稼働

> 例:
> ~/dotfiles/docker/mattermost/
> ~/Dropbox/docker/mattermost/data/db/
> ~/Dropbox/docker/mattermost/data/files/
> ~/dotfiles/docker/gitea/
> ~/Dropbox/docker/gitea/data/db/
> ~/Dropbox/docker/gitea/data/repos/

> この分離により、設定とデータが独立し、Dockerイメージは各マシン個別に管理可能です。

---

## 3. Mattermost 移行手順（学習付き）

### 3.1 事前バックアップ
> docker ps  
> docker export mattermost_app > ~/mattermost_backup.tar  
> docker exec -t mattermost_db pg_dumpall -U mmuser > ~/mattermost_db_backup.sql  

> ※ポイント: DBだけでなくコンテナ全体も保存すると復元可能です

### 3.2 新構想用ディレクトリ準備
> mkdir -p ~/dotfiles/docker/mattermost/config  
> mkdir -p ~/Dropbox/docker/mattermost/data/db  
> mkdir -p ~/Dropbox/docker/mattermost/data/files  

> ※学習: config は dotfiles に、db/files は Dropbox に配置。複数マシンで再現可能

### 3.3 docker-compose.yml 設定例
> version: '3.8'  
> services:  
> &nbsp;&nbsp;db:  
> &nbsp;&nbsp;&nbsp;&nbsp;image: postgres:15  
> &nbsp;&nbsp;&nbsp;&nbsp;container_name: mattermost_db  
> &nbsp;&nbsp;&nbsp;&nbsp;restart: always  
> &nbsp;&nbsp;&nbsp;&nbsp;environment:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;POSTGRES_USER: mmuser  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;POSTGRES_PASSWORD: mmpass  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;POSTGRES_DB: mattermost  
> &nbsp;&nbsp;&nbsp;&nbsp;volumes:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- ~/Dropbox/docker/mattermost/data/db:/var/lib/postgresql/data  
> &nbsp;&nbsp;&nbsp;&nbsp;app:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;image: mattermost/mattermost-enterprise-edition:7  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;container_name: mattermost_app  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;restart: always  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ports:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- "8065:8065"  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;environment:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;MM_USERNAME: mmuser  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;MM_PASSWORD: mmpass  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;MM_DBNAME: mattermost  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;MM_DBUSER: mmuser  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;MM_DBPASSWORD: mmpass  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;MM_DBHOST: db:5432  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;depends_on:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- db  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;volumes:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- ~/Dropbox/docker/mattermost/data/files:/mattermost/data  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- ~/dotfiles/docker/mattermost/config:/mattermost/config  

### 3.4 テスト起動
> cd ~/dotfiles/docker/mattermost/  
> docker compose up -d  

> ※注意: 既存コンテナと同時起動しないこと。DBはDropboxに置く場合、複数マシンで同時起動すると競合します。

### 3.5 古いコンテナ停止
> docker compose -f old_docker_compose.yml down  

---

## 4. Gitea 新規構築手順

### 4.1 ディレクトリ作成
> mkdir -p ~/dotfiles/docker/gitea/config  
> mkdir -p ~/Dropbox/docker/gitea/data/db  
> mkdir -p ~/Dropbox/docker/gitea/data/repos  

### 4.2 docker-compose.yml 設定例
> version: '3.8'  
> services:  
> &nbsp;&nbsp;db:  
> &nbsp;&nbsp;&nbsp;&nbsp;image: postgres:15  
> &nbsp;&nbsp;&nbsp;&nbsp;container_name: gitea_db  
> &nbsp;&nbsp;&nbsp;&nbsp;restart: always  
> &nbsp;&nbsp;&nbsp;&nbsp;environment:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;POSTGRES_USER: gitea  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;POSTGRES_PASSWORD: gitea_pass  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;POSTGRES_DB: gitea  
> &nbsp;&nbsp;&nbsp;&nbsp;volumes:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- ~/Dropbox/docker/gitea/data/db:/var/lib/postgresql/data  
> &nbsp;&nbsp;&nbsp;&nbsp;gitea:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;image: gitea/gitea:1.21.3  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;container_name: gitea_app  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;restart: always  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ports:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- "3000:3000"  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- "222:22"  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;environment:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;USER_UID: 1000  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;USER_GID: 1000  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;DB_TYPE: postgres  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;DB_HOST: db:5432  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;DB_NAME: gitea  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;DB_USER: gitea  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;DB_PASSWD: gitea_pass  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;depends_on:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- db  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;volumes:  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- ~/Dropbox/docker/gitea/data/repos:/data  
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- ~/dotfiles/docker/gitea/config:/etc/gitea  

### 4.3 起動
> cd ~/dotfiles/docker/gitea/  
> docker compose up -d  

> 初回セットアップ後は Dropbox データと dotfiles 設定だけで再現可能です。

---

## 5. 常時起動(systemd)

### Mattermost
> [Unit]  
> Description=Mattermost Docker Compose Service  
> Requires=docker.service  
> After=docker.service  
>   
> [Service]  
> WorkingDirectory=/home/minoru/dotfiles/docker/mattermost  
> ExecStart=/usr/bin/docker compose up  
> ExecStop=/usr/bin/docker compose down  
> Restart=always  
> User=minoru  
> Group=minoru  
>   
> [Install]  
> WantedBy=multi-user.target  

### Gitea
> [Unit]  
> Description=Gitea Docker Compose Service  
> Requires=docker.service  
> After=docker.service  
>   
> [Service]  
> WorkingDirectory=/home/minoru/dotfiles/docker/gitea  
> ExecStart=/usr/bin/docker compose up  
> ExecStop=/usr/bin/docker compose down  
> Restart=always  
> User=minoru  
> Group=minoru  
>   
> [Install]  
> WantedBy=multi-user.target  

> sudo systemctl enable mattermost  
> sudo systemctl enable gitea  

---

## 6. 運用ルールと注意点

- Mattermost 設定は dotfiles で管理  
- Mattermost DB は Dropbox 上で共有（サブ機で直接起動しない）  
- Gitea は dotfiles + Dropbox で設定・データ管理  
- コンテナ更新はメイン機で build & push  
- サブ機は pull & up で参照のみ  
- DB は常に1台のみ稼働（Dropbox 競合回避）  
- docker-compose.yml と .env は dotfiles に保管  

---

## 7. 実務運用ルール

- メイン機で構築・更新・push・バックアップ  
- サブ機は pull & up のみ、Mattermost DB は立ち上げない  
- Gitea は両機で共有運用可能  
- 設定は dotfiles に保存、データは Dropbox 上で共有  
- 同時に DB コンテナを複数マシンで起動しない  

---

## 8. トラブルシュート

- **DB競合**: 同時に DB コンテナを複数マシンで起動すると postgres がクラッシュ
- **ポート競合**: 同じポートで別アプリが起動している場合エラー
- **Dropbox同期遅延**: データ量が多いと初回同期に時間がかかる
- **docker-compose 再起動時のキャッシュ**: 更新したイメージが反映されない場合
- `docker compose pull && docker compose up -d --build` を実行

---
## 9. まとめ

- Mattermost と Gitea は別 compose で運用
- Mattermost: 設定は dotfiles、DB は Dropbox
- Gitea: 設定・データとも dotfiles + Dropbox
- systemd 常時起動、cron 自動バックアップ
- メイン機・サブ機両対応ルールで安全に構築・運用可能

## 10. 運用のポイント

- メイン機で構築・更新・push・バックアップ
- サブ機は pull & up のみ、Mattermost DB は立ち上げない
- 設定は dotfiles に保存、データは Dropbox 上で共有
- トラブル発生時は上記 8. を参照
