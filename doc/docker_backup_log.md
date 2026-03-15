# Docker & Backup 環境整理ログ

**日付:** 2026-03-15

## 現状
- メイン機(P1)で GitHub と xserver に git 管理済み
- makefile + cron で自動バックアップ運用
- 手動 commit/push はメイン機のみ、サブ機は pull だけ
- Docker 利用で Mattermost 運用中。Gitea も docker-compose 予定
- Debian12 → 13 アップデート検討中
- Dropbox は両機で同一アカウント利用

## 目標
- 環境設定は dotfiles、データは Dropbox、アプリは Docker で整理
- メイン機とサブ機で Docker 環境を共有するが、競合は避けたい
- Mattermost と Gitea は別 docker-compose で管理
- 専門知識がなくても簡単に運用できる仕組み

## Docker環境共有の考え方
- Docker イメージやコンテナは Dropbox に置かない
- 共有するのは設定ファイル（dotfiles）と永続データ（Dropbox）
- docker-compose.yml と .env は dotfiles 管理
- DB やアップロードファイルなどの永続データを Dropbox に置きマウント

## 運用方針
- サブ機は pull してデータ確認や軽い操作のみ
- メイン機で Docker イメージ build → サブ機は pull で展開
- 両機で同時起動は避ける

## クリーンインストール & 復元戦略
1. メイン機 P1 を Debian13 クリーンインストール
2. dotfiles でユーザー環境復元
3. Docker-compose と設定ファイルも dotfiles から復元
4. Dropbox データを mount して docker-compose up

## Mattermost & Gitea 分離運用
- アプリ別に docker-compose ディレクトリを分ける
- 各 .env と設定ファイルを dotfiles 管理
- データは Dropbox で共有
- docker-compose up で個別に起動可能

## 簡単に実現するポイント
- 設定は dotfiles、データは Dropbox で分離
- Docker イメージやコンテナは各マシンで個別管理
- メイン機を「ビルド＆push」、サブ機を「pull＆確認」専用にする
- 同時起動を避けて Dropbox データ競合を防ぐ
- アプリ別 docker-compose で運用

## まとめ
- dotfiles + Dropbox + Docker で安全に環境を移行・共有可能
- 専門知識がなくても「docker-compose up」で運用可能
