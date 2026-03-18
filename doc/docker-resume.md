# 個人用 Mattermost & Gitea 構築レジュメ

## 目次

<div class="toc">
- [現状](#現状)
- [考察](#考察)
- [結論](#結論)
- [目的のまとめ](#目的のまとめ)
- [基本方針](#基本方針)
- [ディレクトリ構成例（これでいいかは自信ない）](#ディレクトリ構成例これでいいかは自信ない)
- [メイン機 / サブ機運用](#メイン機/サブ機運用)
- [推奨構成・注意点](#推奨構成・注意点)
	- [Mattermost](#mattermost)
	- [Gitea](#gitea)
	- [共通注意](#共通注意)
- [次のステップ](#次のステップ)
</div>


## 現状

- Debian 12 上で Mattermost を Docker Compose で試用中（個人的な日誌・備忘録用途）
- dotfiles や Web 記事データは Github + Xserver でgit管理中
- 次のステップとして Gitea を Docker Compose で構築して設定は dotfiles、DB・リポジトリは Dropbox に置きたい
- メイン機・サブ機で共有したいが競合トラブルを防ぐため原則サブ機は閲覧のみ
- Composeでいれたメイン機のMattermost+desktopアプリ は試用段階で中身が少ないので移行せずに削除して新規構築としたい

## 考察

1. 現用Mattermost をそのまま残す vs 新規構築

- 中身が少なく試行段階 → 新規立ち上げのほうが圧倒的に簡単
- 古い DB や設定を気にせず、Docker Compose の yaml から新環境を一発で再現可能
- dotfiles 管理に統一できるので、運用がシンプルになる

2. Gitea と共通の運用方針

- 設定: dotfiles
- データ: Dropbox
- メイン機で書き込み・更新、サブ機は閲覧のみ
→ Mattermost も同じ方式に統一すると管理しやすい

3. Dropbox 共有時の注意点

- 競合回避のため、書き込みはメイン機のみ
- Docker ボリュームは Dropbox 上に置く形（.db ファイルやリポジトリデータ）
- Gitea も Mattermost もこの方式でメイン機・サブ機を安全に共有可能

4. 運用簡略化のポイント

- Docker Compose の設定ファイルは dotfiles に置く
- DB やデータは Dropbox
- サブ機は「Compose up で閲覧用サービス起動のみ」
- 万一サブ機で書き込み操作が走っても、Dropbox 側の競合・破損リスクが少ない

## 結論

現状の Mattermost は移行せず削除して新規構築するほうが管理も運用もシンプルだと思う。
Gitea も Mattermost も同じ運用方針（dotfiles + Dropbox データ + メイン機書き込み/サブ機閲覧）に統一すると、将来的なメンテナンス負荷が最小化できます。

----

## 目的のまとめ
- 個人的な日誌・備忘録として **Mattermost** を利用
- Git リポジトリやウェブ記事管理用に **Gitea** を利用
- 両方とも **常時起動**
- 設定は **dotfiles で管理**
- DB・データは **Dropbox 上で共有**
- メイン機で書き込み、サブ機は閲覧のみ
- メイン機サブ機を更新しても安全にリストア出来る仕組みをdotfilesのmakefileに
- mattermost Giteaともdesktopアプリがあるならそのinstallもmakefileのターゲットに加えておきたい。

---

## 基本方針

1. **独立した Docker Compose プロジェクト**
   - Mattermost と Gitea は統合せず、別々に構築
   - 片方のトラブルがもう片方に影響しない

2. **データ管理**
   - 設定ファイルは `dotfiles` 配下に置く
   - DB やリポジトリデータは Dropbox 上に置く
   - メイン機のみ書き込み、サブ機は閲覧専用

3. **運用方式**
   - Docker Compose で常時起動 (`docker compose up -d`)
   - 障害やアップデート時も個別対応可能

---

## ディレクトリ構成例（これでいいかは自信ない）

``` text
/home/minoru/src/github.com/minorugh/dotfiles/docker
├─ mattermost/
│ └─ docker-compose.yml
└─ gitea/
└─ docker-compose.yml

~/Dropbox/docker/mattermost_data/
~/Dropbox/docker/gitea_data/
```

---

## メイン機 / サブ機運用

| 機 | 動作 | 書き込み権限 |
|----|------|-------------|
| メイン機 | Mattermost & Gitea 常時起動 | ○ |
| サブ機   | Mattermost & Gitea 常時起動 | ×（閲覧のみ） |

---

## 推奨構成・注意点

### Mattermost
- 初期は軽量 DB（SQLite）でスタート可
- データ量が増えたら PostgreSQL に移行も可能
- dotfiles で docker-compose.yml 管理

### Gitea
- Git リポジトリ量に応じて DB 選択（SQLite / MySQL / PostgreSQL）
- Dropbox 上にリポジトリデータと DB を置き、サブ機は閲覧専用
- 書き込み・更新はメイン機でのみ行う

### 共通注意
- Dropbox 共有時は書き込みはメイン機のみに限定
- Docker ボリュームは Dropbox 上にマウントして競合回避
- Compose プロジェクト単位で起動・停止を管理

---

## 次のステップ
1. **Mattermost の Docker Compose ファイル作成**
2. **Gitea の Docker Compose ファイル作成**
3. Dropbox ボリュームのマウント設計
4. メイン機・サブ機の起動テスト
5. 運用ルールを dotfiles に明記
