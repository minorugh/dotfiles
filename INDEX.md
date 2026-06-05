# P1 環境リファレンス INDEX

更新日: 2026-06-05

---

## 新規PCリストア時はこの順で読む

| # | ファイル | 内容 |
|---|---------|------|
| 00 | [env-import-README](READMES/00-env-import-README.md) | GPG鍵インポート・dotfilesクローン手順 |
| 01 | [env_source-README](READMES/01-env_source-README.md) | SSH鍵・.netrc・機密ファイルの管理 |
| 02 | [dotfiles-README](READMES/02-dotfiles-README.md) | dotfiles全体の構成・Makefileターゲット一覧 |
| 03 | [backup-README](READMES/03-backup-README.md) | 夜間自動バックアップの仕組み |
| 04 | [cron-README](READMES/04-cron-README.md) | cron・systemd timerで動くスクリプト群 |
| 05 | [mutt-README](READMES/05-mutt-README.md) | neomutt設定・abook連携 |
| 06 | [abook-README](READMES/06-abook-README.md) | アドレス帳バックアップ・リストア |
| 07 | [bin-README](READMES/07-bin-README.md) | ~/bin のスクリプト群 |
| 08 | [docker-README](READMES/08-docker-README.md) | Docker環境（Gitea等） |
| 09 | [Dropbox-README](READMES/09-Dropbox-README.md) | Dropbox/backup/ ディレクトリ構成 |

---

## リストア手順（概要）

```
1. Dropbox インストール・同期完了
        ↓
2. env-import を HTTPS でクローン
   git clone https://github.com/minorugh/env-import.git
        ↓
3. make gpg          # GPG秘密鍵インポート
        ↓
4. make dotfiles     # dotfilesクローン＋環境復元
        ↓
5. reboot
        ↓
6. make switch-ssh   # git remote を SSH に切り替え
        ↓
7. make abook-restore  # アドレス帳リストア
```

詳細は `00-env-import-README` を参照。

---

## バックアップの在処

| データ | 場所 |
|--------|------|
| dotfiles | github.com/minorugh/dotfiles |
| 機密ファイル (.env_source) | Dropbox/backup/env/env_repo.bundle.gpg |
| GPG秘密鍵 | Dropbox/backup/env/gnupg/secret-all.key.gpg |
| mozc辞書 | Dropbox/backup/mozc/ |
| abook アドレス帳 | Dropbox/backup/abook/addressbook_YYYY-MM-DD.gpg |
| Thunderbird設定 | Dropbox/backup/thunderbird/ |
| zsh履歴 | Dropbox/backup/env/zsh/ |
| Gitea data | Dropbox/backup/gitea/ |

---

## READMES/ について

`READMES/` は各所の README.md へのシンボリックリンク集約ディレクトリ。
`.gitignore` により git 管理外（ローカル専用）。

リストア後に再構築する場合は各ディレクトリで `mklink-readme` を実行。
