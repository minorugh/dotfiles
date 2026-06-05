# NeoMutt 設定リファレンス (dotfiles/.mutt/)

更新日: 2026-06-05

---

## 1. ファイル構成

### dotfiles/.mutt/（公開リポジトリ・シンボリックで ~/.mutt/ に展開）

```
dotfiles/.mutt/
  mailcap              # MIMEタイプ別の処理設定
  certificates         # SSL証明書
  dracula.muttrc       # Draculaカラーテーマ
  nord.muttrc          # Nordカラーテーマ
  abook-add.sh         # Fromヘッダーをデコードして abook に追加するラッパー
  README.md            # このファイル
```

### ~/.env_source/.mutt/（非公開リポジトリ・機密ファイル）

```
~/.env_source/.mutt/
  password.rc          # Gmail認証情報（平文・非公開管理）
  signature            # メール署名（GPG暗号化）
```

### ~/.abook/（gitignore済み・Dropboxにバックアップ）

```
~/.abook/
  addressbook          # abookアドレス帳（実データ）
  backup.log           # バックアップ実行ログ（abook-backup.sh が記録）
  README.md            # バックアップ・リストア手順
```

---

## 2. 読み込み関係

```
~/.muttrc
  └─ source "~/.env_source/.mutt/password.rc"   # 認証情報
  └─ set signature = "~/.env_source/.mutt/signature"  # 署名
```

---

## 3. abook 連携

| 操作 | キー | 説明 |
|------|------|------|
| アドレス追加 | `a` | Fromヘッダーをデコードして追加（`abook-add.sh` 経由） |
| abook を開く | `Ctrl+a` | 対話モードで直接編集 |
| 宛先補完 | `Tab` | 作成画面でアドレスを検索・補完 |

### abook-add.sh の役割

neomutt の `a` キーで呼ばれるラッパースクリプト。
UTF-8/Base64 エンコードされた日本語の差出人名を正しくデコードしてから
`abook --add-email` に渡す。

```
neomutt → a キー → abook-add.sh（デコード処理）→ abook --add-email
```

### アドレス帳のバックアップ・リストア

- バックアップ: 毎夜 `autobackup.sh` 経由で GPG 暗号化して Dropbox に保存
- リストア: `env-import/Makefile` の `make abook-restore`

---

## 4. リストア時のセットアップ

`dotfiles/Makefile` の `neomutt` ターゲットで一括セットアップ：

```bash
make neomutt
```

以下を自動実行：
- `neomutt` / `urlscan` / `abook` のインストール
- `~/.mutt/` 以下へのシンボリックリンク展開
- `~/.muttrc` のシンボリックリンク作成

アドレス帳のリストアは別途：

```bash
cd ~/src/github.com/minorugh/env-import
make abook-restore
```
