# CHANGELOG 2026-03-10

## 問題
再起動後に FileZilla・upsftp.pl で SSH パスフレーズを毎回求められる。
ターミナルは問題なし。X250（サブ機）セットアップ時に発覚。

## 根本原因
`autostart.desktop` の `Exec=bash $HOME/.autostart.sh` で
`$HOME` が展開されずスクリプトが実行されていなかった。
→ keychain が起動せず `SSH_AUTH_SOCK` のソケットが存在しない状態になっていた。

P1 では別の経路（.zshrc 経由）で動いていたため気づかなかった。

## 修正内容

### autostart.desktop
`$HOME` をフルパスに変更。

```
# 変更前
Exec=bash $HOME/.autostart.sh

# 変更後
Exec=bash /home/minoru/.autostart.sh
```

### .xprofile
keychain の起動を削除し `.autostart.sh` に一本化。
`${HOSTNAME}` を `$(hostname)` に統一。

```bash
if [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi
dbus-update-activation-environment --systemd SSH_AUTH_SOCK SSH_AGENT_PID
source ~/.keychain/$(hostname)-sh
```

### .autostart.sh
- `.zsh_history` のコピーを削除（`.zshrc` で親機/サブ機分岐済みのため不要）
- keyrings のコピーを `cp -rf` から `cp -a` に変更
  - `-a` はタイムスタンプ・パーミッションを元のまま保持するため、Default_keyring 以外のファイルのタイムスタンプが再起動のたびに更新される問題を解消

```bash
# 変更前
cp -rf ~/Dropbox/backup/keyrings/. ~/.local/share/keyrings/

# 変更後
cp -a ~/Dropbox/backup/keyrings/. ~/.local/share/keyrings/
```

### Makefile（dotfiles）
- `keyring` ターゲットを P1/サブ機で分岐：
  - P1: Dropbox へのシンボリックリンク（従来通り）
  - サブ機: シンボリックリンクが残っていれば削除するだけ（コピーは autostart.sh が担当）
- `emacs-mozc` の `ln -vsfn -rf` を `ln -vsn` に修正（`-rf` は `ln` には不正なオプション）
- 冒頭の英語手順コメントを削除し README.md に移動
- 全ターゲットの `## コメント` を日本語化
- footer の英語版「Customize settings」ブロックを削除（日本語版のみ残す）

```makefile
keyring:
    $(APT) seahorse libsecret-tools
ifeq ($(shell uname -n),P1)
    # 親機: Dropbox/backup/keyrings へのシンボリックリンク（正本）
    test -L ${HOME}/.local/share/keyrings || rm -rf ${HOME}/.local/share/keyrings
    ln -vsfn {${HOME}/Dropbox/backup,${HOME}/.local/share}/keyrings
else
    # サブ機: シンボリックリンクが残っていれば削除するだけ
    # コピーは起動時に autostart.sh が行う（Dropbox競合防止）
    test -L ${HOME}/.local/share/keyrings && rm -f ${HOME}/.local/share/keyrings || true
endif
```

### filezilla.sh
デバッグログを削除、`export SSH_AUTH_SOCK` を追加して完成。

```bash
source ~/.keychain/$(hostname)-sh 2>/dev/null
export SSH_AUTH_SOCK
filezilla "$@" &
```

### README.md
- Makefile 冒頭の手動準備手順を移動し Markdown で整形
- SSH キー・keychain の仕組みのセクションを新設
- make ターゲット一覧を表形式で追加
- 更新履歴を表形式に整理、誤記（Debian10→Debian11）を修正

## 確認済み動作
- P1・X250 両機とも再起動後にパスフレーズなしで動作
  - upsftp.pl、filezilla.sh、Emacs からの FileZilla 起動、すべて正常
- `~/Dropbox/backup/keyrings/` に競合発生なし
- `cp -a` により Default_keyring 以外のタイムスタンプが保持されることを確認

## 教訓
- `autostart.desktop` の `Exec=` では環境変数 `$HOME` が展開されない
- `cp -rf` ではコピー先のタイムスタンプがコピー実行時刻になる。バックアップ系のコピーには `-a` を使う
- `ln` に `-rf` は不正なオプション（`cp`・`rm` 用）。シンボリックリンク作成には `-vsn` で十分

---

## 追加作業（午後）

### deepl-translate.el — DeepL API 仕様変更対応

DeepL が認証方式を変更したため動作しなくなっていた。
POST ボディの `auth_key` を廃止し、`Authorization` ヘッダーに移行。

```elisp
;; 変更前
:data `(("auth_key" . ,deepl-auth-key)
        ("text" . ,text) ...)

;; 変更後
:headers `(("Authorization" . ,(format "DeepL-Auth-Key %s" deepl-auth-key)))
:data `(("text" . ,text) ...)
```

- 原作者の gist は未対応のまま。自分の GitHub リポジトリ側で修正
- `70-translate.el` の `:url` コメントに修正済みの旨を追記

### gpgimport — USB 依存廃止・AES256 暗号化方式に移行

**問題**：GPG 秘密鍵が USB メディアにしかなく、紛失・劣化リスクがあった。
また Dropbox に保存していた `encrypt.zip` は zip 暗号化強度が低く心もとなかった。

**対応**：
- `secret-all.key` を GPG 対称暗号化（AES256）して Dropbox に保存
- 旧 `encrypt.zip` と生の `secret-all.key` は `shred` で完全削除
- Makefile の `gpg` ターゲットを USB → Dropbox の `.key.gpg` から復号する形に変更
- `export` ターゲットも暗号化まで一本化
- README.md を新規作成（リストア手順を Step 1〜5 で整理）

```bash
# 保存（P1 で一度だけ実施済み）
gpg --symmetric --cipher-algo AES256 \
    -o ~/Dropbox/backup/gnupg/secret-all.key.gpg secret-all.key

# 復元時（make gpg が自動実行）
gpg --decrypt ~/Dropbox/backup/gnupg/secret-all.key.gpg > secret-all.key
```

パスフレーズは SSH 鍵（id_rsa）と統一。

**教訓**：
- zip のパスワード暗号化は強度が低い。秘密情報には GPG 対称暗号化（AES256）を使う
- 保存場所は README.md に明記しない（セキュリティ上）

---

## 追加作業（夕方）

### Dropbox/backup/ の棚卸し

GitHub（dotfiles）と重複している不要なディレクトリを整理・削除した。

**削除したもの**
- `dotfiles/` — GitHub 管理で十分
- `bin/` — dotfiles の bin/ で管理
- `devils/` — dotfiles 管理
- `etc/` — dotfiles 管理
- `local/` — dotfiles 管理
- `tex/` — dotfiles 管理
- `abook/` — dotfiles 管理
- `mutt/` — 不使用
- `w3m/` — dotfiles 管理
- `config/autostart/` — dotfiles 管理
- `config/sxiv/` — dotfiles 管理
- `config/git/` — gist/ に統合

**残したもの（Dropbox が必須）**
- `keyrings/` — keyring 正本
- `mozc/` — mozc 設定正本
- `filezilla/` — FileZilla 設定正本
- `gist/` — gist 認証ファイル・gitk 設定（後述）
- `icons/` — アイコン・壁紙
- `gnupg/` — GPG 秘密鍵（AES256 暗号化済み）
- `config/rclone/` `config/hub` — rclone・hub 設定正本
- `config/git/gitk` → `gist/gitk` に移動（後述）
- `GH/` — myjob.sh のパスワードファイル管理
- `passwd/` — KeePassXC の kdbx ファイル
- `deepl/` — DeepL API キー
- `zsh/` — .zsh_history・cdr 履歴
- `emacs/` — elpa 世代バックアップ・自作ロゴ
- `ssh/` — 保留（気持ちの整理がついたら削除）

### gist と gitk の整理

`gist`（認証ファイル単体）と `config/git/gitk` を `gist/` ディレクトリにまとめた。

```
# 変更前
~/Dropbox/backup/gist          # ファイル単体
~/Dropbox/backup/config/git/gitk

# 変更後
~/Dropbox/backup/gist/gist
~/Dropbox/backup/gist/gitk
```

Makefile の `gist` `gitk` ターゲットのパスを合わせて修正。
`make gist gitk` で P1・X250 両機とも動作確認済み。
