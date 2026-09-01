# Arch版サブ機 環境構築（Let's note CF-LX3）

Debian機（P1メイン / X250サブ）と同じ dotfiles・秘密鍵管理の仕組みに、
Arch Linuxからも乗るための手順です。パッケージ導入だけpacman/AUR向けに
作り直し、SSH鍵・dotfiles本体は既存の仕組み（env-import・dotfilesリポジトリ）
をそのまま流用します。

このディレクトリ（`arch-install/`）はdotfilesリポジトリの一部として
git管理しています（`git/`・`docker/`・`cron/`と同じくサブMakefile方式）。
本体の`.zshrc`・`.emacs.d`等は再cloneせず、このMakefileが動いている
時点で既に手元にあるものをそのままシンボリックリンクします。

---

## 環境構築の手順

### 0. Arch clean install（make実行前の手動準備）

archinstallでXfce + sudoユーザーまで作成しておきます（ここまでは
別途USB作成〜archinstall実行の話として完了している前提）。

### 1. dotfilesをclone

Makefileがまだ手元に無いので、gitだけ先に手動で入れます。

```bash
sudo pacman -Sy --noconfirm git
mkdir -p ~/src/github.com/minorugh
git clone https://github.com/minorugh/dotfiles.git ~/src/github.com/minorugh/dotfiles
cd ~/src/github.com/minorugh/dotfiles/arch-install
```

以降はこのディレクトリで`make`を実行します。

### 2. 基本パッケージ・AURヘルパー・Dropbox

```bash
make base         # zsh/vim/emacs/gnome-terminal/git/openssh/keychain/gnupg/chromium等
make sudo-setup   # wheelグループのsudo有効化
make yay          # AURヘルパー(yay)導入
make aur          # dropbox導入
```

`make aur`のあと、メニューからDropboxを起動して初期設定・同期完了を
待ちます（この後の`env-restore`はDropbox上の暗号化bundleを読むため）。

### 3. 秘密鍵・SSH復元

> ⚠️ この段階ではまだSSHが使えないため、env-importはHTTPSでcloneします
> （`make gpg`が内部で自動的にclone）。

```bash
make gpg          # env-importをclone→パスフレーズ入力で秘密鍵インポート
make env-restore  # ~/.env_source をDropboxのbundleから復元
make ssh          # ~/.env_source/.ssh/* をシンボリックリンク
```

`make gpg`実行時、内部で`sudo apt install gnupg`がエラー表示されますが
無視して構いません（gnupgは`make base`で既に入っているため、後続の
インポート処理自体は問題なく完走します）。

### 4. dotfiles展開

```bash
make init         # .emacs.d/.zshrc/.vimrc/.gitconfig をシンボリックリンク
```

この時点でemacs/zsh/vim/gitがすぐ使える状態になります。

### 5. 再起動してSSHに切り替え

```bash
reboot
```

再起動後、SSH接続を確認してから切り替えます。

```bash
ssh -T git@github.com
# → "Hi minorugh! You've successfully authenticated..." と出ればOK

cd ~/src/github.com/minorugh/dotfiles/arch-install
make switch-ssh    # env-import・dotfiles両方のremoteをSSHに切り替え
make push-block    # 両方ともpush封鎖（サブ機と同じくpull専用で運用）
```

### 6. シェルをzshに変更

```bash
make zsh-default
```

---

## make ターゲット一覧

`make help`で一覧表示できます。

| ターゲット | 内容 |
|---|---|
| `make base` | 基本パッケージ一括インストール |
| `make sudo-setup` | sudoグループ(wheel)の有効化 |
| `make yay` | AURヘルパー(yay)のビルド・インストール |
| `make aur` | AUR経由でdropboxをインストール |
| `make gpg` | env-importをclone→秘密鍵インポート（`##!`: パスフレーズ入力あり） |
| `make env-restore` | `~/.env_source`をDropboxのbundleから復元 |
| `make ssh` | SSH鍵一式をシンボリックリンク |
| `make init` | dotfiles（emacs/zsh/vim/git）をシンボリックリンク展開 |
| `make switch-ssh` | git remoteをHTTPS→SSHに切り替え（`##!`: 再起動・SSH確認後） |
| `make push-block` | env-import・dotfilesともpush封鎖（pull専用） |
| `make zsh-default` | ログインシェルをzshに変更 |
| `make all` | base→sudo-setup→yay→aur→zsh-default を一括実行 |

`make all`は自動化できる範囲（パッケージ導入）のみをまとめたもので、
パスフレーズ入力や再起動を挟む`gpg`以降は手動で順番に実行してください。

---

## Debian版との違い

- パッケージ導入はpacman/AURに置き換え。パッケージ一覧はDebian版より
  大幅に絞っています（emacs/zsh/vim/gnome-terminal/chromium/dropbox等、
  実際に使うものだけ）
- Debian版の`baseinstall`（apt決め打ちの巨大チェーン）は呼びません。
  keymap・grub・autostart・cron・docker等の細かいシステム設定は
  このArch版には持ち込んでいません（必要になれば都度追加）
- env-importリポジトリは無改造のまま流用しています。内部の
  `sudo apt install gnupg`はArch上ではエラー表示されますが、
  `make base`で事前にgnupgを入れておくことで後続処理には影響しません
