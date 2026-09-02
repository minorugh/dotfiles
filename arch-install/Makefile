### Arch Linux 環境構築 (Let's note CF-LX3 / Arch版サブ機)
# dotfilesリポジトリの一部として管理（git/・docker/・cron/ と同じくサブMakefile方式）。
# パッケージ導入はpacman/AUR向け。秘密鍵・SSH復元は既存の env-import リポジトリ
# （無改造・pull専用）に乗せる。dotfiles本体（.zshrc/.emacs.d等）は
# このMakefileが動いている時点で既にclone済みのため、再cloneはしない。
#
# 前提: archinstall で Xfce + sudoユーザー作成済み、ネット接続済み
# 初回のみ手動: sudo pacman -Sy --noconfirm git
#              git clone https://github.com/minorugh/dotfiles.git ~/src/github.com/minorugh/dotfiles
#              cd ~/src/github.com/minorugh/dotfiles/arch-install

########################################################
## 変数定義
########################################################
PACMAN     := sudo pacman -S --needed --noconfirm
AUR        := yay -S --needed --noconfirm

# 公式リポジトリで揃うもの
# gnupg はここで先に入れておく。env-import の `make gpg` 内の
# `sudo apt install gnupg` はArchでは失敗するが、.ONESHELLに set -e が
# 無いため後続のインポート処理には影響しない（詳細は下記 gpg ターゲット参照）
PACKAGES   := zsh vim emacs gnome-terminal git openssh keychain gnupg
PACKAGES   += base-devel sudo chromium

# AUR経由のみのもの（dropboxのみ）
AUR_PACKAGES := dropbox

ENV_IMPORT_DIR := $(HOME)/src/github.com/minorugh/env-import
# このMakefileは dotfiles/arch-install/ に置く前提。一つ上がdotfilesリポジトリ本体。
DOTFILES_DIR   := $(abspath $(CURDIR)/..)

.DEFAULT_GOAL := help

########################################################
## エントリーポイント
########################################################
.PHONY: all help base aur yay sudo-setup zsh-default
.PHONY: env-import-clone gpg env-restore ssh init switch-ssh push-block

help: ## ターゲット一覧を表示
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: base sudo-setup yay aur zsh-default ## base→sudo→yay→aur→zsh-default を一括実行
# 秘密鍵・SSH復元（gpg以降）はパスフレーズ入力や再起動を挟むため
# 手動で順番に実行すること（下の「SSH / dotfiles復元」セクション参照）

########################################################
## パッケージ導入
########################################################
base: ## 公式リポジトリの基本パッケージ一括インストール
	$(PACMAN) $(PACKAGES)

yay: ##! AURヘルパー(yay)をソースからビルドしてインストール
	sudo pacman -S --needed --noconfirm git base-devel
	rm -rf /tmp/yay
	git clone https://aur.archlinux.org/yay.git /tmp/yay
	cd /tmp/yay && makepkg -si --noconfirm
	rm -rf /tmp/yay

aur: ##! AUR経由でdropboxをインストール（yay導入後に実行、対話プロンプトあり）
	$(AUR) $(AUR_PACKAGES)
# 導入後、メニューからDropboxを起動して初期設定・同期を完了させること
# （このあとの env-restore は Dropbox 上の暗号化bundleを読むため、同期完了が前提）

########################################################
## システム設定
########################################################
sudo-setup: ## sudoグループ(wheel)の有効化
	sudo sed -i 's/^# %wheel ALL=(ALL:ALL) ALL/%wheel ALL=(ALL:ALL) ALL/' /etc/sudoers
	sudo usermod -aG wheel $$USER
	@echo "✓ wheelグループのsudoを有効化しました。反映には再ログインが必要です。"

zsh-default: ## ログインシェルをzshに変更
	sudo chsh -s /usr/bin/zsh $$USER
	@echo "✓ 次回ログインからzshが有効になります。"

########################################################
## SSH / dotfiles復元（Dropbox同期完了後、手動で順番に実行）
########################################################
env-import-clone: ## env-import リポジトリをHTTPSでclone（この時点ではまだSSHが無い）
	mkdir -p $(HOME)/src/github.com/minorugh
	git clone https://github.com/minorugh/env-import.git $(ENV_IMPORT_DIR)

gpg: env-import-clone ##! env-importのgpgターゲットを呼び出し秘密鍵をインポート（パスフレーズ入力あり）
	$(MAKE) -C $(ENV_IMPORT_DIR) gpg
# 内部で `sudo apt install gnupg` が失敗表示されるが、base で入れた gnupg で
# 後続のインポート処理自体は問題なく完走する（env-import は無改造のまま使用）

env-restore: ##! env-importのenv-restoreを呼び出し ~/.env_source を復元
	$(MAKE) -C $(ENV_IMPORT_DIR) env-restore

ssh: ## ~/.env_source からSSH鍵一式をシンボリックリンク
	mkdir -p $(HOME)/.ssh
	for item in config known_hosts id_rsa; do \
		ln -vsf $(HOME)/.env_source/.ssh/$$item $(HOME)/.ssh/$$item; \
	done
	chmod 600 $(HOME)/.ssh/id_rsa
	@echo "✓ SSH鍵を展開しました（xserverへは ssh xsrv などで接続可能）"

init: ## dotfilesのシンボリックリンク展開（emacs/zsh/vim/gitがすぐ使える）
# dotfiles本体は既にclone済み（このMakefile自身がその中にあるため）
	test -L $(HOME)/.emacs.d || rm -rf $(HOME)/.emacs.d
	ln -vsfn $(DOTFILES_DIR)/.emacs.d $(HOME)/.emacs.d
	for item in gitconfig zshrc vimrc; do \
		ln -vsf $(DOTFILES_DIR)/.$$item $(HOME)/.$$item; \
	done
	@echo "✓ dotfilesを展開しました（emacs/zsh/vim/gitがすぐ使えます）"
# トップレベルの baseinstall（apt決め打ち）は呼ばない。
# Arch側の基本環境は base/aur/sudo-setup/zsh-default で既に揃えている

switch-ssh: ##! HTTPS→SSHへgit remoteを切り替え（reboot後、SSH接続確認してから実行）
	$(MAKE) -C $(ENV_IMPORT_DIR) switch-ssh
# env-import側のswitch-sshが env-import・dotfiles 両方のremoteを切り替える
# （パスが ~/src/github.com/minorugh/{env-import,dotfiles} 固定のため、この構成と一致）

push-block: ## env-import・dotfiles ともpushを封鎖（サブ機と同じくpull専用で運用）
	git -C $(ENV_IMPORT_DIR) remote set-url --push origin no-push
	git -C $(DOTFILES_DIR) remote set-url --push origin no-push
	@echo "✓ push封鎖完了（pull専用）"

emacs-stable: ##! Emacs 安定版のソースビルド（pacman依存パッケージを事前インストール）
	sudo pacman -S --needed --noconfirm \
	  base-devel autoconf texinfo pkgconf \
	  gnutls jansson harfbuzz libxml2 gtk3 libgccjit \
	  ncurses sqlite tree-sitter \
	  libjpeg-turbo libpng giflib libtiff librsvg libxpm zlib
	mkdir -p ~/src
	cd ~/src && \
	wget -c https://ftp.gnu.org/gnu/emacs/emacs-31.1.tar.gz && \
	tar xvfz emacs-31.1.tar.gz && \
	cd emacs-31.1 && \
	./configure \
	  --with-native-compilation=aot \
	  --with-x-toolkit=gtk3 \
	  --without-xim \
	  --without-gsettings \
	  --without-dbus \
	  --without-toolkit-scroll-bars \
	  --without-imagemagick \
	  --without-mailutils \
	  --without-pop \
	  --without-gpm \
	  --without-selinux \
	  --without-compress-install && \
	NATIVE_FULL_AOT=1 make -j$(nproc) && \
	sudo make install
	@echo "## アンインストール: ~/src/emacs-30.2 で sudo make uninstall && make distclean"

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# タブ崩れなどの構文エラー確認（missing separator対策）
#   make -n >/dev/null
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
