### Debian GNU/Linux 環境復元用 dotfiles
# author Minoru Yamada. 2021.10.11
# update 2022.09.22 for Debian10
# update 2024.10.01 for Debian12
# update 2025.03.09 cleanup packages / sxiv→nsxiv移行メモ追加
# update 2026.03.10 SSH/keychain サブ機対応・コメント日本語化
# update 2026.03.18 Docker管理を docker/Makefile に分離
# update 2026.03.19 polkit ターゲット追加（Docker認証ダイアログ抑制）
# update 2026.03.26 filezilla/gitea バックアップ追加
# update 2026.03.26 keyring を全機共通コピー方式に統一、autostart.sh の条件分岐を削除
#
# make 実行前の手動準備手順は README.md を参照してください
# https://github.com/minorugh/dotfiles

ENV_SOURCE_DIR := $(HOME)/.env_source

HOSTNAME := $(shell hostname)
PACKAGES	:= hugo nkf wget curl file unar unzip gcc npm keychain smartmontools lm-sensors
PACKAGES += zsh-syntax-highlighting silversearcher-ag expect arc-theme
PACKAGES	+= pandoc rsync cmigemo e2ps evince net-tools wmctrl tig trash-cli
PACKAGES	+= ruby gnome-terminal xclip vim tmux xdotool
PACKAGES	+= autokey-gtk autokey-common lhasa fzf tree aspell aspell-en
PACKAGES	+= mosh xscreensaver xscreensaver-gl-extra nodejs sxiv
PACKAGES	+= menulibre pwgen xfce4-screenshooter bluetooth blueman gdebi
PACKAGES += gimp darktable inkscape shotwell cups cups-bsd vlc

BASE_PKGS	:= automake autoconf openssl build-essential
BASE_PKGS	+= libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev
BASE_PKGS	+= libgtk-3-dev libncurses-dev libgnutls28-dev libdbus-1-dev
BASE_PKGS	+= libtool flatpak python3-pip libnet-sftp-foreign-perl
BASE_PKGS	+= libxcb-shape0 libxcb-shm0 libxcb-xfixes0 libxcb-randr0 libxcb-image0
BASE_PKGS	+= libfontconfig1 libgl1-mesa-glx libxi6 libsm6 libxrender1 libpulse0

APT		:= sudo apt install -y
.DEFAULT_GOAL	:= help

.PHONY: all allinstall nextinstall
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: baseinstall nextinstall
baseinstall: env-setup ssh install base init init-sub keymap keyd grub autostart cron emacs-trash keyring tlp emacs-mozc icons gist fonts emacs-toggle
nextinstall: google-chrome filezilla gitk neomutt w3m abook sxiv lepton zoom printer

.ONESHELL:
SHELL = /bin/bash

env-setup: ## dotfiles/env/ のシンボリックリンク作成（~/.env_source から展開）
	ln -vsf ${ENV_SOURCE_DIR}/.netrc ${PWD}/env/.netrc
	ln -vsf ${ENV_SOURCE_DIR}/.ssh ${PWD}/env/.ssh
	ln -vsf ${ENV_SOURCE_DIR}/tokens ${PWD}/env/tokens
	cp -vf "${ENV_SOURCE_DIR}/info.md" "${PWD}/env/info.md"

ssh: ## SSH設定の初期化（~/.env_source から展開）
	mkdir -p ${HOME}/.$@
	for item in config known_hosts id_rsa xsrv; do
		ln -vsf ${ENV_SOURCE_DIR}/.ssh/$$item ${HOME}/.ssh/$$item
	done
	chmod 600 ${HOME}/.ssh/id_rsa

init: ## dotfiles のシンボリックリンク展開
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	for item in xprofile gitconfig bashrc zshrc vimrc tmux.conf Xresources textlintrc aspell.conf; do
		ln -vsf {${PWD},${HOME}}/.$$item
	done
	ln -vsf ${ENV_SOURCE_DIR}/tokens/hub ${HOME}/.config/hub

init-sub: ## サブ機のgit push封鎖（x250のみ実行）
ifeq ($(HOSTNAME),x250)
	git -C ~/src/github.com/minorugh/dotfiles remote set-url --push origin no-push
	@echo "サブ機push封鎖完了"
else
	@echo "これはサブ機専用の処理です"
endif

tig: ## tig の設定展開
	mkdir -p ${HOME}/.config/tig
	ln -vsf ${PWD}/.config/tig/config ${HOME}/.config/tig/config

keymap: ## キーマップのカスタマイズ（keyd + setxkbmap）
	ln -vsf {${PWD},${HOME}}/.Xmodmap
# .Xmodmap は参照用のみ（xmodmap は廃止・keyd + setxkbmap で管理）

keyd: ## keyd のインストールと設定（PrtSc→Alt_R / CapsLock→Ctrl）
	$(APT) git build-essential
	cd ${HOME}/src && \
	git clone https://github.com/rvaiya/keyd && \
	cd keyd && \
	make && \
	sudo make install
	sudo ln -vsf ${PWD}/etc/keyd/default.conf /etc/keyd/default.conf
	sudo systemctl enable keyd
	sudo systemctl start keyd

ifeq ($(shell uname -n),P1)
grub: ## grub・lightdm・logind の設定（P1のみ）
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
	sudo ln -vsf ${PWD}/etc/systemd/logind.conf /etc/systemd/logind.conf
	sudo ln -vsf ${PWD}/etc/default/grub /etc/default/grub
	sudo update-grub2
endif

go: ## Go 言語のインストール（v1.23.2）
	cd ${HOME}/Downloads && \
	wget https://go.dev/dl/go1.23.2.linux-amd64.tar.gz
	sudo rm -rf /usr/local/go
	sudo tar -C /usr/local -xzf go1.23.2.linux-amd64.tar.gz
	rm go1.23.2.linux-amd64.tar.gz
# PATH設定は .zshrc に記載済み（make init でシンボリックリンク展開後に有効）

ghq: ## ghq のインストール（リポジトリ管理ツール）
	go install github.com/x-motemen/ghq@latest
# go 1.23 以上が必要（make go を先に実行すること）

hugo: ## Hugo のインストール（extended版・GitHub releases経由）
	cd ${HOME}/Downloads && \
	wget https://github.com/gohugoio/hugo/releases/download/v0.147.0/hugo_extended_0.147.0_linux-amd64.deb
	sudo dpkg -i hugo_extended_0.147.0_linux-amd64.deb
	rm -f hugo_extended_0.147.0_linux-amd64.deb
# バージョンアップ時はURLの数字を変更すること
# 確認: hugo version

autostart: ## GUI起動時の自動処理設定（SSH鍵自動入力・mozc同期）
	ln -vsf {${PWD},${HOME}}/.autostart.sh
	chmod +x ${HOME}/.autostart.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/autostart.desktop

.PHONY: cron automerge autobackup
# F12キーでEmacsの最小化・復元をトグルする
emacs-toggle: ## emacs-toggle スクリプトのシンボリックリンク作成
	sudo ln -vsfn ${PWD}/bin/emacs-toggle /usr/local/bin/emacs-toggle
	sudo chmod +x /usr/local/bin/emacs-toggle
	xfconf-query -c xfce4-keyboard-shortcuts -p "/commands/custom/F12" -n -t string -s "emacs-toggle"

automerge: ## automerge.sh のシンボリックリンク作成（cron自動実行用、P1のみ）
	sudo ln -vsfn ${PWD}/cron/automerge.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/automerge.sh

autobackup: ## cron スクリプト群のシンボリックリンク作成（cron自動実行用、P1のみ）
	sudo ln -vsfn ${PWD}/backup/autobackup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/autobackup.sh
	sudo ln -vsfn ${PWD}/backup/mozc-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/mozc-backup.sh
	sudo ln -vsfn ${PWD}/backup/thunderbird-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/thunderbird-backup.sh
	sudo ln -vsfn ${PWD}/backup/filezilla-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/filezilla-backup.sh
	sudo ln -vsfn ${PWD}/backup/gitea-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/gitea-backup.sh
	sudo ln -vsfn ${PWD}/cron/xsrv-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/xsrv-backup.sh

emacs-trash: ## Emacs ゴミ箱スイープスクリプトのリンク作成
	sudo ln -vsfn ${PWD}/cron/emacs-trash-sweep.sh /usr/local/bin/emacs-trash-sweep.sh
	sudo chmod +x /usr/local/bin/emacs-trash-sweep.sh

cron: ## メイン機 (P1) のみ実行: automerge/autobackup のリンク作成 + crontab バックアップ＆反映
	@if [ "$$(hostname)" = "P1" ]; then \
		$(MAKE) automerge; \
		$(MAKE) autobackup; \
		BACKUP_FILE=${PWD}/cron/crontab.backup.$$(date +%Y%m%d); \
		crontab -l > $$BACKUP_FILE || true; \
		crontab ${PWD}/cron/crontab; \
	fi

xsrv-systemd: ## systemd-user で xsrv-backup を登録・有効化（P1のみ）
	@if [ "$$(hostname)" = "P1" ]; then \
		sudo ln -vsfn ${PWD}/cron/xsrv-backup.sh /usr/local/bin/xsrv-backup.sh; \
		sudo chmod +x /usr/local/bin/xsrv-backup.sh; \
		mkdir -p ${HOME}/.config/systemd/user; \
		ln -vsfn ${PWD}/.config/systemd/user/xsrv-backup.service \
		          ${HOME}/.config/systemd/user/xsrv-backup.service; \
		ln -vsfn ${PWD}/.config/systemd/user/xsrv-backup.timer \
		          ${HOME}/.config/systemd/user/xsrv-backup.timer; \
		systemctl --user daemon-reload; \
		systemctl --user enable --now xsrv-backup.timer; \
		echo "xsrv-backup timer enabled."; \
	else \
		echo "P1 以外では実行しません"; \
	fi

install: ## Debian パッケージの一括インストール
	$(APT) $(PACKAGES)

base: ## ビルド用ベースパッケージのインストール
	$(APT) $(BASE_PKGS)

emacs-mozc: ## Emacs + Mozc のインストール（Emacs本体もここで導入）
	$(APT) $@ fcitx-mozc
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsn ~/Dropbox/backup/mozc/.mozc ~/
# Debian12: emacs 29.1 がインストールされる
# 現在は emacs-stable/devel で自前ビルド(30.1)を使用中
# Debian13以降では emacs 30.1 が標準になる見込みのため自前ビルド不要になるかも
# fcitx の設定: ターミナルで im-config を実行 → Enter → fcitx を選択

tlp: ## 省電力・バッテリー劣化防止設定（TLP）
	$(APT) $@ tlp-rdw powertop
	sudo ln -vsf ${PWD}/etc/tlp.conf /etc/tlp.conf
	sudo tlp start
# TLP 1.4以降の正式設定ファイルは /etc/tlp.conf（旧 /etc/default/tlp は廃止）
# 充電しきい値の変更は dotfiles/cron/Makefile の bat-set-60 / bat-set-80 を使うこと

keyring: ## Gnome keyring の初期化（両機: Dropboxからコピー / P1: 夜間バックアップcron登録）
	$(APT) seahorse libsecret-tools
	test -L ${HOME}/.local/share/keyrings && rm -f ${HOME}/.local/share/keyrings || true
	mkdir -p ${HOME}/.local/share/keyrings
	rsync -av --delete ${HOME}/Dropbox/backup/keyrings/ ${HOME}/.local/share/keyrings/

icons: ## アイコン・壁紙のシンボリックリンク作成
	ln -vsf ${HOME}/Dropbox/backup/icons/* ${HOME}/Pictures

fonts: ## ユーザーフォントのシンボリックリンク作成
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.fonts

gist: ## gist コマンドのインストールと設定
	sudo gem install gist
	ln -vsf ${ENV_SOURCE_DIR}/tokens/gist ${HOME}/.gis

gitk: ## gitkのインストールと設定ファイル
	$(APT) $@
	mkdir -p ${HOME}/.config/git
	sudo ln -vsfn ${PWD}/.config/git/gitk ${HOME}/.config/git/gitk

printer: ## CUPS導入（driverless印刷を前提）
	sudo apt install -y cups
# Brother HL-L2375DW はドライバ不要（driverless対応）
# CUPSの自動検出 or http://localhost:631 から追加

## アプリケーションのインストール
google-chrome: ## Google Chrome のインストール
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
	$(APT) ./google-chrome-stable_current_amd64.deb
	rm -f ./google-chrome-stable_current_amd64.deb

filezilla: ## FileZilla のインストールと設定
	$(APT) $@
	rm -rf ${HOME}/.config/filezilla
	mkdir -p ${HOME}/.config/filezilla
	cp -a ${HOME}/Dropbox/backup/filezilla/config/. ${HOME}/.config/filezilla/
	chmod 700 ${HOME}/.config/filezilla
	mkdir -p ${HOME}/.local/bin
	ln -vsf {${PWD},${HOME}/.local}/bin/filezilla.sh
	chmod +x ${HOME}/.local/bin/filezilla.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/filezilla.desktop

thunderbird: ## Thunderbird の設定
# Gmail はOAuth2認証なので2段階認証設定必須
	@echo "[Thunderbird] Installing and restoring profile..."
	$(APT) $@
	mv ${HOME}/.thunderbird ${HOME}/.thunderbird.bak.$(date +%s) 2>/dev/null || true
	cp -a ${HOME}/Dropbox/backup/thunderbird/profile ${HOME}/.thunderbird
	sudo cp ${HOME}/Dropbox/backup/thunderbird/addons/external-editor-revived /usr/local/bin
	sudo chmod +x /usr/local/bin/external-editor-revived
	mkdir -p ${HOME}/.mozilla/native-messaging-hosts
	cp -a ${HOME}/Dropbox/backup/thunderbird/native-messaging-hosts/* \
	      ${HOME}/.mozilla/native-messaging-hosts/
	@echo "[Thunderbird] Done."

# External Editor アドオン（External Editor Revived）
# Debian 12 の GLIBC 2.36 では v1.2.0 バイナリが動かない（GLIBC 2.39 必要）
# バイナリは v1.1.0 のまま（~/Dropbox/backup/thunderbird/addons/external-editor-revived）
# xpi（拡張機能）は v1.2.0 のまま
# アドオン設定の Bypass version check にチェックを入れて Apply
# Debian 13（GLIBC 2.40）に上げれば v1.2.0 バイナリも動くようになる

neomutt: ## NeoMutt（IMAP read-only・検索／障害時の保険用）
# Thunderbirdが使えない場合でもIMAPから直接メール参照するために残す
# ※送信・削除しないread-only運用
	$(APT) $@
	mkdir -p ${HOME}/.mutt
	ln -vsf ${PWD}/.muttrc ${HOME}/.muttrc
	for item in password.gpg signature.gpg mailcap certificates dracula.muttrc nord.muttrc; do
		ln -vsf {${PWD},${HOME}}/.mutt/$$item
	done
	sudo ln -vsfn ${PWD}/bin/neomutt.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/neomutt.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/neomutt.desktop

w3m: ## NeoMutt補助（URL閲覧・簡易HTML表示）
	$(APT) $@
	mkdir -p ${HOME}/.w3m
	ln -vsfn {${PWD},${HOME}}/.w3m/keymap

abook: ## NeoMutt補助（簡易アドレス帳・検索用）
	$(APT) $@
	mkdir -p ${HOME}/.abook
	ln -vsf {${PWD},${HOME}}/.abook/addressbook

sxiv: ## sxiv の設定
	$(APT) $@
	mkdir -p ${HOME}/.config/sxiv/exec
	ln -vsf {${PWD},${HOME}}/.config/$@/exec/image-info && chmod +x $$_
# ファイル名に & などの特殊文字が含まれる場合は表示できないことがある
# その場合はファイル名を修正すること

lepton: ## Lepton（Gist クライアント）のインストール
	sudo apt update
	sudo apt install snapd
	sudo snap install snapd
	sudo snap install lepton
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/lepton.desktop

zoom: ## Zoom のインストール
	cd ${HOME}/Downloads && \
	wget https://zoom.us/client/latest/zoom_amd64.deb
	sudo gdebi zoom_amd64.deb
	rm -f ./zoom_amd64.deb

########################################################
## 以降はシェルと対話しながら個別に実行するターゲット
########################################################
dracula-theme: ## gnome-terminal 用 Dracula テーマのインストール
	cd ${HOME}/Downloads
	git clone https://github.com/dracula/gnome-terminal
	cd gnome-terminal && ./install.sh
	rm -fr ${HOME}/Downloads/gnome-terminal

keepassxc: ## KeePassXC のインストールと自動起動設定
	$(APT) $@ libsecret-tools
	sudo ln -vsfn ${PWD}/bin/keepass.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/keepass.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/keepass-auto.desktop
# 初回セットアップ時に一度だけ実行してパスワードを登録する
# | sudo secret-tool store --label "KeePassXC master password" type kbd
# パスワード入力を求められるのでユーザーパスワードを入力する
# 以降は以下のコマンドでパスワードなしで起動できる
# | secret-tool lookup type kdb | keepassxc --pw-stdin /path/to/keepassxc.kdb

google-earth: ## Google Earth のインストール
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/dl/earth/client/current/google-earth-pro-stable_current_amd64.deb
	$(APT) ./google-earth-pro-stable_current_amd64.deb
	rm -f ./google-earth-pro-stable_current_amd64.deb

slack: ## Slack デスクトップのインストール
	cd ${HOME}/Downloads && \
	wget https://slack.com/downloads/instructions/linux?ddl=1&build=deb
	sudo gdebi slack-desktop-4.47.69-amd64.deb # ファイル名はダウンロード時に確認すること
	rm -f slack-desktop-4.47.69-amd64.deb
	ln -vsf {${PWD},${HOME}}/.config/autostart/slack.desktop

spotify: ## Spotify クライアントのインストール
	curl -sS https://download.spotify.com/debian/pubkey_6224F9941A8AA6D1.gpg | sudo gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/spotify.gpg
	echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
	sudo apt update
	sudo apt install spotify-client

flatpak: ## flatpak 経由で Pinta をインストール
	$(APT) $@
	sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
	flatpak install flathub com.github.PintaProject.Pinta
## アンインストールする場合: flatpak uninstall --delete-data flathub com.spotify.Client

texlive: ## TeX Live のインストール（scheme-medium + 日本語）
	cd ${HOME}/Downloads && \
	wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
	tar xvf install-tl-unx.tar.gz && \
	rm -f install-tl-unx.tar.gz && \
	cd install-tl* && \
	sudo ./install-tl -no-gui \
		-scheme medium \
		-repository http://mirror.ctan.org/systems/texlive/tlnet/
	sudo /usr/local/texlive/2024/bin/x86_64-linux/tlmgr path add
	sudo tlmgr install collection-langjapanese
	sudo tlmgr update --self --all

texlive-full: ## TeX Live フルインストール（全パッケージ約7GB）
	cd ${HOME}/Downloads && \
	wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
	tar xvf install-tl-unx.tar.gz && \
	rm -f install-tl-unx.tar.gz && \
	cd install-tl* && \
	sudo ./install-tl -no-gui -repository http://mirror.ctan.org/systems/texlive/tlnet/
# Actions を聞かれたら `I` を入力してインストール開始
	sudo /usr/local/texlive/2024/bin/x86_64-linux/tlmgr path add
	sudo tlmgr update --self --all

latex: ## LaTeX 用スクリプト・スタイルファイルのシンボリックリンク作成
	sudo ln -vsfn ${PWD}/tex/dvpd.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/dvpd.sh
	sudo ln -vsfn ${PWD}/tex/platex/my-sty /usr/local/texlive/2024/texmf-dist/tex/platex
	sudo mktexlsr

emacs-stable: ## Emacs 安定版のソースビルド
	sudo apt build-dep emacs-gtk
	cd ~/src/
	wget http://mirrors.nav.ro/gnu/emacs/emacs-29.4.tar.xz
	tar xvf emacs-29.4.tar.xz
	cd emacs-29.4
	./autogen.sh
	./configure --with-native-compilation=aot
	make -j 4   # CPU コア数に合わせて数字を変更すること
	sudo make install

# アンインストールする場合はソースディレクトリで以下を実行する
# $ sudo make uninstall
# $ make clean
# $ make distclean

emacs-devel: ## Emacs 開発版のソースビルド
	cd ~/src/
	wget -c https://ftpmirror.gnu.org/emacs/emacs-30.1.tar.gz
	tar xvfz emacs-30.1.tar.gz
	cd emacs-30.1
	sudo apt build-dep emacs-gtk
	./autogen.sh
	./configure --with-native-compilation=aot && make -j8 && sudo make install && make clean
	rm -rf ${HOME}/.emacs.d/elpa  # elpa を初期化したくない場合はコメントアウトして実行

github: ## GitHub リポジトリのクローン
	mkdir -p ${HOME}/src/github.com/minorugh
	cd ${HOME}/src/github.com/minorugh
	git clone git@github.com:minorugh/GH.git
	git clone git@github.com:minorugh/minorugh.com.git
	git clone git@github.com:minorugh/minorugh.github.io.git
	git clone git@github.com:minorugh/upsftp.git
	git clone git@github.com:minorugh/env-import.git
	git clone git@github.com:minorugh/vim-cheat.git
	git clone git@github.com:minorugh/git-peek.git
	git clone git@github.com:minorugh/xsrv-GH.git
	git clone git@github.com:minorugh/xsrv-minorugh.git
# GH.git minorugh.com.git は .git のみ残して他は削除（本体は~/Dropbox）

########################################################
## Docker 環境セットアップ＆リストア手順
##
##   新規マシンへのリストア手順（順番に実行）:
##   Step 0: make docker-install  # Docker Engine インストール（初回のみ）
##   Step 1: make docker-setup    # ディレクトリ作成＋データ復元（polkit も自動実行）
##
########################################################
.PHONY: docker-install polkit docker-setup
docker-install: ## 【Step0】Docker Engine + Compose のインストール（公式リポジトリ経由）
	sudo apt update
	sudo apt install -y ca-certificates curl gnupg lsb-release
	sudo install -m 0755 -d /etc/apt/keyrings
	curl -fsSL https://download.docker.com/linux/debian/gpg \
		| sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
	sudo chmod a+r /etc/apt/keyrings/docker.gpg
	echo \
		"deb [arch=$$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] \
		https://download.docker.com/linux/debian \
		$$(. /etc/os-release && echo $$VERSION_CODENAME) stable" \
		| sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
	sudo apt update
	sudo apt install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
	sudo usermod -aG docker $$USER
	@echo "✓ Docker インストール完了。グループ反映には一度ログアウトが必要です。"
# インストール確認: docker --version && docker compose version

polkit: ## polkit認証ダイアログ抑制（docker-setup から自動実行）
	printf '%s\n' \
		'polkit.addRule(function(action, subject) {' \
		'    if (subject.user === "minoru") {' \
		'        return polkit.Result.YES;' \
		'    }' \
		'});' | sudo tee /etc/polkit-1/rules.d/49-nopasswd-docker.rules > /dev/null
	sudo systemctl restart polkit
	@echo "✓ polkit rules configured."

docker-setup: polkit ## 【Step1】データディレクトリ作成＋Dropboxからrsyncで復元
	mkdir -p /home/minoru/Docker/gitea/data
	rsync -av /home/minoru/Dropbox/backup/gitea/ /home/minoru/Docker/gitea/data/
	@echo "✓ 完了。次: make gitea（docker/Makefile）"

## ====================================================================
## Debian インストール後の手動カスタマイズ
## ====================================================================
# 1. Caps キーを Ctrl に置き換える>> `sudo nano /etc/default/keyboard' && XKBOPTIONS="ctrl:nocaps" に編集して再起動
# 2. ウィンドウマネージャー（設定マネージャー）>> スタイル: Arc-Dark、キーボード編集-> ウィンドウの切り替え (Super+Alt)、アプリケーションの切り替え (Ctrl+Super)、ウィンドウの非表示 (Alt+f9 → End キー)
# 3. 外観設定>> スタイル: Arc-Dark、フォントサイズ: 14
# 4. 印刷設定>> `sudo system-config-printer' で設定
# 5. キーボード設定>> emacs:s-e, chrome:s-x, gnome-terminal:C-z, xfce4-screenshooter -r:Alt+Shift, xfce4-screenshooter -w:Alt+Ctrl
# 6. スクリーンセーバー>> Atlantis / Only One モードを選択
# 7. セッションと起動>> 自動起動アプリに .autostart.sh を追加（最小化起動）
# 8. セッションと起動>> 一般タブの「ログアウト時にセッションを自動保存」のチェックを外す
# 9. パネルのアクションボタン>> ログアウト時の「セッションを保存する」のチェックを外す


# Hostname-based git push control:
# P1 (main): commit + push / others (sub): pull --rebase only
# Note: Also synchronizes the secret '~/.env_source' repository on P1.
git: ## Auto commit+push (main only, sub: pull only)
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"

ifeq ($(HOSTNAME),P1)
	if [ -f $(ENV_SOURCE_DIR)/Makefile ]; then \
		$(MAKE) -C $(ENV_SOURCE_DIR) git; \
	fi
	git push
else
	@echo "$(HOSTNAME): サブ機からはpushしません（pullのみ）"
	git pull --rebase
endif

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# タブ崩れなどの構文エラー確認（missing separator対策）
#   make -n >/dev/null
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
