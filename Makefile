### Debian GNU/Linux 環境復元用 dotfiles
# author Minoru Yamada. 2021.10.11
# update 2022.09.22 for Debian10
# update 2024.10.01 for Debian12
# update 2025.03.09 cleanup packages / sxiv→nsxiv移行メモ追加
# update 2026.03.10 SSH/keychain サブ機対応・コメント日本語化
#
# make 実行前の手動準備手順は README.md を参照してください
# https://github.com/minorugh/dotfiles
PACKAGES	:= hugo nkf wget curl file unar unzip gcc npm keychain
PACKAGES	+= zsh-syntax-highlighting silversearcher-ag expect arc-theme
PACKAGES	+= pandoc rsync cmigemo e2ps evince net-tools wmctrl
PACKAGES	+= ruby gnome-terminal xclip vim tmux xdotool
PACKAGES	+= autokey-gtk autokey-common lhasa fzf tree aspell aspell-en
PACKAGES	+= mosh xscreensaver xscreensaver-gl-extra nodejs sxiv
PACKAGES	+= menulibre pwgen xfce4-screenshooter bluetooth blueman gdebi
PACKAGES	+= gimp darktable inkscape shotwell cups cups-bsd vlc

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
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: baseinstall nextinstall
baseinstall: ssh install base init keymap grub autostart myjob keyring tlp emacs-mozc icons gist fonts
nextinstall: google-chrome filezilla gitk neomutt w3m abook sxiv lepton zoom printer

.ONESHELL:
SHELL = /bin/bash

ssh: ## SSH設定の初期化
	mkdir -p ${HOME}/.$@
	for item in config known_hosts id_rsa xsrv; do
		ln -vsfn {${PWD},${HOME}}/.ssh/$$item
	done
	chmod 600 ${HOME}/.ssh/id_rsa

init: ## dotfiles のシンボリックリンク展開
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	for item in xprofile gitconfig bashrc zshrc vimrc tmux.conf Xresources textlintrc aspell.conf; do
		ln -vsf {${PWD},${HOME}}/.$$item
	done
	ln -vsf {${PWD},${HOME}}/.config/hub

keymap: ## キーマップのカスタマイズ（Xmodmap）
	ln -vsf {${PWD},${HOME}}/.Xmodmap

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
# export PATH=$PATH:/usr/local/go/bin を .zshrc に追加すること

ghq: ## ghq のインストール（リポジトリ管理ツール）
	go install github.com/x-motemen/ghq@latest
# go 1.23 以上が必要
# export PATH=$PATH:$HOME/go/bin を .zshrc に追加すること

autostart: ## GUI起動時の自動処理設定（SSH鍵自動入力・mozc同期）
	ln -vsf {${PWD},${HOME}}/.autostart.sh
	chmod +x ${HOME}/.autostart.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/autostart.desktop

myjob: ## myjob.sh のシンボリックリンク作成（cron自動実行用）
	sudo ln -vsfn ${PWD}/cron/myjob.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/myjob.sh

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
	sudo ln -vsf ${PWD}/etc/default/tlp /etc/default/tlp
	sudo tlp start

keyring: ## Gnome keyring の初期化（P1: Dropboxシンボリックリンク / サブ機: リンク削除のみ）
	$(APT) seahorse libsecret-tools
# 親機: Dropbox/backup/keyrings へのシンボリックリンク（正本）
ifeq ($(shell uname -n),P1)
	test -L ${HOME}/.local/share/keyrings || rm -rf ${HOME}/.local/share/keyrings
	ln -vsfn {${HOME}/Dropbox/backup,${HOME}/.local/share}/keyrings
# サブ機: シンボリックリンクが残っていれば削除するだけ
# コピーは起動時に autostart.sh が行う（Dropbox競合防止）
else
	test -L ${HOME}/.local/share/keyrings && rm -f ${HOME}/.local/share/keyrings || true
endif
# SSH鍵パスフレーズ登録は P1 で一度だけ実行済み（Dropbox経由でサブ機にも反映）
# ※ secret-tool store は両マシンで同時実行しないこと（Dropbox競合の原因になる）

icons: ## アイコン・壁紙のシンボリックリンク作成
	ln -vsf ${HOME}/Dropbox/backup/icons/* ${HOME}/Pictures

fonts: ## ユーザーフォントのシンボリックリンク作成
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.fonts

gist: ## gist コマンドのインストールと設定
	sudo gem install gist
	ln -vsf ${HOME}/Dropbox/backup/gist/gist ${HOME}/.gist

gitk: ## gitk の設定
	$(APT) $@
	mkdir -p ${HOME}/.config/git
	sudo ln -vsfn ${HOME}/Dropbox/backup/gist/gitk ${HOME}/.config/git/gitk

printer: ## プリンタードライバーのインストール（Brother HL-L2375DW）
	cd ${HOME}/Downloads && \
	wget https://download.brother.com/welcome/dlf103535/hll2375dwpdrv-4.0.0-1.i386.deb
	sudo dpkg -i --force-all hll2375dwpdrv-4.0.0-1.i386.deb
	rm -f ./hll2375dwpdrv-4.0.0-1.i386.deb

## アプリケーションのインストール
google-chrome: ## Google Chrome のインストール
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
	$(APT) ./google-chrome-stable_current_amd64.deb
	rm -f ./google-chrome-stable_current_amd64.deb

filezilla: ## FileZilla のインストールと設定
	$(APT) $@
	test -L ${HOME}/.config/filezilla || rm -rf ${HOME}/.config/filezilla
	ln -vsfn {${HOME}/Dropbox/backup,${HOME}/.config}/filezilla
	chmod 700 ~/.config/filezilla
	mkdir -p ${HOME}/.local/bin
	ln -vsf {${PWD},${HOME}/.local}/bin/filezilla.sh
	chmod +x ${HOME}/.local/bin/filezilla.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/filezilla.desktop

neomutt: ## neomutt メールクライアントの設定
	$(APT) $@
	mkdir -p ${HOME}/.mutt
	ln -vsf ${PWD}/.muttrc ${HOME}/.muttrc
	for item in password.rc signature mailcap certificates dracula.muttrc nord.muttrc; do
		ln -vsf {${PWD},${HOME}}/.mutt/$$item
	done
	sudo ln -vsfn ${PWD}/bin/neomutt.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/neomutt.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/neomutt.desktop

w3m: ## w3m のインストールと設定
	$(APT) $@
	mkdir -p ${HOME}/.w3m
	ln -vsfn {${PWD},${HOME}}/.w3m/keymap

abook: ## abook アドレス帳のインストールと設定
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
rclone: ## rclone の設定
	sudo -v ; curl https://rclone.org/install.sh | sudo bash
	test -L ${HOME}/.config/rclone || rm -rf ${HOME}/.config/rclone
	ln -vsfn ${HOME}/Dropbox/backup/config/rclone ${HOME}/.config/rclone
	chmod 600 ${HOME}/.config/rclone/rclone.conf

rdp: ## Windows へのリモートデスクトップ接続設定
	$(APT) freerdp2-x11
	sudo ln -vsf {${PWD},/usr/local}/bin/freerdp.sh
	sudo chmod 600 /usr/local/bin/freerdp.sh
	ln -vsf {${PWD},${HOME}}/.local/share/applications/freerdp.desktop

devilspie: ## devilspie の設定（起動時ウィンドウ最小化）
	mkdir -p ${HOME}/.devilspie
	$(APT) $@
	sudo ln -vsfn ${PWD}/devils/emacs.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/sylpheed.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/devils_startup.sh  /usr/local/bin
	sudo chmod +x /usr/local/bin/devils_startup.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/devils_startup.desktop

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

thunderbird: ## Thunderbird の設定（Gmail はアプリパスワードで認証）
# サブ機など既存インストールがある場合は事前に手動で削除すること
# | sudo apt remove --purge thunderbird && rm -rf ~/.thunderbird
	$(APT) $@
	mkdir -p ${HOME}/.thunderbird/r5oy09ua.default-default
	$(eval TBIRD := ${HOME}/Dropbox/backup/thunderbird)
	$(eval PROF  := ${HOME}/.thunderbird/r5oy09ua.default-default)
	ln -vsf ${TBIRD}/profiles.ini ${HOME}/.thunderbird/profiles.ini
	ln -vsf ${TBIRD}/installs.ini ${HOME}/.thunderbird/installs.ini
	for item in prefs.js cert9.db key4.db logins.json; do \
		ln -vsf ${TBIRD}/$$item ${PROF}/$$item; \
	done
	sudo ln -vsfn ${HOME}/Dropbox/backup/thunderbird/addons/external-editor-revived /usr/local/bin
	sudo chmod +x /usr/local/bin/external-editor-revived

# External Editor アドオン（External Editor Revived）
# アドオン本体（v1.2.0）
# https://github.com/Frederick888/external-editor-revived/releases/download/v1.2.0/external-editor-revived-v1.2.0.xpi
# ネイティブメッセージングホストは v1.1.0 を使用すること（v1.2 はエラーになる）
# https://github.com/Frederick888/external-editor-revived/releases/download/v1.1.0/ubuntu-latest-gnu-native-messaging-host-v1.1.0.zip


google-earth: ## Google Earth のインストール
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/dl/earth/client/current/google-earth-pro-stable_current_amd64.deb
	$(APT) ./google-earth-pro-stable_current_amd64.deb
	rm -f ./google-earth-pro-stable_current_amd64.deb

slack: ## Slack デスクトップのインストール
	cd ${HOME}/Downloads && \
	wget https://slack.com/downloads/instructions/linux?ddl=1&build=deb
	sudo gdebi slack-desktop-4.39.95-amd64.deb # ファイル名はダウンロード時に確認すること
	rm -f slack-desktop-4.39.95-amd64.deb
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
	# rm -rf ${HOME}/.emacs.d/elpa  # elpa を初期化したい場合のみコメントを外して実行

github: ## GitHub リポジトリのクローン
	mkdir -p ${HOME}/src/github.com/minorugh
	cd ${HOME}/src/github.com/minorugh
	git clone git@github.com:minorugh/GH.git
	git clone git@github.com:minorugh/upsftp.git
	git clone git@github.com:minorugh/evil-easy-hugo.git
	git clone git@github.com:minorugh/minorugh.github.io.git
# GH.git は .git のみ残して他のファイルは削除して使用する


## ====================================================================
## Debian インストール後の手動カスタマイズ
## ====================================================================
# 1. Caps キーを Ctrl に置き換える>> `sudo nano /etc/default/keyboard' && XKBOPTIONS="ctrl:nocaps" に編集して再起動
# 2. ウィンドウマネージャー（設定マネージャー）>> スタイル: Arc-Dark、キーボード編集-> ウィンドウの切り替え (Super+Alt)、アプリケーションの切り替え (Ctrl+Super)、ウィンドウの非表示 (Alt+f9 → End キー)
# 3. 外観設定>> スタイル: Arc-Dark、フォントサイズ: 14
# 4. 印刷設定>> `sudo system-config-printer' で設定
# 5. キーボード設定>> emacs:s-e, chrome:s-x, gnome-terminal:C-z, xfce4-screenshooter -r:Alt+Shift, xfce4-screenshooter -w:Alt+Ctrl
# 6. スクリーンセーバー>> Atlantis / Only One モードを選択
# 7. セッションと起動>> 自動起動アプリに devils_startup.sh を追加（最小化起動）
# 8. セッションと起動>> 一般タブの「ログアウト時にセッションを自動保存」のチェックを外す
# 9. パネルのアクションボタン>> ログアウト時の「セッションを保存する」のチェックを外す
