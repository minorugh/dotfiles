### Dotfiles to restore Debian GNU/Linux
# author Minoru Yamada. 2021.10.11
# update 2022.09.22 for Debian10
# update 2024.10.01 for Debian12
# update 2025.03.09 cleanup packages / sxiv→nsxiv移行メモ追加

## =====================================================================
## Manual setting before executing make
## =====================================================================
## 1. Boot from USB to install Debian latest
# Create installation USB from netinst iso image. Use Rufs.exe on Windows
# rufs https://rufus.ie/ja/

## 2. Register username to sudoers
# Log in as root
# | gpasswd -a ${USER} sudo
# Edit sudoers as root directly (DO NOT use sudo here, not yet available)
# | visudo
# Add and edit correction as follows
# ----------------------------------
# # User privilege specification
# root    ALL=(ALL:ALL) ALL
# minoru ALL=(ALL:ALL) NOPASSWD:ALL
# # Allow members of group sudo to execute any command
# %sudo   ALL=(ALL:ALL) NOPASSWD:ALL
# ----------------------------------

## 3. Set home sub directorys to English notation
# Log in with ${USER}
# | sudo apt install -y xdg-user-dirs-gtk ## Not needed for debian12 or later
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y make git nautilus

## 4. Install dropbox & setting
# Before installing, configure the Synapyic repository & Check existence of package
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and initial settings

## 5. Import GPG private_key
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone git@github.com:minorugh/gpgimport.git
# | cd gpgimport
# | make gpg

## 6. Clone dotfiles from GitHub
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone git@github.com:minorugh/dotfiles.git
# | cd dotfiles
# | git-crypt unlock
# | make all

## 7. Change shell to zsh
# | chsh -s /usr/bin/zsh


## =======================================================================
## Run make from here
## =======================================================================
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

all: allinstall nextinstall
allinstall: ssh install base init keymap grub autostart myjob keyring tlp emacs-mozc icons gist fonts
nextinstall: google-chrome filezilla gitk neomutt w3m abook sxiv lepton zoom printer

.ONESHELL:
SHELL = /bin/bash

ssh: ## Init ssh
	mkdir -p ${HOME}/.$@
	for item in config known_hosts id_rsa xsrv; do
		ln -vsfn {${PWD},${HOME}}/.ssh/$$item
	done
	chmod 600 ${HOME}/.ssh/id_rsa

init: ## Initial deploy dotfiles
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	for item in xprofile gitconfig bashrc zshrc vimrc tmux.conf Xresources textlintrc aspell.conf; do
		ln -vsf {${PWD},${HOME}}/.$$item
	done
	ln -vsf {${PWD},${HOME}}/.config/hub

keymap: ## Custom Keymap
	ln -vsf {${PWD},${HOME}}/.Xmodmap

ifeq ($(shell uname -n),P1)
grub: ## Configure grub
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
	sudo ln -vsf ${PWD}/etc/systemd/logind.conf /etc/systemd/logind.conf
	sudo ln -vsf ${PWD}/etc/default/grub /etc/default/grub
	sudo update-grub2
endif

go: ## Instll go for version 1.23.2 see https://go.dev/doc/install
	cd ${HOME}/Downloads && \
	wget https://go.dev/dl/go1.23.2.linux-amd64.tar.gz
	sudo rm -rf /usr/local/go
	sudo tar -C /usr/local -xzf go1.23.2.linux-amd64.tar.gz
	rm go1.23.2.linux-amd64.tar.gz
# 'export PATH=$PATH:/usr/local/go/bin'

ghq: ## Install ghq see https://github.com/x-motemen/ghq
	go install github.com/x-motemen/ghq@latest
# go version needs to be 1.23 or higher.
# 'export PATH=$PATH:$HOME/go/bin'

autostart: ## Run mozc copy for submachine & SSH key auto-input at GUI startup
	ln -vsf {${PWD},${HOME}}/.autostart.sh
	chmod +x ${HOME}/.autostart.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/autostart.desktop

myjob: ## Symbolic link for myjob.sh to be run automatically via cron
	sudo ln -vsfn ${PWD}/cron/myjob.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/myjob.sh

install: ## Install debian packages
	$(APT) $(PACKAGES)

base: ## Install debian base packages
	$(APT) $(BASE_PKGS)

emacs-mozc:  ## Install emacs-mozc fcitx-mozc (emacs本体もここでインストールされる)
	$(APT) $@ fcitx-mozc
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsfn -rf ~/Dropbox/backup/mozc/.mozc ~/
# Debian12: emacs 29.1 がインストールされる
# 現在は emacs-stable/devel で自前ビルド(30.1)を使用中
# Debian13以降では emacs 30.1 が標準になる見込みのため自前ビルド不要になるかも
# Set fcitx: Input im-config in terminal and ret → ret → check to fcitx

tlp: ## Setting for power saving and preventing battery deterioration
	$(APT) $@ tlp-rdw powertop
	sudo ln -vsf ${PWD}/etc/default/tlp /etc/default/tlp
	sudo tlp start

keyring: ## Init gnome keyring (P1: Dropbox symlink / サブ機: 実ディレクトリ+autostart でコピー)
	$(APT) seahorse libsecret-tools
# ifeq ($(shell uname -n),P1)
	# 親機: Dropbox/backup/keyrings へのシンボリックリンク（正本）
	test -L ${HOME}/.local/share/keyrings || rm -rf ${HOME}/.local/share/keyrings
	ln -vsfn {${HOME}/Dropbox/backup,${HOME}/.local/share}/keyrings
# else
# 	# サブ機: 実ディレクトリに変更（起動時に autostart.sh が親機から上書きコピー）
# 	# → Dropbox 同時起動による競合コピー防止
# 	rm -rf ${HOME}/.local/share/keyrings
# 	mkdir -p ${HOME}/.local/share/keyrings
# 	cp ${HOME}/Dropbox/backup/keyrings/Default_keyring.keyring \
# 	   ${HOME}/.local/share/keyrings/Default_keyring.keyring
endif
# SSH鍵パスフレーズ登録は P1 で一度だけ実行済み（Dropbox経由でサブ機にも反映）
# ※ secret-tool store は両マシンで同時実行しないこと（Dropbox競合の原因になる）

icons: ## Copy Collected icons & wallpaper to picture folder
	ln -vsf ${HOME}/Dropbox/backup/icons/* ${HOME}/Pictures

fonts: ## Symlink for user fonts
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.fonts

gist: ## Install gist for use gist-command from shell
	sudo gem install gist
	ln -vsf ${HOME}/Dropbox/backup/gist ${HOME}/.gist

printer: ## Install Printer driver for Brother HL-L2375DW
	cd ${HOME}/Downloads && \
	wget https://download.brother.com/welcome/dlf103535/hll2375dwpdrv-4.0.0-1.i386.deb
	sudo dpkg -i --force-all hll2375dwpdrv-4.0.0-1.i386.deb
	rm -f ./hll2375dwpdrv-4.0.0-1.i386.deb

## install for applications
google-chrome: ## Install Google-chrome-stable
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
	$(APT) ./google-chrome-stable_current_amd64.deb
	rm -f ./google-chrome-stable_current_amd64.deb

filezilla:  ## Install filezilla and set "Filezilla -s" to start selected my:servers
	$(APT) $@
	test -L ${HOME}/.config/filezilla || rm -rf ${HOME}/.config/filezilla
	ln -vsfn {${HOME}/Dropbox/backup,${HOME}/.config}/filezilla
	chmod 700 ~/.config/filezilla
	mkdir -p ${HOME}/.local/bin
	ln -vsf {${PWD},${HOME}/.local}/bin/filezilla.sh
	chmod +x ${HOME}/.local/bin/filezilla.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/filezilla.desktop

neomutt: ## Init neomutt mail client with abook
	$(APT) $@
	mkdir -p ${HOME}/.mutt
	ln -vsf ${PWD}/.muttrc ${HOME}/.muttrc
	for item in password.rc signature mailcap certifcates dracula.muttrc nord.muttrc; do
		ln -vsf {${PWD},${HOME}}/.mutt/$$item
	done
	sudo ln -vsfn ${PWD}/bin/neomutt.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/neomutt.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/neomutt.desktop

w3m: ## Install w3m
	$(APT) $@
	mkdir -p ${HOME}/.w3m
	ln -vsfn {${PWD},${HOME}}/.w3m/keymap

abook: ## Install Abook
	$(APT) $@
	mkdir -p ${HOME}/.abook
	ln -vsf {${PWD},${HOME}}/.abook/addressbook

gitk: ## Init gitk for git-gui
	$(APT) $@
	mkdir -p ${HOME}/.config/git
	sudo ln -vsfn {${PWD},${HOME}}/.config/git/gitk

sxiv: ## Init sxiv (current)
	$(APT) $@
	mkdir -p ${HOME}/.config/sxiv/exec
	ln -vsf {${PWD},${HOME}}/.config/$@/exec/image-info && chmod +x $$_
# ファイル名に & などの特殊文字が含まれる場合は表示できないことがある
# その場合はファイル名を修正すること

nsxiv: ## Build and install nsxiv (sxiv successor) with Imlib2 from source
# Debian12の公式Imlib2はv1.10.0止まりでnsxiv最新版(v34)に対応していない
# Imlib2をソースビルドしてからnsxivをビルドする（時間がかかる）
# Step1: Imlib2のビルド依存パッケージをインストール
	$(APT) libx11-dev libxext-dev zlib1g-dev libjpeg-dev libpng-dev \
		libfreetype6-dev libfontconfig1-dev libid3tag0-dev \
		libexif-dev libtiff-dev libgif-dev
# Step2: Imlib2をソースビルド
	cd ${HOME}/src && \
	wget https://downloads.sourceforge.net/project/enlightenment/imlib2-src/1.12.3/imlib2-1.12.3.tar.xz && \
	tar xvf imlib2-1.12.3.tar.xz && \
	cd imlib2-1.12.3 && \
	./configure --prefix=/usr/local && \
	make -j4 && sudo make install
# Step3: nsxivをソースビルド
	cd ${HOME}/src && \
	git clone https://github.com/nsxiv/nsxiv.git && \
	cd nsxiv && \
	PKG_CONFIG_PATH=/usr/local/lib/pkgconfig make && \
	sudo make install
# Step4: 設定ディレクトリとimage-infoをセットアップ
	mkdir -p ${HOME}/.config/nsxiv/exec
	ln -vsf ${PWD}/.config/sxiv/exec/image-info ${HOME}/.config/nsxiv/exec/image-info
	chmod +x ${HOME}/.config/nsxiv/exec/image-info
# Step5: .zshrcのaliasを iv='nsxiv' に変更すること

lepton: ## Install lepton
	sudo apt update
	sudo apt install snapd
	sudo snap install snapd
	sudo snap install lepton
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/lepton.desktop

zoom: ## Install zoom
	cd ${HOME}/Downloads && \
	wget https://zoom.us/client/latest/zoom_amd64.deb
	sudo gdebi zoom_amd64.deb
	rm -f ./zoom_amd64.deb

########################################################
## From here, Step by step while interacting with SHELL
########################################################
rclone: ## Init rclone
	sudo -v ; curl https://rclone.org/install.sh | sudo bash
	test -L ${HOME}/.config/rclone || rm -rf ${HOME}/.config/rclone
	ln -vsfn ${HOME}/Dropbox/backup/config/rclone ${HOME}/.config/rclone
	chmod 600 ${HOME}/.config/rclone/rclone.conf

rdp: ## RDP Connection to Windows
	$(APT) freerdp2-x11
	sudo ln -vsf {${PWD},/usr/local}/bin/freerdp.sh
	sudo chmod 600 /usr/local/bin/freerdp.sh
	ln -vsf {${PWD},${HOME}}/.local/share/applications/freerdp.desktop

devilspie: ## Init devilspie for minimize_startup applications
	mkdir -p ${HOME}/.devilspie
	$(APT) $@
	sudo ln -vsfn ${PWD}/devils/emacs.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/sylpheed.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/devils_startup.sh  /usr/local/bin
	sudo chmod +x /usr/local/bin/devils_startup.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/devils_startup.desktop

dracula-theme: ## Install dracula theme for gnome-terminal
	cd ${HOME}/Downloads
	git clone https://github.com/dracula/gnome-terminal
	cd gnome-terminal && ./install.sh
	rm -fr ${HOME}/Downloads/gnome-terminal

keepassxc: ## Install keeypassXC and auto start with master passwd.
	$(APT) $@ libsecret-tools
	sudo ln -vsfn ${PWD}/bin/keepass.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/keepass.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/keepass-auto.desktop
# select-tool setup at first
# | sudo secret-tool store --label "KeePassXC master password" type kbd
# asked for a password so enter
# popup panel for passward input so input '<user passwd>' && use shell
# use | $ secret-tool lookup type kdb | keepassxc --pw-stdin /path/to/keepassxc.kdb

thunderbird: ## Init thunderbird（for gmail Use App Password for authentication）
	$(APT) $@
	test -L ${HOME}/.thunderbird || rm -rf ${HOME}/.thunderbird
	ln -vsfn ${HOME}/Dropbox/thunderbird/.thunderbird ${HOME}/.thunderbird
	sudo ln -vsfn ${HOME}/Dropbox/thunderbird/external-editor-revived /usr/local/bin
	sudo chmod +x /usr/local/bin/external-editor-revived

# External Editor Addon External Editor Revived
# Main is here
# https://github.com/Frederick888/external-editor-revived/releases/download/v1.2.0/external-editor-revived-v1.2.0.xpi
# native messaging host v1.1.0 (an error will occur if it is v1.2)
# https://github.com/Frederick888/external-editor-revived/releases/download/v1.1.0/ubuntu-latest-gnu-native-messaging-host-v1.1.0.zip


google-earth: ## Install google-earth
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/dl/earth/client/current/google-earth-pro-stable_current_amd64.deb
	$(APT) ./google-earth-pro-stable_current_amd64.deb
	rm -f ./google-earth-pro-stable_current_amd64.deb

slack: ## Install slack desktop
	cd ${HOME}/Downloads && \
	wget https://slack.com/downloads/instructions/linux?ddl=1&build=deb
	sudo gdebi slack-desktop-4.39.95-amd64.deb # Filename must be confirmed each time
	rm -f slack-desktop-4.39.95-amd64.deb
	ln -vsf {${PWD},${HOME}}/.config/autostart/slack.desktop

spotify: ## Install sptify client
	curl -sS https://download.spotify.com/debian/pubkey_6224F9941A8AA6D1.gpg | sudo gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/spotify.gpg
	echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
	sudo apt update
	sudo apt install spotify-client

flatpak: ## Install Pinta from flatpak
	$(APT) $@
	sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
	flatpak install flathub com.github.PintaProject.Pinta
## uninstall for package 'flatpak uninstall --delete-data flathub com.spotify.Client'

texlive: ## Install texlive (scheme-medium + Japanese)
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

texlive-full: ## Install texlive full (all packages ~7GB)
	cd ${HOME}/Downloads && \
	wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
	tar xvf install-tl-unx.tar.gz && \
	rm -f install-tl-unx.tar.gz && \
	cd install-tl* && \
	sudo ./install-tl -no-gui -repository http://mirror.ctan.org/systems/texlive/tlnet/
# Asked for Actions, so enter `I` to start the installation
	sudo /usr/local/texlive/2024/bin/x86_64-linux/tlmgr path add
	sudo tlmgr update --self --all

latex: ## Symbolic link for dvpd.sh && mysty
	sudo ln -vsfn ${PWD}/tex/dvpd.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/dvpd.sh
	sudo ln -vsfn ${PWD}/tex/platex/my-sty /usr/local/texlive/2024/texmf-dist/tex/platex
	sudo mktexlsr

emacs-stable: ## Install stable version of emacs
	sudo apt build-dep emacs-gtk
	cd ~/src/
	wget http://mirrors.nav.ro/gnu/emacs/emacs-29.4.tar.xz
	tar xvf emacs-29.4.tar.xz
	cd emacs-29.4
	./autogen.sh
	./configure --with-native-compilation=aot
	make -j 4   # Replace the number 4 with the number of cores that your CPU has.
	sudo make install

# Uninstallation
# Change directory to the folder containing the source and perform the following steps:
# $ sudo make uninstall
# $ make clean
# $ make distclean

emacs-devel: ## Install development version of emacs
	cd ~/src/
	wget -c https://ftpmirror.gnu.org/emacs/emacs-30.1.tar.gz
	tar xvfz emacs-30.1.tar.gz
	cd emacs-30.1
	sudo apt build-dep emacs-gtk
	./autogen.sh
	./configure --with-native-compilation=aot && make -j8 && sudo make install && make clean
	# rm -rf ${HOME}/.emacs.d/elpa

github: ## Clone github repository
	mkdir -p ${HOME}/src/github.com/minorugh
	cd ${HOME}/src/github.com/minorugh
	git clone git@github.com:minorugh/GH.git
	git clone git@github.com:minorugh/upsftp.git
	git clone git@github.com:minorugh/evil-easy-hugo.git
	git clone git@github.com:minorugh/minorugh.github.io.git
# GH.git will keep only `.git' and delete all other files.


## =====================================================================
## Custmize settings after Debian install
## =====================================================================
# 1. Replace key Caps with Ctrl>> `sudo nano /etc/default/keyboard' && edit to XKBOPTIONS="ctrl:nocaps" then reboot
# 2. Window Manager(in setting manager)>> style-> Arc-Dark, edit keyboard-> switch windows (Super+Alt), switch applications (Ctrl+Super), hide window (Alt+f9 to End key)
# 3. Exterior setting>> select style:Arc-Dark, font size:14
# 4. Print setting >> edit command: `sudo system-config-printer'
# 5. Keyboad setting>> emacs:s-e, chrome:s-x, gnome-terminal:C-z, xfce4-screenshooter -r:Alt+Shift, xfce4-screenshooter -w:Alt+Ctrl
# 6. Screen-saver>> select Atlantis with Only One mode
# 7. Session & launch>> Add minimized startup for command:devils_startup.sh
# 8. Session & launch>> Uncheck "Save Session Self-Recyclically on Logout" on general Tags.
# 9. Action button on panel>> Uncheck "Save session for next login" on Logout selection panel.

## ====================================================================
## Debian インストール後に設定をカスタマイズする
## ====================================================================
# 1. Capsキーを Ctrl に置き換えます>> `sudo nano /etc/default/keyboard' && XKBOPTIONS="ctrl:nocaps" に編集して再起動します
# 2. ウィンドウマネージャー(設定マネージャー内)>> スタイル-> Arc-Dark、キーボード編集-> ウィンドウの切り替え (Super+Alt)、アプリケーションの切り替え (Ctrl+Super)、ウィンドウの非表示 (Alt+f9 から End キー)
# 3. 外部設定 >> スタイル:Arc-Dark、フォントサイズ:14 を選択
# 4. 印刷設定 >> コマンド編集: `sudo system-config-printer'
# 5. キーボード設定>> emacs:s-e, chrome:s-x, gnome-terminal:C-z, xfce4-screenshooter -r:Alt+Shift, xfce4-screenshooter -w:Alt+Ctrl
# 6. スクリーンセーバー >> Atlantis with Only One モードを選択します
# 7. セッションと起動>> コマンドの最小化スタートアップを追加:devils_startup.sh
# 8. セッションと起動>> 一般タグの「ログアウト時にセッションを自己循環的に保存」のチェックを外します。
# 9. パネルのアクションボタン>> ログアウト選択パネルの「次回のログインのためにセッションを保存する」のチェックを外します。
