### Dotfiles to restore Debian GNU/Linux
# author Minoru Yamada. 2021.10.11
# update 2022.09.22 for Debian10
# update 2024.10.01 for Debian12

## =====================================================================
## Manual setting before executing make
## =====================================================================
## 1. Boot from USB to install Debian latest
# Create installation USB from netinst iso image. Use Rufs.exe on Windows
# rufs https://rufus.ie/ja/

## 2. Register username to sudoers
# Log in as root
# | gpasswd -a ${USER} sudo
# | exit
# Log in with ${USER}
# | logout
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | logout

## 3. Set home sub directorys to English notation
# Log in with ${USER}
# | sudo apt install -y xdg-user-dirs-gtk ## Not needed for debian12 or later
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh make unzip git gnupg git-crypt nautilus

## 4. Install dropbox & setting
# Before installing, configure the Synapyic repository & Check existence of package
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and initial settings

## 5. Import GPG private_key
# Wait for Dropbox sync to complete.
# | cd ~/Dropbox/backup
# | make gpg

## 6. Clone dotfiles from GitHub
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone git@github.com:minorugh/dotfiles.git
# | cd dotfiles
# | git-crypt unlock
# | make all
# | chsh -s /usr/bin/zsh

## =======================================================================
## Run make from here
## =======================================================================
PACKAGES	:= hugo nkf wget curl file unar unzip gcc npm keychain
PACKAGES	+= zsh-syntax-highlighting silversearcher-ag expect arc-theme
PACKAGES	+= pandoc rsync cmigemo e2ps evince net-tools ntp wmctrl hub
PACKAGES	+= ruby gnome-terminal xclip vim tmux freerdp2-x11 xdotool
PACKAGES	+= autokey-gtk autokey-common lhasa fzf tree aspell aspell-en
PACKAGES	+= screen mosh compizconfig-settings-manager compiz-plugins
PACKAGES	+= libsecret-tools xscreensaver xscreensaver-gl-extra nodejs
PACKAGES	+= menulibre pwgen xfce4-screenshooter bluetooth blueman gdebi
PACKAGES	+= gimp darktable inkscape shotwell cups cups-bsd vlc

BASE_PKGS	:= automake autoconf texinfo openssl patch build-essential
BASE_PKGS	+= libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev
BASE_PKGS	+= libgtk2.0-dev libncurses-dev libgtk-3-dev libgnutls28-dev
BASE_PKGS	+= libtool xorg-dev libncurses5-dev python3-pip libdbus-1-dev
BASE_PKGS	+= libm17n-dev librsvg2-dev libotf-dev libxml2-dev libmagickwand-dev
BASE_PKGS	+= libc6-dev libtiff5-dev flatpak zlib1g-dev libnet-sftp-foreign-perl
BASE_PKGS	+= libice-dev libsm-dev libxext-dev libxmuu-dev libssl-dev zlib1g-dev
BASE_PKGS	+= libxrandr-dev libxt-dev libxtst-dev libxv-dev libglib2.0-0
BASE_PKGS	+= libxcb-shape0 libxcb-shm0 libxcb-xfixes0 libxcb-randr0 libxcb-image0
BASE_PKGS	+= libfontconfig1 libgl1-mesa-glx libxi6 libsm6 libxrender1 libpulse0
BASE_PKGS	+= libgccjit0 build-dep emacs-gtk python3.11 python3.11-venv

APT		:= sudo apt install -y
.DEFAULT_GOAL	:= help

.PHONY: all allinstall nextinstall
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: allinstall nextinstall
allinstall: restor gnupg ssh install base init grub autologin keyring tlp emacs-mozc mozc icons gist fonts
nextinstall: google-chrome filezilla mutt sxiv lepton zoom printer

.ONESHELL:
SHELL = /bin/bash

gnupg: ## Deploy gnupg (Run after Private Key Import)
	$(APT) $@ git-crypt
	mkdir -p ${HOME}/.$@
	ln -vsf {${PWD},${HOME}}/.$@/gpg-agent.conf

ssh: ## Init ssh
	mkdir -p ${HOME}/.$@
	for item in config known_hosts id_rsa xsrv; do
		ln -vsfn {${PWD},${HOME}}/.ssh/$$item
	done
	chmod 600 ${HOME}/.ssh/id_rsa

init: ## Initial deploy dotfiles
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	for item in gitconfig zshrc vimrc tmux.conf Xmodmap Xresources; do
		ln -vsf {${PWD},${HOME}}/.$$item
	done
	xmodmap ${HOME}/.Xmodmap
	ln -vsf {${PWD},${HOME}}/.config/hub
	ln -vsfn {${PWD},${HOME}}/.fonts
	# ln -vsfn {${PWD},${HOME}}/.vim
	cp -rf {${PWD},${HOME}}/.vim

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
# 'export PATH=$PATH:$HOME/bin'

autologin: ## Run ssh-add with passphrase auto input at GUI startup
	ln -vsf {${PWD},${HOME}}/.autologin.sh
	chmod 600 ${HOME}/.autologin.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/autologin.desktop

install: ## Install debian packages
	$(APT) $(PACKAGES)

base: ## Install debian base packages
	$(APT) $(BASE_PKGS)

emacs-mozc:  ## Install emacs-mozc fcitx-mozc
	$(APT) $@ fcitx-mozc
# Set fcitx: Input im-config in terminal and ret → ret → check to fcitx

ifeq ($(shell uname -n),P1)
mozc: ## For mainmachine (Thinkpad P1)
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsfn ${HOME}/Dropbox/backup/mozc/.mozc ${HOME}/.mozc
else
mozc: ## For submachine (Thinkpad X250)
	cp -rf ~/Dropbox/backup/mozc/.mozc ~/
endif

tlp: ## Setting for power saving and preventing battery deterioration
	$(APT) $@ tlp-rdw powertop
	sudo ln -vsf ${PWD}/etc/default/tlp /etc/default/tlp
	sudo tlp start

keyring: ## Init gnome keyrings
	$(APT) seahorse
	mkdir -p ${HOME}/.local/share
	test -L ${HOME}/.local/share/keyrings || rm -rf ${HOME}/.local/share/keyrings
	ln -vsfn ${HOME}/{Dropbox/backup,.local/share}/keyrings

icons: ## Copy Collected icons & wallpaper to picture folder
	ln -vsf ${HOME}/Dropbox/Documents/icons/* ${HOME}/Pictures

fonts: ## Symlink for user fonts
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts

gist: ## Install gist for use gist-command from shell
	sudo gem install gist
	ln -vsf ${PWD}/.config/gist ${HOME}/.gist

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

sylpheed: ## Init sylpheed（Use App Password for authentication）
	$(APT) $@ bogofilter kakasi
	test -L ${HOME}/.sylpheed-2.0 || rm -rf ${HOME}/.sylpheed-2.0
	ln -vsfn ${HOME}/Dropbox/sylpheed/.sylpheed-2.0 ${HOME}/.sylpheed-2.0

mutt: neomutt w3m abook
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

devilspie: ## Init devilspie for minimize_startup applications
	mkdir -p ${HOME}/.devilspie
	$(APT) $@
	sudo ln -vsfn ${PWD}/devils/emacs.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/sylpheed.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/devils_startup.sh  /usr/local/bin
	sudo chmod +x /usr/local/bin/devils_startup.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/devils_startup.desktop

gitk: ## Init gitk for git-gui
	$(APT) $@
	sudo ln -vsfn {${PWD},${HOME}}/.config/git/gitk

sxiv: ## Init sxiv
	$(APT) $@
	mkdir -p ${HOME}/.config/sxiv/exec
	ln -vsfn {${PWD},${HOME}}/.config/sxiv/exec/image-info
	chmod +x ${HOME}/.config/sxiv/exec/image-info

webcatalog: ## Install Webcatalog for Appimage
	mkdir -p ${HOME}/Appimage
	cd ${HOME}/Appimage && \
	wget https://webcatalog.io/api/download/?platform=linux&arch=x64
	chmod a+x WebCatalog-60.0.0.AppImage

lepton: ## Install lepton
	mkdir -p ${HOME}/Appimage
	cd ${HOME}/Appimage && \
	wget https://github.com/hackjutsu/Lepton/releases/download/v1.10.0/Lepton-1.10.0.AppImage
	chmod a+x Lepton-1.10.0.AppImage
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/lepton.desktop

zoom: ## Install zoom
	cd ${HOME}/Downloads && \
	wget https://zoom.us/client/latest/zoom_amd64.deb
	sudo gdebi zoom_amd64.deb
	rm -f ./zoom_amd64.deb

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

########################################################
## From here, Step by step while interacting with SHELL
########################################################
texlive: ## Install texlive full
	cd ${HOME}/Downloads && \
	wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
	tar xvf install-tl-unx.tar.gz && \
	rm -f install-tl-unx.tar.gz && \
	cd install-tl* && \
	sudo ./install-tl -no-gui -repository http://mirror.ctan.org/systems/texlive/tlnet/
## Asked for Actions, so enter `I` to start the installation
	sudo /usr/local/texlive/2024/bin/x86_64-linux/tlmgr path add
	sudo tlmgr update --self --all

latex: ## Symbolic link for dvpd.sh && mysty
	sudo ln -vsfn ${HOME}/Dropbox/backup/tex/dvpd.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/dvpd.sh
	sudo ln -vsfn ${HOME}/Dropbox/backup/tex/platex/my-sty /usr/local/texlive/2024/texmf-dist/tex/platex
	sudo mktexlsr

perlbrew: ## Install perlbrew
	curl -L http://install.perlbrew.pl | bash
	perlbrew install 5.30.3
	perlbrew switch 5.30.3
	perlbrew install-cpanm && \

emacs-stable: ## Install stable version of emacs
	cd ~/src/
	wget http://mirrors.nav.ro/gnu/emacs/emacs-29.4.tar.xz
	tar xvf emacs-29.4.tar.xz
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
	git clone -b emacs-30 git@github.com:emacs-mirror/emacs.git ${HOME}/src/emacs
	cd ${HOME}/src/emacs && ./autogen.sh && ./configure --with-native-compilation=aot && make && sudo make install && make clean
	rm -rf ${HOME}/.emacs.d/elpa

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
