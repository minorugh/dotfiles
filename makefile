### Dotfiles to restore Debian GNU/Linux
# author Minoru Yamada. 2021.10.11

## =====================================================================
## Manual setting before executing make

## 1. Boot from Live-USB to install debian
## Download iso image file from https://www.debian.org/CD/live/
## Create a Live USB stick,
## https://www.archlinux.site/2018/03/linuxisoubuntulive-usb.html

## 2. Register username to sudoers
## Log in as root
# | gpasswd -a minoru sudo
# | log out
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | log out

## 3. Make HOME directory English
## Log in with ${USER}
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git nautilus
# | chsh -s /bin/zsh

## 4. Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and setting

## 5. Prepare dotfiles
# | mkdir -p ~/src/github.com/minorugh
# | cd src/github.com/minorugh
# | git clone https://github.com/minorugh/dotfiles.git
# | cd dotfiles
# | make install
# | make init

## Run make from here
## 1st stage for make allinstall
## =====================================================================

PACKAGES	:= silversearcher-ag hugo nkf wget curl file unar gcc golang npm
PACKAGES	+= pandoc make rsync cmigemo git e2ps evince net-tools ntp wmctrl hub expect
PACKAGES	+= ruby gnome-terminal nautilus nautilus-sendto xclip vim tmux unrar zsh
PACKAGES	+= autokey-gtk autokey-common lhasa zsh fzf tree aspell aspell-en arc-theme
PACKAGES	+= screen keychain mosh compizconfig-settings-manager compiz-plugins
PACKAGES	+= libsecret-tools xscreensaver xscreensaver-gl-extra nodejs gimp darktable
PACKAGES	+= menulibre pwgen xfce4-screenshooter bluetooth blueman gdebi shotwell
PACKAGES	+= cups cups-bsd

BASE_PKGS	:= openssl libssl-dev zlib1g-dev build-essential texinfo
BASE_PKGS	+= libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev
BASE_PKGS	+= libgtk2.0-dev libncurses-dev libgtk-3-dev libgnutls28-dev autoconf
BASE_PKGS	+= automake libtool xorg-dev libncurses5-dev python3-pip libdbus-1-dev
BASE_PKGS	+= libm17n-dev librsvg2-dev libotf-dev libxml2-dev libmagickwand-dev
BASE_PKGS	+= libc6-dev libtiff5-dev flatpak zlib1g-dev libnet-sftp-foreign-perl
BASE_PKGS	+= libice-dev libsm-dev libxext-dev libxmuu-dev
BASE_PKGS	+= libxrandr-dev libxt-dev libxtst-dev libxv-dev patch libglib2.0-0
BASE_PKGS	+= libxcb-shape0 libxcb-shm0 libxcb-xfixes0 libxcb-randr0 libxcb-image0
BASE_PKGS	+= libfontconfig1 libgl1-mesa-glx libxi6 libsm6 libxrender1 libpulse0

APT			:= sudo apt install -y

.DEFAULT_GOAL := help
.PHONY: all allinstall nextinstall

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: allinstall nextinstall

.ONESHELL:
SHELL = /bin/bash

rclone: ## Init rclone
	$(APT) $@
	chmod 600 ${HOME}/Dropbox/backup/rclone/rclone.conf
	test -L ${HOME}/.config/rclone || rm -rf ${HOME}/.config/rclone
	ln -vsfn ${HOME}/Dropbox/backup/rclone ${HOME}/.config/rclone

gnupg: ## Deploy gnupg (Run after rclone)
	$(APT) $@ git-crypt
	mkdir -p ${HOME}/.gnupg
	ln -vsf {${PWD},${HOME}}/.gnupg/gpg-agent.conf

ssh: ## Init ssh
	mkdir -p ${HOME}/.ssh
	for item in config known_hosts id_rsa github_id_rsa xsrv; do
		ln -vsfn ${HOME}/Dropbox/backup/ssh/$$item ${HOME}/.ssh/$$item
	done
	chmod 600 ${HOME}/.ssh/id_rsa
	chmod 600 ${HOME}/.ssh/github_id_rsa

init: ## Initial deploy dotfiles
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	for item in zprofile zshrc vimrc profile bashrc tmux.conf Xmodmap Xresources autologin.sh; do
		ln -vsf {${PWD},${HOME}}/.$$item
	done
	chmod 600 ${HOME}/.autologin.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/autologin.desktop
	ln -vsf {${PWD},${HOME}}/.config/autostart/slack.desktop
	ln -vsf {${PWD},${HOME}}/.local/share/applications/org-protocol.desktop
	ln -vsfn {${PWD},${HOME}}/.fonts
	ln -vsfn {${PWD},${HOME}}/.vim
	for item in gitconfig gist netrc; do
		ln -vsf ${HOME}/Dropbox/backup/.$$item ${HOME}/.$$item
	done
	ln -vsf ${HOME}/Dropbox/backup/hub ${HOME}/.config/hub
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
	sudo ln -vsf ${PWD}/etc/systemd/logind.conf /etc/systemd/logind.conf
	sudo ln -vsf ${PWD}/etc/default/grub /etc/default/grub
	sudo update-grub2

base: ## Install base-devel packages
	$(APT) $(BASE_PKGS)

install: ## Install debian packages
	$(APT) $(PACKAGES)

emacs-mozc:  ## Install fcitx-mozc
	$(APT) $@ fcitx-mozc emacs-mozc
# Setup fcitx: Input im-config in terminal and ret → ret → check fcitx
# Setup mozc: Open mozc settings from menu and import azik to romaji-table

## symbolic link of mozc dictionary
ifeq ($(shell uname -n),e590)
mozc: ## for e509
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsfn ${HOME}/Dropbox/mozc/.mozc ${HOME}/.mozc
else
mozc: ## for submachine
	cp -rf ~/Dropbox/mozc/.mozc ~/Dropbox/backup/mozc
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsfn ${HOME}/Dropbox/backup/mozc/.mozc ${HOME}/.mozc
endif

tlp: ## Setting for power saving and preventing battery deterioration
	$(APT) $@ tlp-rdw powertop
	sudo ln -vsf ${PWD}/etc/default/tlp /etc/default/tlp
	sudo tlp start

keyring: ## Init gnome keyrings
	$(APT) seahorse
	mkdir -p ${HOME}/.local/share
	test -L ${HOME}/.local/share/keyrings || rm -rf ${HOME}/.local/share/keyrings
	ln -vsfn ${HOME}/Dropbox/backup/keyrings ${HOME}/.local/share/keyrings

gistinstall: ## Gist install | $ gist --login from terminal at first
	sudo gem install gist

images: ## Copy wallpaper to the user picture folder
	ln -vsf ${HOME}/Dropbox/images/wallpaper ${HOME}/Pictures
	ln -vsf ${HOME}/Dropbox/images/icons ${HOME}/Pictures

fontawesome: ##  Ini Font Awesome
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts


## next install for applications
## =====================================================================
chrome: ## Install Google-chrome-stable
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
	$(APT) ./google-chrome-stable_current_amd64.deb
	rm  ./google-chrome-stable_current_amd64.deb

spotify: ## Install Spotify on Debian11
	curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | sudo apt-key add -
	echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
	sudo apt update
	$(APT) spotify-client

filezilla:  ## Install filezilla and set "Filezilla -s" to start selected myserver
	$(APT) $@
	test -L ${HOME}/.config/filezilla || rm -rf ${HOME}/.config/filezilla
	ln -vsfn ${HOME}/Dropbox/backup/filezilla ${HOME}/.config/filezilla

keepassxc: ## Install keeypassXC and auto start with master passwd.
	$(APT) $@ libsecret-tools
	test -L ${HOME}/.config/keepassxc || rm -rf ${HOME}/.config/keepassxc
	ln -vsfn {${PWD},${HOME}}/.config/keepassxc
## select-tool setup for password autoinmut
## | $ sudo secret-tool store --label "KeePassXC master password" type kbd
## asked for a password so enter
## popup panel for passward input so input '<user passwd>'
## Set Start command: 'secret-tool lookup type kdb | keepassxc --pw-stdin /path/to/keepassxc.kdb'

sylpheed: ## Init sylpheed
	$(APT) $@ bogofilter kakasi
	test -L ${HOME}/.sylpheed-2.0 || rm -rf ${HOME}/.sylpheed-2.0
	ln -vsfn ${HOME}/Dropbox/sylpheed/.sylpheed-2.0 ${HOME}/.sylpheed-2.0
## Gmail security requires you to use the app password
## Choose bogofilter for spam filter

devilspie: ## Init devilspie for minimize_startup applications
	mkdir -p ${HOME}/.devilspie
	sudo apt install devilspie
	sudo ln -vsfn ${PWD}/devils/devils_startup.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/devils_startup.sh  /usr/local/bin
	sudo chmod +x /usr/local/bin/devils_startup.sh
## minimized startup  → https://snap.minorugh.com/post/2022/0126-minimize-startup-sylpheed/

sxiv: ## Init sxiv
	$(APT) $@
	mkdir -p ${HOME}/.config/sxiv/exec
	ln -vsfn {${PWD},${HOME}}/.config/sxiv/exec/image-info
	chmod +x ${HOME}/.config/sxiv/exec/image-info

lepton: ## Init lepton
	mkdir -p ${HOME}/Appimage
	cd ${HOME}/Appimage && \
	wget https://github.com/hackjutsu/Lepton/releases/download/v1.10.0/Lepton-1.10.0.AppImage
	chmod a+x Lepton-1.10.0.AppImage
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/lepton.desktop

zoom: ## install zoom
	cd ${HOME}/Downloads && \
	wget https://zoom.us/client/latest/zoom_amd64.deb
	sudo gdebi zoom_amd64.deb
	ln -vsfn {${PWD},${HOME}}/.config/zoomus.conf

Slack: ## Install slack
	cd ${HOME}/Downloads && \
	wget https://downloads.slack-edge.com/linux_releases/slack-desktop-4.0.2-amd64.deb
	$(APT) ./slack-desktop-*.deb
	rm ./slack-desktop-*.deb

mattermost: ## Install mattermost
	curl -o- https://deb.packages.mattermost.com/setup-repo.sh | sudo bash
	$(APT) mattermost-desktop

google-earth: ## Install google-earth
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/dl/earth/client/current/google-earth-pro-stable_current_amd64.deb
	sudo apt install ./google-earth-pro-stable_current_amd64.deb
	rm ./google-earth-pro-stable_current_amd64.deb


# From here, Step by step while interacting with SHELL
## =====================================================================
texlive: ## Install texlive full
	cd ${HOME}/Downloads && \
	wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
	tar xvf install-tl-unx.tar.gz && \
	cd install-tl* && \
	sudo ./install-tl -no-gui -repository http://mirror.ctan.org/systems/texlive/tlnet/
## Asked for Actions, so enter `I` to start the installation
	sudo /usr/local/texlive/2022/bin/*/tlmgr path add
	sudo tlmgr update --self --all

latex: ## Symbolic for dvpd.sh && mysty
	sudo ln -vsfn ${PWD}/tex/dvpd.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/dvpd.sh
	sudo ln -vsfn ${PWD}/tex/platex/my-sty /usr/local/texlive/????/texmf-dist/tex/platex
	sudo mktexlsr

perlbrew: ## Install perlbrew
	curl -L http://install.perlbrew.pl | bash
	perlbrew install 5.30.3
	perlbrew switch 5.30.3
	perlbrew install-cpanm
	cpanm Net::FTPSSL
	cpanm Net::SFTP::Foreign

emacs-devel: ## Install development version of emacs
	cd ${HOME}/src && \
	git clone -b emacs-27 git@github.com:emacs-mirror/emacs.git && \
	cd emacs && ./autogen.sh &&	./configure && \
	make
	sudo make install
	rm -rf ${HOME}/.emacs.d/elpa

allinstall: rclone gnupg ssh base install init keyring tlp emac-mozc mozc gistinstall images fontawesome

nextinstall: chrome spotify filezilla keepassxc sylpheed sxiv lepton zoom slack mattermost

## Some settings
#########################################
## 設定マネージャー→ウインドウマネージャー
# スタイル: select to Arc-Dark
# キーボード: ウインドウ切り替え(Super+Alt) アプリケーション切り替え(Ctrl+Super)
##########################################
## 外観
# スタイル: select Arc-Dark
##########################################
## 印刷設定 in Whisker menu
# edit command: add sudo →sudo system-config-printer
##########################################
# update 2022.09.22
