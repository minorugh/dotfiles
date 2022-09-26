### Dotfiles to restore Debian GNU/Linux
# author Minoru Yamada. 2021.10.11
# update 2022.09.22

## =====================================================================
## Manual setting before executing make
## =====================================================================
## 1. Boot from USB to netinstall Debian latest
# Download iso image file from https://www.debian.org/CD/netinst/index.ja.html
# Create a Install-USB stick by Rufs on Windows machine
# Download Rufs from https://rufs.ie/ja/

## 2. Register username to sudoers
# Log in as root
# | gpasswd -a minoru sudo
# | log out
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | log out

## 3. Set home sub directorys to English notation
# Log in with ${USER}
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git make nautilus
# | chsh -s /bin/zsh

## 4. Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and setting

## 5. Restore dotfiles
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone https://github.com/minorugh/dotfiles.git
# | cd dotfiles
# | make all

## =======================================================================
## Run make from here
## =======================================================================
PACKAGES	:= hugo nkf wget curl file unar unzip gcc golang npm keychain
PACKAGES	:= zsh-syntax-highlighting silversearcher-ag expect arc-theme
PACKAGES	+= pandoc rsync cmigemo e2ps evince net-tools ntp wmctrl hub
PACKAGES	+= ruby gnome-terminal nautilus-sendto xclip vim tmux unrar
PACKAGES	+= autokey-gtk autokey-common lhasa fzf tree aspell aspell-en
PACKAGES	+= screen mosh compizconfig-settings-manager compiz-plugins
PACKAGES	+= libsecret-tools xscreensaver xscreensaver-gl-extra nodejs
PACKAGES	+= menulibre pwgen xfce4-screenshooter bluetooth blueman gdebi
PACKAGES	+= pinta gimp darktable inkscape shotwell cups cups-bsd

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

APT			:= sudo apt install -y
.DEFAULT_GOAL := help

.PHONY: all allinstall nextinstall
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: allinstall nextinstall
allinstall: nextcloud gnupg ssh base install init keyring tlp emac-mozc mozc gistinstall images fontawesome
nextinstall: google-chrome spotify filezilla keepassxc sylpheed devilspie sxiv lepton zoom slack mattermost google-earth

.ONESHELL:
SHELL = /bin/bash

nextcloud: ## Init Nextcloud desktop
	$(APT) nextcloud-desktop
	test -L ${HOME}/.config/Nextcloud || rm -rf ${HOME}/.config/Nexicloud
	ln -vsfn ${HOME}/Dropbox/backup/Nextcloud ${HOME}/.config/Nexicloud

gnupg: ## Deploy gnupg (Run after rclone)
	$(APT) $@ git-crypt
	mkdir -p ${HOME}/.gnupg
	ln -vsf {${PWD},${HOME}}/.gnupg/gpg-agent.conf

ssh: ## Init ssh
	mkdir -p ${HOME}/.ssh
	for item in config known_hosts id_rsa github_id_rsa xsrv; do
		ln -vsfn ${HOME}/Dropbox/backup/ssh/$$item ${HOME}/.ssh/$$item
	done
	chmod 600 ${HOME}/.ssh/id_rsa ${HOME}/.ssh/github_id_rsa

init: ## Initial deploy dotfiles
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	for item in zprofile zshrc vimrc profile bashrc tmux.conf Xmodmap Xresources autologin.sh; do
		ln -vsf {${PWD},${HOME}}/.$$item
	done
	xmodmap ${HOME}/.Xmodmap
	chmod 600 ${HOME}/.autologin.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/autologin.desktop
	ln -vsf {${PWD},${HOME}}/.config/autostart/slack.desktop
	ln -vsf {${PWD},${HOME}}/.local/share/applications/org-protocol.desktop
	ln -vsfn {${PWD},${HOME}}/.fonts
	ln -vsfn {${PWD},${HOME}}/.vim
	for item in gitconfig gist; do
		ln -vsf ${HOME}/Dropbox/backup/.$$item ${HOME}/.$$item
	done
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
	sudo ln -vsf ${PWD}/etc/systemd/logind.conf /etc/systemd/logind.conf
	sudo ln -vsf ${PWD}/etc/default/grub /etc/default/grub
	sudo update-grub2

install: ## Install debian packages
	$(APT) $(PACKAGES)

base: ## Install base-devel packages
	$(APT) $(BASE_PKGS)

emacs-mozc:  ## Install emacs-mozc fcitx-mozc
	$(APT) $@ fcitx-mozc
# Set fcitx: Input im-config in terminal and ret → ret → check fcitx
# Set mozc: Open mozc settings from menu and import azik to romaji-table

## symbolic link of mozc dictionary
ifeq ($(shell uname -n),e590)
mozc: ## for mainmachine (Thinkpad E590)
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsfn ${HOME}/Dropbox/backup/mozc/.mozc ${HOME}/.mozc
else
mozc: ## for submachine (Thinkpad X250)
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
	ln -vsfn ${HOME}/Dropbox/backup/keyrings ${HOME}/.local/share/keyrings

gistinstall: ## Gist install | $ gist --login from terminal at first
	sudo gem install gist

images: ## Copy wallpaper to the user picture folder
	ln -vsf ${HOME}/Dropbox/images/wallpaper ${HOME}/Pictures
	ln -vsf ${HOME}/Dropbox/images/icons ${HOME}/Pictures

fontawesome: ##  Init Font Awesome
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts

## install for applications
google-chrome: ## Install Google-chrome-stable
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
	$(APT) ./google-chrome-stable_current_amd64.deb
	rm -f ./google-chrome-stable_current_amd64.deb

spotify: ## Install Spotify for Debian
	curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | sudo apt-key add -
	echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
	sudo apt update
	$(APT) spotify-client

filezilla:  ## Install filezilla and set "Filezilla -s" to start selected my:servers
	$(APT) $@
	test -L ${HOME}/.config/filezilla || rm -rf ${HOME}/.config/filezilla
	ln -vsfn {${PWD},${HOME}}/.config/filezilla

keepassxc: ## Install keeypassXC and auto start with master passwd.
	$(APT) $@ libsecret-tools
	test -L ${HOME}/.config/keepassxc || rm -rf ${HOME}/.config/keepassxc
	ln -vsfn {${PWD},${HOME}}/.config/keepassxc
# select-tool setup at first
# | sudo secret-tool store --label "KeePassXC master password" type kbd
# asked for a password so enter
# popup panel for passward input so input '<user passwd>' && use shell
# use term| secret-tool lookup type kdb | keepassxc --pw-stdin /path/to/keepassxc.kdb

sylpheed: ## Init sylpheed
	$(APT) $@ bogofilter kakasi
	test -L ${HOME}/.sylpheed-2.0 || rm -rf ${HOME}/.sylpheed-2.0
	ln -vsfn ${HOME}/Dropbox/sylpheed/.sylpheed-2.0 ${HOME}/.sylpheed-2.0
## Gmail security requires you to use the app password

devilspie: ## Init devilspie for minimize_startup applications
	mkdir -p ${HOME}/.devilspie
	$(APT) $@
	sudo ln -vsfn ${PWD}/devils/devils_startup.ds  ${HOME}/.devilspie
	sudo ln -vsfn ${PWD}/devils/devils_startup.sh  /usr/local/bin
	sudo chmod +x /usr/local/bin/devils_startup.sh

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

zoom: ## Install zoom
	cd ${HOME}/Downloads && \
	wget https://zoom.us/client/latest/zoom_amd64.deb
	sudo gdebi zoom_amd64.deb
	ln -vsfn {${PWD},${HOME}}/.config/zoomus.conf
	rm -f ./zoom_amd64.deb

slack: ## Install slack
	cd ${HOME}/Downloads && \
	wget https://downloads.slack-edge.com/linux_releases/slack-desktop-4.0.2-amd64.deb
	$(APT) ./slack-desktop-4.0.2-amd64.deb
	rm -f ./slack-desktop-4.0.2-amd64.deb

mattermost: ## Install mattermost
	curl -o- https://deb.packages.mattermost.com/setup-repo.sh | sudo bash
	$(APT) mattermost-desktop

google-earth: ## Install google-earth
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/dl/earth/client/current/google-earth-pro-stable_current_amd64.deb
	$(APT) ./google-earth-pro-stable_current_amd64.deb
	rm -f ./google-earth-pro-stable_current_amd64.deb

## From here, Step by step while interacting with SHELL
texlive: ## Install texlive full
	cd ${HOME}/Downloads && \
	wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
	tar xvf install-tl-unx.tar.gz && \
	rm -f install-tl-unx.tar.gz && \
	cd install-tl* && \
	sudo ./install-tl -no-gui -repository http://mirror.ctan.org/systems/texlive/tlnet/
## Asked for Actions, so enter `I` to start the installation
	sudo /usr/local/texlive/2022/bin/x86_64-linux/tlmgr path add
	sudo tlmgr update --self --all

latex: ## Symbolic link for dvpd.sh && mysty
	sudo ln -vsfn ${PWD}/tex/dvpd.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/dvpd.sh
	sudo ln -vsfn ${PWD}/tex/platex/my-sty /usr/local/texlive/2022/texmf-dist/tex/platex
	sudo mktexlsr

perlbrew: ## Install perlbrew
	curl -L http://install.perlbrew.pl | bash
	perlbrew install 5.30.3
	perlbrew switch 5.30.3
	perlbrew install-cpanm && \
	cpanm Net::FTPSSL && \
	cpanm Net::SFTP::Foreign

emacs-latest: ## Install Emacs of latest stable version
	cd ${HOME}/src && \
	git clone -b emacs-27 git@github.com:emacs-mirror/emacs.git && \
	cd emacs && ./autogen.sh &&	./configure && \
	make
	sudo make install
	rm -rf ${HOME}/.emacs.d/elpa

docker: ## Install docker
	sudo apt -y install apt-transport-https ca-certificates curl gnupg lsb-release
	curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
	echo "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/debian \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
	sudo apt update
	sudo apt -y install docker-ce docker-ce-cli containerd.io
	sudo adduser $USER docker
## logout && check `docker version'

docker-compose: ## Install docker-compose
	cd /usr/libexec/docker/cli-plugins/ && \
	sudo wget https://github.com/docker/compose/releases/download/v2.0.1/docker-compose-linux-x86_64
	sudo chmod +x  docker-compose-linux-x86_64
	sudo mv docker-compose-linux-x86_64 docker-compose
## chec `docker copose version`

github: ## Git clone
	mkdir -p ${HOME}/src/github.com/minorugh
	cd ${HOME}/src/github.com/minorugh && \
	git clone git@github.com:minorugh/GH.git && \
	git clone git@github.com:minorugh/.emacs.d.git && \
	git clone git@github.com:minorugh/minorugh.github.io.git && \
    git clone git@github.com:minorugh/emacs.d.Git && \
	git clone git@github.com:minorugh/upsftp.git && \
	git clone git@github.com:minorugh/iceberg-theme.git
# GH.git saves `.git' folder only and removes other data. These restored from Dropbox.

xfce4: ## Restore xfce4 session
	test -L ${HOME}/.config/xfce4 || rm -rf ${HOME}/.config/xfce4
	cp -rf ${HOME}/Dropbox/backup/xfce4/xfce4 ${HOME}/.config
# Restore xfce4 session>> `tar -zxvf ~/Dropbox/backup/xfce4/**.tar.gz' then `make xfce4'

## ==========================================================
## Manual settings after Debian install
## ==========================================================
## Some settings may be handled by xfce4-session restore
# 1. Replace key Caps with Ctrl>> `sudo nano /etc/default/keyboard' && edit to XKBOPTIONS="ctrl:nocaps" then reboot
# 2. Window Manager(in setting manager)>> style-> Arc-Dark, edit keyboard-> switch windows (Super+Alt), switch applications (Ctrl+Super), hide window (Alt+f9 to End key)
# 3. Exterior setting>> select style:Arc-Dark, font size:14
# 4. Print setting >> edit command: `sudo system-config-printer'
# 5. Keyboad setting>> emacs:s-e, sylpheed:s-s, chrome:s-c, gnome-terminal:C-z, xfce4-screenshooter -r:Alt+Shift, xfce4-screenshooter -w:Alt+Ctrl
# 6. Screen-saver>> select Atlantis with Only One mode
# 7. session & launch>> Add minimized startup, Set command:devils_startup.sh
