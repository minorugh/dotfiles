### Dotfiles to restore Debian GNU/Linux
# author Minoru Yamada. 2021.10.11
# update 2022.09.22

## =====================================================================
## Manual setting before executing make
## =====================================================================
## 1. Boot from USB to install Debian latest
# Create installation USB from netinst iso image. Use Rufs.exe on Windows
# Download firmware from https://bre.is/f2LBmD3t for using WiFi
# Unzip firmware.zip, then paste to firmware directory of install USB

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
# | sudo apt install -y xdg-user-dirs-gtk ## Not needed on debian12
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git make nautilus

## 4. Install dropbox & setting
# Before installing, configure the Synapyic repository & Check existence of package
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and initial settings

## 5. Clone dotfiles from GitHub
# | mkdir -p ~/src/github.com/minorugh
# | cd ~/src/github.com/minorugh
# | git clone git@github.com:minorugh/dotfiles.git
# | cd dotfiles
# | make all
# | chsh -s /usr/bin/zsh

## =======================================================================
## Run make from here
## =======================================================================
PACKAGES	:= hugo nkf wget curl file unar unzip gcc golang npm keychain
PACKAGES	+= zsh-syntax-highlighting silversearcher-ag expect arc-theme
PACKAGES	+= pandoc rsync cmigemo e2ps evince net-tools ntp wmctrl hub
PACKAGES	+= ruby gnome-terminal xclip vim tmux freerdp2-x11
PACKAGES	+= autokey-gtk autokey-common lhasa fzf tree aspell aspell-en
PACKAGES	+= screen mosh compizconfig-settings-manager compiz-plugins
PACKAGES	+= libsecret-tools xscreensaver xscreensaver-gl-extra nodejs
PACKAGES	+= menulibre pwgen xfce4-screenshooter bluetooth blueman gdebi
PACKAGES	+= gimp darktable inkscape shotwell cups cups-bsd

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
BASE_PKGS	+= libgccjit0

APT			:= sudo apt install -y
.DEFAULT_GOAL := help

.PHONY: all allinstall nextinstall
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: allinstall nextinstall
allinstall: gnupg ssh install base init grub keyring tlp emacs-mozc mozc icons fontawesome gist
nextinstall: google-chrome filezilla sxiv lepton zoom pdrv

.ONESHELL:
SHELL = /bin/bash

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
	ln -vsf {${PWD},${HOME}}/.local/share/applications/keepass.desktop
	ln -vsf {${PWD},${HOME}}/.local/share/applications/lepton.desktop
	ln -vsfn {${PWD},${HOME}}/.fonts
	ln -vsfn {${PWD},${HOME}}/.vim
	for item in gitconfig gist; do
		ln -vsf ${HOME}/Dropbox/backup/.$$item ${HOME}/.$$item
	done

ifeq ($(shell uname -n),P1)
grub:
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
	sudo ln -vsf ${PWD}/etc/systemd/logind.conf /etc/systemd/logind.conf
	sudo ln -vsf ${PWD}/etc/default/grub /etc/default/grub
	sudo update-grub2
endif

install: ## Install debian packages
	$(APT) $(PACKAGES)

base: ## Install debian base packages
	$(APT) $(BASE_PKGS)

emacs-mozc:  ## Install emacs-mozc fcitx-mozc
	$(APT) $@ fcitx-mozc
# Set fcitx: Input im-config in terminal and ret → ret → check to fcitx

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

icons: ## Copy icons to picture folder
	ln -vsf ${HOME}/Dropbox/Documents/icons/* ${HOME}/Pictures

fontawesome: ##  Init Font Awesome
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts

gist: ## Install gist | $ gist --login from terminal at first
	sudo gem install gist

pdrv: ## Install Printer driver for Brother HL-L2375DW
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
	ln -vsfn ${HOME}/Dropbox/backup/filezilla ${HOME}/.config

thunderbird: ## Install Thunderbird and add external_editor_revived.json
	$(APT) $@
	sudo ln -vsfn ${PWD}/bin/external-editor-revived /usr/local/bin
	sudo chmod +x /usr/local/bin/external-editor-revived
	mkdir -p ${HOME}/.mozilla/native-messaging-hosts
	external-editor-revived | tee ${HOME}/.mozilla/native-messaging-hosts/external_editor_revived.json
# apply the add-on external-editor-revived at first
# see https://github.com/Frederick888/external-editor-revived/wiki

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
	sudo ln -vsfn ${PWD}/devils/devils_startup.sh  /usr/local/bin
	sudo chmod +x /usr/local/bin/devils_startup.sh

gitk: ## Init gitk for git-gui
	$(APT) $@
	sudo ln -vsfn {${PWD},${HOME}}/.config/git/gitk

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
	rm -f ./zoom_amd64.deb

google-earth: ## Install google-earth
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/dl/earth/client/current/google-earth-pro-stable_current_amd64.deb
	$(APT) ./google-earth-pro-stable_current_amd64.deb
	rm -f ./google-earth-pro-stable_current_amd64.deb

slack: ## Install slack
	cd ${HOME}/Downloads && \
	wget https://slack.com/downloads/instructions/linux?ddl=1&build=deb
	sudo gdebi slack-desktop-4.39.95-amd64.deb # Filename must be confirmed each time
	rm -f slack-desktop-4.39.95-amd64.deb

flatpak:
	$(APT) $@
	sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
	flatpak install flathub com.github.PintaProject.Pinta
	flatpak install flathub com.spotify.Client
	flatpak install flathub com.slack.Slack
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
	sudo ln -vsfn ${PWD}/tex/dvpd.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/dvpd.sh
	sudo ln -vsfn ${PWD}/tex/platex/my-sty /usr/local/texlive/2024/texmf-dist/tex/platex
	sudo mktexlsr

perlbrew: ## Install perlbrew
	curl -L http://install.perlbrew.pl | bash
	perlbrew install 5.30.3
	perlbrew switch 5.30.3
	perlbrew install-cpanm && \
	cpanm Net::FTPSSL && \
	cpanm Net::SFTP::Foreign

emacs-devel: ## Install emacs29.1
	sudo apt-get build-dep emacs-gtk
	cd ${HOME}/src
	wget http://ftp.gnu.org/gnu/emacs/emacs-29.1.tar.xz
	tar xvf emacs-29.1.tar.xz
	cd emacs-29.1 && ./autogen.sh && ./configure --with-native-compilation && sudo make && sudo make install  && make clean
	rm -rf ${HOME}/.emacs.d/elpa

github: ## Clone github repository
	mkdir -p ${HOME}/src/github.com/minorugh
	cd ${HOME}/src/github.com/minorugh && \
	git clone git@github.com:minorugh/GH.git && \
	git clone git@github.com:minorugh/upsftp.git && \
	git clone git@github.com:minorugh/iceberg-theme.git \
	git clone git@github.com:minorugh/emacs.d.git && \
	git clone git@github.com:minorugh/emacs-easy-hugo.git \
	git clone git@github.com:minorugh/minorugh.github.io.git
# GH.git saves `.git' folder only and removes other data. These restored from Dropbox.

## =====================================================================
## Custmize settings after Debian install
## =====================================================================
# 1. Replace key Caps with Ctrl>> `sudo nano /etc/default/keyboard' && edit to XKBOPTIONS="ctrl:nocaps" then reboot
# 2. Window Manager(in setting manager)>> style-> Arc-Dark, edit keyboard-> switch windows (Super+Alt), switch applications (Ctrl+Super), hide window (Alt+f9 to End key)
# 3. Exterior setting>> select style:Arc-Dark, font size:14
# 4. Print setting >> edit command: `sudo system-config-printer'
# 5. Keyboad setting>> emacs:s-e, chrome:s-x, gnome-terminal:C-z, xfce4-screenshooter -r:Alt+Shift, xfce4-screenshooter -w:Alt+Ctrl
# 6. Screen-saver>> select Atlantis with Only One mode
# 7. session & launch>> Add minimized startup for command:devils_startup.sh
