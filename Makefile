### Debian 10 Restore Settings

## =====================================================================
## Manual setting procedure
## =====================================================================

## 1. Boot from install-usb to install debian

## 2. Log in as root
## Register username to sudoers
# | gpasswd -a minoru sudo
# | log out
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | log out

## 3. Log in with ${USER}
## Make home directory English
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git nautilus chromium chromium-l10n
# | chsh -s /bin/zsh

## 4. Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and setting

## =====================================================================
## Make install
## =====================================================================
## 1st stage for make allinstall

allinstall: gnupg ssh base install init keyring tlp cica emacsmozc cups pipinstall snapinstall

gnupg: ## Deploy gnupg (Run after rclone)
	sudo apt install -y git-crypt gnupg
	mkdir -p ${HOME}/.gnupg
	ln -vsf ${PWD}/.gnupg/gpg-agent.conf ${HOME}/.gnupg/gpg-agent.conf

ssh: ssh-common thinkpad

ssh-common: ## ssh commom files
	mkdir -p ${HOME}/.ssh
	ln -vsf ${PWD}/.ssh/known_hosts ${HOME}/.ssh/known_hosts
	ln -vsf ${PWD}/.ssh/id_rsa ${HOME}/.ssh/id_rsa
	ln -vsf ${PWD}/.ssh/id_rsa.pub ${HOME}/.ssh/id_rsa.pub
	ln -vsf ${PWD}/.ssh/github_id_rsa ${HOME}/.ssh/github_id_rsa
	ln -vsf ${PWD}/.ssh/github_id_rsa.pub ${HOME}/.ssh/github_id_rsa.pub
	ln -vsf ${PWD}/.ssh/xsrv_rsa ${HOME}/.ssh/xsrv_rsa
	chmod 600 ${HOME}/.ssh/id_rsa
	chmod 600 ${HOME}/.ssh/github_id_rsa
	chmod 600 ${HOME}/.ssh/xsrv_rsa

## Branch setting by machine model
ifeq ($(shell uname -n),x250)
thinkpad: ## for sub machine (Thinkpad X250)
	ln -vsf ${PWD}/.ssh/config.x250 ${HOME}/.ssh/config
	test -L ${HOME}/.config/xfce4 || rm -rf ${HOME}/.config/xfce4
	ln -vsfn ${HOME}/Dropbox/backup/conf/xfce4.x250 ${HOME}/.config/xfce4
else
thinkpad: ## for main machine (Thinkpad E590)
	ln -vsf ${PWD}/.ssh/config.e590 ${HOME}/.ssh/config
	test -L ${HOME}/.config/xfce4 || rm -rf ${HOME}/.config/xfce4
	ln -vsfn ${HOME}/Dropbox/backup/conf/xfce4.e590 ${HOME}/.config/xfce4
endif

cica: ## Initial font cica ricty
	ln -vsfn ${PWD}/.fonts ${HOME}/.fonts

init: ## Initial deploy dotfiles
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	ln -vsf ${PWD}/.zshrc ${HOME}/.zshrc
	ln -vsf ${PWD}/.vimrc ${HOME}/.vimrc
	ln -vsf ${PWD}/.bashrc ${HOME}/.bashrc
	ln -vsf ${PWD}/.tmuc.conf ${HOME}/.tmux.conf
	ln -vsf ${PWD}/.Xmodmap ${HOME}/.Xmodmap
	ln -vsf ${PWD}/.Xresources ${HOME}/.Xresources
	ln -vsf ${PWD}/.gitconfig ${HOME}/.gitconfig
	ln -vsf ${PWD}/.netrc ${HOME}/.netrc
	ln -vsf ${PWD}/.config/hub ${HOME}/.config/hub
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf  ## auto-login

base: ## Install base and base-devel package
	sudo apt install -y openssl libssl-dev zlib1g-dev build-essential texinfo \
	libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev \
	libgtk2.0-dev libncurses-dev libgtk-3-dev libgnutls28-dev autoconf \
	automake libtool xorg-dev libncurses5-dev python3-pip libdbus-1-dev \
	libm17n-dev librsvg2-dev libotf-dev libxml2-dev libmagickwand-dev libc6-dev \
	libtiff5-dev xaw3dg-dev python-xlib zlib1g-dev libice-dev libsm-dev \
	libxext-dev libxi-dev libxmu-dev libxmuu-dev libxrandr-dev libxt-dev \
	libxtst-dev libxv-dev patch libglib2.0-0 libxcb-shape0 libxcb-shm0 \
	libxcb-xfixes0 libxcb-randr0 libxcb-image0 libfontconfig1 libgl1-mesa-glx \
	libxi6 libsm6 libxrender1 libpulse0 libxcomposite1 libxslt1.1 libsqlite3-0 \
	libxcb-keysyms1 libxcb-xtest0

install: ## Install debian linux packages using apt
	sudo apt install -y silversearcher-ag hugo nkf wget curl gcc golang \
	pandoc make rsync cmigemo git e2ps evince net-tools ntp wmctrl hub gwenview \
	ruby gedit gnome-terminal nautilus xclip vim tmux unrar zsh autokey-gtk \
	autokey-common inkscape darktable lhasa ruby zsh fzf tree aspell aspell-en \
	screen keychain mosh compizconfig-settings-manager compiz-plugins \
	libsecret-tools pinta xscreensaver xscreensaver-gl-extra nodejs npm \
	menulibre pwgen

emacsmozc: ## Install emacs mozc
	sudo apt install -y fcitx-mozc emacs-mozc
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsfn ${HOME}/Dropbox/mozc/.mozc ${HOME}/.mozc

tlp: ## Setting for power saving and preventing battery deterioration
	sudo apt install -y tlp tlp-rdw powertop
	sudo ln -vsf ${PWD}/etc/default/tlp /etc/default/tlp
	sudo tlp start

keyring: ## Init gnome keyrings
	sudo apt install -y seahorse
	mkdir -p ${HOME}/.local/share
	test -L ${HOME}/.local/share/keyrings || rm -rf ${HOME}/.local/share/keyrings
	ln -vsfn ${HOME}/Dropbox/backup/conf/keyrings ${HOME}/.local/share/keyrings

cups: ## Install cups & lpr
	sudo apt install -y cups lpr

pipinstall: ## Install python packages
	sudo apt install -y python3-pip python3-sphinx
	pip3 install recommonmark

snapinstall: ## Install snap packages
	sudo apt install -y snapd
	sudo snap install core
	sudo snap install lepton spotify
	sudo ln -vsf /var/lib/snapd/desktop/applications/Spotify ${HOME}/.local/share/applications/Spotify

## =====================================================================
## next stage for make step by step

texlive: ## Install tevlive full
	wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
	tar xvf install-tl-unx.tar.gz
	cd install-tl*
	sudo ./install-tl -no-gui -repository http://mirror.ctan.org/systems/texlive/tlnet/
## Asked for Actions, so enter I to start the installation
	sudo /usr/local/texlive/????/bin/*/tlmgr path add
	sudo tlmgr update --self --all

mysty: ## Symbolic for my-sty
	sudo ln -vsfn ${PWD}/tex/platex/my-sty /usr/local/texlive/????/texmf-dist/tex/platex
	sudo mktexlsr

dvpd: ## Symbolic for dvpd.sh
	sudo ln -vsf ${PWD}/tex/dvpd.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/dvpd.sh

perlbrew: ## Install perlbrew
	curl -L http://install.perlbrew.pl | bash
	perlbrew install 5.30.3
	perlbrew switch 5.30.3
	perlbrew install-cpanm
	cpanm Net::FTPSSL

albert: ## Install albert
	cd ${HOME}/Downloads;\
	echo 'deb http://download.opensuse.org/repositories/home:/manuelschneid3r/Debian_10/ /' | sudo tee /etc/apt/sources.list.d/home:manuelschneid3r.list
	curl -fsSL https://download.opensuse.org/repositories/home:manuelschneid3r/Debian_10/Release.key | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/home_manuelschneid3r.gpg > /dev/null
	sudo apt update
	sudo apt install albert
	cd ${HOME}/src/github.com/minorugh/dotfiles;\
	ln -vsf ${PWD}/.config/albert/albert.conf ${HOME}/.config/albert/albert.conf

zoom: ## Download the DEB file for Debian from https://zoom.us/download?os=linux
	cd ${HOME}/Downloads;\
	wget https://zoom.us/client/latest/zoom_amd64.deb
	sudo apt install ./zoom_amd64.deb ## in Downloas folder
	cd ${HOME}/src/github.com/minorugh/dotfiles;\
	ln -vsf ${PWD}/.config/zoomus.conf ${HOME}/.config/zoomus.conf

filezilla:  ## Install filezilla and auto start selected myserver.
	sudo apt install -y filezilla
	test -L ${HOME}/.config/filezilla || rm -rf ${HOME}/.config/filezilla
	ln -vsfn ${HOME}/Dropbox/backup/conf/filezilla ${HOME}/.config/filezilla
## edit start command: 'filezilla -s'

keepassxc: ## Install keeypassXC and auto start with master passwd.
	sudo apt install -y keepassxc
	sudo apt install -y libsecret-tools
	test -L ${HOME}/.config/keepassxc || rm -rf ${HOME}/.config/keepassxc
	ln -vsfn ${HOME}/Dropbox/backup/conf/keepassxc ${HOME}/.config/keepassxc
## select-tool setup
## | $ sudo secret-tool store --label "KeePassXC master password" type kbd
## asked for a password so enter
## popup panel for passward input so input 'gospel'
## Set Start command: 'secret-tool lookup type kdb | keepassxc --pw-stdin /path/to/keepassxc.kdb'

sylpheed: ## Init sylpheed
	sudo apt install -y sylpheed
	test -L ${HOME}/.sylpheed-2.0 || rm -rf ${HOME}/.sylpheed-2.0
	ln -vsfn ${HOME}/Dropbox/sylpheed/.sylpheed-2.0 ${HOME}/.sylpheed-2.0

### Copy favorite wallpaper to the user picture folder
wallpaper:
	ln -vsf ${HOME}/Dropbox/backup/wallpaper ${HOME}/Pictures

emacs-devel: ## Install development version of emacs
	cd ${HOME}/src;\
	git clone -b emacs-27 git@github.com:emacs-mirror/emacs.git;\
	cd emacs;\
	./autogen.sh;\
	./configure;\
	make;\
	sudo make install;\
	rm -rf ${HOME}/.emacs.d/elpa

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help

# update 2021.1.31
