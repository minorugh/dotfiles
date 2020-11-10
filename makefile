export PATH := ${HOME}/.zinit/polaris/sbin:${HOME}/.zinit/polaris/bin:${HOME}/perl5/perlbrew/bin:${HOME}/perl5/perlbrew/perls/perl-5.30.3/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:${HOME}/bin

### Debian 10 Restore Settings
### Boot from install usb to install debian

## =====================================================================
## Manual setting procedure
## =====================================================================
## 1. Log in as root
## Register username to sudoers
# | gpasswd -a minoru sudo
# | exit
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]

## 2. Log in with ${USER}
## Make home directory English
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git nautilus chromium chromium-l10n

## Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | ~/.dropbox-dist/dropboxd

## =====================================================================
## Make install
## =====================================================================
## 1st stage for make allinstall
rclone: ## Init rclone
	sudo apt install -y rclone
	chmod 600 ${PWD}/.config/rclone/rclone.conf
	test -L ${HOME}/.config/rclone || rm -rf ${HOME}/.config/rclone
	ln -vsfn ${PWD}/.config/rclone ${HOME}/.config/rclone

gnupg: ## Deploy gnupg (Run after rclone)
	sudo apt install -y git-crypt gnupg
	mkdir -p ${HOME}/.gnupg
	ln -vsf ${PWD}/.gnupg/gpg-agent.conf ${HOME}/.gnupg/gpg-agent.conf

ssh: ## Init ssh
		mkdir -p ${HOME}/.ssh
	ln -vsf ${PWD}/.ssh/.gitattributes ${HOME}/.ssh/.gitattributes
	ln -vsf ${PWD}/.ssh/config ${HOME}/.ssh/config
	ln -vsf ${PWD}/.ssh/known_hosts ${HOME}/.ssh/known_hosts
	ln -vsf ${PWD}/.ssh/id_rsa ${HOME}/.ssh/id_rsa
	ln -vsf ${PWD}/.ssh/xsrv_rsa ${HOME}/.ssh/xsrv_rsa
	chmod 600 ${HOME}/.ssh/id_rsa
	chmod 600 ${HOME}/.ssh/xsrv_rsa

cica: ## Initial font cica ricty
	ln -vsfn ${PWD}/.fonts ${HOME}/.fonts

init: ## Initial deploy dotfiles
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	ln -vsf ${PWD}/.gitattributes ${HOME}/.gitattributes
	ln -vsf ${PWD}/.zshrc ${HOME}/.zshrc
	ln -vsf ${PWD}/.vimrc ${HOME}/.vimrc
	ln -vsf ${PWD}/.bashrc ${HOME}/.bashrc
	ln -vsf ${PWD}/.tmuc.conf ${HOME}/.tmux.conf
	ln -vsf ${PWD}/.Xmodmap ${HOME}/.Xmodmap
	ln -vsf ${PWD}/.Xmodmap_default ${HOME}/.Xmodmap_default
	ln -vsf ${PWD}/.Xresources ${HOME}/.Xresources
	ln -vsf ${PWD}/.gitconfig ${HOME}/.gitconfig
	ln -vsf ${PWD}/.netrc ${HOME}/.netrc
	ln -vsf ${PWD}/.config/.gitattributes ${HOME}/.config/.gitattributes
	ln -vsf ${PWD}/.config/hub ${HOME}/.config/hub

base: ## Install base and base-devel package
	sudo apt install -y openssl libssl-dev zlib1g-dev build-essential texinfo \
	libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev \
	libgtk2.0-dev libncurses-dev gnutls-dev libgtk-3-dev libgnutls28-dev \
	autoconf automake libtool xorg-dev libncurses5-dev python3-pip \
	libdbus-1-dev libm17n-dev librsvg2-dev libotf-dev libxml2-dev \
	libmagickwand-dev libc6-dev libtiff5-dev xaw3dg-dev \
	zlib1g-dev libice-dev libsm-dev libxext-dev libxi-dev libxmu-dev \
	libxmuu-dev libxrandr-dev libxt-dev libxtst-dev libxv-dev patch

install: ## Install debian linux packages using apt
	sudo apt install -y silversearcher-ag hugo nkf wget curl gcc golang \
	pandoc make rsync cmigemo git e2ps evince net-tools ntp wmctrl hub \
	ruby gedit gnome-terminal nautilus xclip vim tmux unrar zsh python3-pip \
	inkscape darktable lhasa ruby zsh fzf tree aspell aspell-en screen \
	keychain mosh compizconfig-settings-manager compiz-plugins

emacsmozc: ## Install emacs mozc
	sudo apt install -y fcitx-mozc emacs-mozc
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsfn ${HOME}/Dropbox/mozc/.mozc ${HOME}/.mozc

cups: ## Install cups & lpr
	sudo apt install -y cups lpr


### 2nd stage for make step by step
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

filezilla:  ## Install filezilla and auto start selected myserver.
	sudo apt install -y filezilla
## edit start command: 'filezilla -s'

keepassxc: ## Install keeypassXC and auto start with master passwd.
	sudo apt install -y keepassxc
	# sudo apt install -y secret-tool
	# secret-tool store --label "KeePassXC master password" type kbd ## asked for a password, so enter 'gospel'
## Set Start command: 'secret-tool lookup type kdb | keepassxc --pw-stdin /path/to/keepassxc.kdb'

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

allinstall: rclone ssh cica emacsmozc init base install cups

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help

# update 2020.10.29
