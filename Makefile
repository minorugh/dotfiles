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

gnupg: ## Deploy gnupg (Run after rclone)
	sudo apt install -y git-crypt gnupg
	mkdir -p ${HOME}/.gnupg
	ln -vsf ${PWD}/.gnupg/gpg-agent.conf ${HOME}/.gnupg/gpg-agent.conf

ssh: ssh-common thinkpad

ssh-common: ## ssh commom files
	mkdir -p ${HOME}/.ssh
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/known_hosts ${HOME}/.ssh/known_hosts
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/id_rsa ${HOME}/.ssh/id_rsa
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/id_rsa.pub ${HOME}/.ssh/id_rsa.pub
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/github_id_rsa ${HOME}/.ssh/github_id_rsa
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/github_id_rsa.pub ${HOME}/.ssh/github_id_rsa.pub
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/xsrv_rsa ${HOME}/.ssh/xsrv_rsa
	chmod 600 ${HOME}/.ssh/id_rsa
	chmod 600 ${HOME}/.ssh/github_id_rsa
	chmod 600 ${HOME}/.ssh/xsrv_rsa

## Branch setting by machine model
ifeq ($(shell uname -n),x250)
thinkpad: ## for sub machine (Thinkpad X250)
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/config.x250 ${HOME}/.ssh/config
	test -L ${HOME}/.config/xfce4 || rm -rf ${HOME}/.config/xfce4
	ln -vsfn ${HOME}/Dropbox/backup/conf/xfce4.x250 ${HOME}/.config/xfce4
else
thinkpad: ## for main machine (Thinkpad E590)
	ln -vsf ${HOME}/Dropbox/backup/conf/.ssh/config.e590 ${HOME}/.ssh/config
	test -L ${HOME}/.config/xfce4 || rm -rf ${HOME}/.config/xfce4
	ln -vsfn ${HOME}/Dropbox/backup/conf/xfce4.e590 ${HOME}/.config/xfce4
endif

cica: ## Initial font cica ricty
	ln -vsfn ${PWD}/.fonts ${HOME}/.fonts

init: ## Initial deploy dotfiles
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	ln -vsf ${PWD}/.zprofile ${HOME}/.zprofile
	ln -vsf ${PWD}/.zshrc ${HOME}/.zshrc
	ln -vsf ${PWD}/.vimrc ${HOME}/.vimrc
	ln -vsf ${PWD}/.bashrc ${HOME}/.bashrc
	ln -vsf ${PWD}/.tmux.conf ${HOME}/.tmux.conf
	ln -vsf ${PWD}/.Xmodmap ${HOME}/.Xmodmap
	ln -vsf ${PWD}/.Xresources ${HOME}/.Xresources
	ln -vsf ${HOME}/Dropbox/backup/conf/.gitconfig ${HOME}/.gitconfig
	ln -vsf ${HOME}/Dropbox/backup/conf/.netrc ${HOME}/.netrc
	ln -vsf ${PWD}/.config/hub ${HOME}/.config/hub
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf  ## auto-login
	sudo ln -vsf ${PWD}/etc/systemd/logind.conf /etc/systemd/logind.conf  ## not sleep if closed lid
	sudo ln -vsf ${PWD}/etc/default/grub /etc/default/grub  ## change grub console screen size
	sudo update-grub2

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
	sudo apt install -y silversearcher-ag hugo nkf wget curl unar gcc golang \
	pandoc make rsync cmigemo git e2ps evince net-tools ntp wmctrl hub expect \
	ruby gnome-terminal nautilus nautilus-sendto xclip vim tmux unrar zsh autokey-gtk \
	autokey-common lhasa ruby zsh fzf tree aspell aspell-en arc-theme \
	screen keychain mosh compizconfig-settings-manager compiz-plugins \
	libsecret-tools xscreensaver xscreensaver-gl-extra nodejs npm albert \
	menulibre pwgen xfce4-screenshooter bluetooth blueman gdebi shotwell

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

printer: ## Install cups printer driver
# Download installer https://www.brother.co.jp/download/software/index.aspx#printer
# Unzip gz file
	sudo bash linux-brjprinter-installer-2.2.2-1 HL-5250DN

pipinstall: ## Install python packages
	sudo apt install -y python3-pip python3-sphinx
	pip3 install recommonmark

gistinstall: ## Gist install | $ gist --login from terminal at first
	sudo apt-get install ruby
	sudo apt-get install gem
	sudo gem install gist
	gist --login

snapinstall: ## Install snap packages
	sudo apt install -y snapd
	sudo snap install pinta spotify
	sudo ln -vsf /var/lib/snapd/desktop/applications/pinta_pinta.desktop ${HOME}/.local/share/applications/pinta_pinta.desktop
	sudo ln -vsf /var/lib/snapd/desktop/applications/spotify_spotify.desktop ${HOME}/.local/share/applications/spotify_spotify.desktop

flatpakinstall: ## Install flatpak
	sudo apt install -y flatpak gnome-software-plugin-flatpak
	flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
	flatpak install flathub org.gimp.GIMP
	flatpak install flathub org.inkscape.Inkscape
	flatpak install flathub org.remmina.Remmina
	flatpak install flathub org.gnu.emacs
	flatpak install flathub us.zoom.Zoom
	ln -vsf ${PWD}/.config/zoomus.conf ${HOME}/.config/zoomus.conf

## =====================================================================
## next stage for make step by step

texlive: ## Install tevlive full
	cd ${HOME}/Downloads;\
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
	cd ${HOME}/Downloads;\
	curl -L http://install.perlbrew.pl | bash
	perlbrew install 5.30.3
	perlbrew switch 5.30.3
	perlbrew install-cpanm
	cpanm Net::FTPSSL

filezilla:  ## Install filezilla and set "Filezilla -s" to start selected myserver
	sudo apt install -y filezilla
	test -L ${HOME}/.config/filezilla || rm -rf ${HOME}/.config/filezilla
	ln -vsfn ${HOME}/Dropbox/backup/conf/filezilla ${HOME}/.config/filezilla

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

sxiv: ## Init sxiv
	sudo apt install -y sxiv
	mkdir -p ${HOME}/.config/sxiv/exec
	ln -vsf ${PWD}/.config/sxiv/exec/image-info ${HOME}/.config/sxiv/exec/image-info
	chmod +x ${HOME}/.config/sxiv/exec/image-info

darktable: ## Install darktable for Debian 10
	echo 'deb http://download.opensuse.org/repositories/graphics:/darktable/Debian_10/ /' | sudo tee /etc/apt/sources.list.d/graphics:darktable.list
	curl -fsSL https://download.opensuse.org/repositories/graphics:darktable/Debian_10/Release.key | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/graphics_darktable.gpg > /dev/null
	sudo apt update
	sudo apt install darktable

images: ## Copy favorite wallpaper to the user picture folder
	ln -vsf ${HOME}/Dropbox/backup/wallpaper ${HOME}/Pictures
	ln -vsf ${HOME}/Dropbox/backup/icons ${HOME}/Pictures

google-erath: ## install google erath
# Download deb file for debian from the url below
# https://www.google.co.jp/earth/download/gep/agree.html
	cd ${HOME}/Downloads;\
	sudo apt install ./google-erath-pro-stable_current_amd64.deb

edge: ## install microsoft edge
# Download deb file for debian from the url below
# https://www.microsoftedgeinsider.com/ja-jp/
	cd ${HOME}/Downloads;\
	sudo dpkg -i microsoft-edge-*.deb

emacs-devel: ## Install development version of emacs
	cd ${HOME}/src;\
	git clone -b emacs-27 git@github.com:emacs-mirror/emacs.git;\
	cd emacs;\
	./autogen.sh;\
	./configure;\
	make;\
	sudo make install;\
	rm -rf ${HOME}/.emacs.d/elpa


allinstall: gnupg ssh base install init keyring tlp cica emacsmozc cups pipinstall snapinstall

nextinstall: albert filezilla keepassxc sylpheed sxiv images



help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.DEFAULT_GOAL := help

# update 2021.2.2
