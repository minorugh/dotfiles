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
# update 2026.06.11 for Debian13
# update 2026.06.11 fixed emacs-stable for Emacs-30.2
# update 2026.06.14 xsrv-backup-data を xsrv-backup に統合（timer/service 一本化）
# update 2026.06.16 xsrv-backup-data を廃止、xsrv-backup に一本化（動的ファイルのみ対象）
# update 2026.06.16 xsrv-backup を cron に移行、xsrv-systemd ターゲット廃止
# update 2026.07.10 Thunderbird廃止（neomutt+Gmail Webへ移行）に伴い thunderbird ターゲット・バックアップ関連を削除
# update 2026.07.25 emacs-stable を ##! に変更（対話確認ありターゲットの目印、09-makefile.el側で判定）
# update 2026.07.30 bin/ スクリプト（シンボリックリンク＋xfconf-queryショートカット登録）を専用セクションに集約、tile-toggle 新規登録、keepass.sh→keepassxc.sh リネーム
# update 2026.07.30 サブ機の git pull後に env-sync（env_source/abook差分同期）を自動呼び出し ※テスト版
# update 2026.07.31 git ターゲットを ##! 化（Emacs経由でも env-sync の対話プロンプトが機能するように）
# update 2026.07.31 baseinstall に make-run・tig を追加、nextinstall に hugo を追加（漏れの解消）
# update 2026.07.31 neomutt-bin を廃止、neomutt ターゲットに一元化（重複処理の整理）
# update 2026.07.31 「対話実行系」セクションの5ターゲット（dracula-theme・keepassxc・google-earth・slack・flatpak）を ##! 化（区分と目印の不整合を解消）
# update 2026.07.31 git/env-sync/git-fix を git/Makefile に分離、トップレベルはラッパー化（リストア用と日常運用用の関心事を分離）
# update 2026.08.11 dropbox-resume-watch ターゲット追加（D-Bus PrepareForSleepフック、クリーンリストア対応）
#
# make 実行前の手動準備手順は README.md を参照してください
# https://github.com/minorugh/dotfiles

########################################################
## 変数定義
########################################################
# NOTE: 将来メイン機を変更した時はここを書き換えること。
MAIN_HOSTNAME  := P1
HOSTNAME       := $(shell hostname)

ENV_SOURCE_DIR := $(HOME)/.env_source
ENV_SYNC_CACHE := $(HOME)/.cache/env-sync

PACKAGES	:= hugo nkf wget curl file unar unzip gcc npm keychain smartmontools lm-sensors
PACKAGES	+= zsh-syntax-highlighting silversearcher-ag expect arc-theme
PACKAGES	+= pandoc rsync cmigemo e2ps evince net-tools wmctrl tig trash-cli
PACKAGES	+= ruby gnome-terminal xclip vim xdotool
PACKAGES	+= autokey-gtk autokey-common lhasa tree aspell aspell-en
PACKAGES	+= mosh nodejs sxiv
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

########################################################
## エントリーポイント
########################################################
.PHONY: all baseinstall nextinstall help

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all: baseinstall nextinstall
baseinstall: env-setup ssh install base init zsh-restore init-sub keymap grub autostart cron dropbox-watch night-suspend emacs-trash keyring fzf-tools tlp emacs-mozc icons gist fonts emacs-toggle tile-toggle make-run tig
nextinstall: google-chrome filezilla gitk neomutt sxiv lepton zoom printer hugo

SHELL = /bin/bash

########################################################
## 環境初期化（dotfiles 展開・SSH・zsh）
########################################################
env-setup: ## env/ を bindfs で ~/.env_source にマウント（新規ファイル自動反映）
	$(APT) bindfs
	sudo ln -vsf ${PWD}/etc/fuse.conf /etc/fuse.conf
	$(MAKE) -s -C git env-remount

ssh: ## SSH設定の初期化（~/.env_source から展開）
	mkdir -p ${HOME}/.$@
	for item in config known_hosts id_rsa xsrv; do \
		ln -vsf ${ENV_SOURCE_DIR}/.ssh/$$item ${HOME}/.ssh/$$item; \
	done
	chmod 600 ${HOME}/.ssh/id_rsa

init: ## dotfiles のシンボリックリンク展開
	test -L ${HOME}/.emacs.d || rm -rf ${HOME}/.emacs.d
	ln -vsfn ${PWD}/.emacs.d ${HOME}/.emacs.d
	for item in xprofile gitconfig bashrc zshrc vimrc Xresources ; do \
		ln -vsf {${PWD},${HOME}}/.$$item; \
	done
	ln -vsf ${ENV_SOURCE_DIR}/tokens/hub ${HOME}/.config/hub
	ln -sf ~/src/github.com/minorugh/dotfiles/.mutt/history ~/.mutt/history
	mkdir -p ${HOME}/.gnupg && ln -vsf ${PWD}/.gnupg/gpg-agent.conf ${HOME}/.gnupg/gpg-agent.conf

zsh-restore: ## zsh履歴を Dropbox からリストア
	cp -p ${HOME}/Dropbox/backup/env/zsh/.zsh_history ${HOME}/.zsh_history

init-sub: ## サブ機のgit push封鎖（メイン機以外で実行）
ifneq ($(HOSTNAME),$(MAIN_HOSTNAME))
	git -C ~/src/github.com/minorugh/dotfiles remote set-url --push origin no-push
	@echo "サブ機push封鎖完了"
else
	@echo "これはサブ機専用の処理です"
endif

########################################################
## システム設定（キーマップ・起動・cron）
########################################################
tig: ## tig の設定展開
	mkdir -p ${HOME}/.config/tig
	ln -vsf ${PWD}/.config/tig/config ${HOME}/.config/tig/config

# キー変換は全てここに集約する。
# - CapsLock→Ctrl: 標準XKBオプションのためシステムXKBで管理
# - PrtSc/ろ/F13-F17: ハードウェア固有のためxmodmapで管理（keydは重複のため2026.07.08廃止）
keymap: ## キーマップのカスタマイズ（CapsLock→Ctrl / .Xmodmap展開）
	ln -vsf {${PWD},${HOME}}/.Xmodmap
	sudo sed -i 's/^XKBOPTIONS=.*/XKBOPTIONS="ctrl:nocaps"/' /etc/default/keyboard
	sudo dpkg-reconfigure -f noninteractive keyboard-configuration
	setxkbmap -layout jp -option ctrl:nocaps

ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
grub: ## grub・lightdm・logind の設定（メイン機のみ）
	sudo ln -vsf ${PWD}/etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
	sudo ln -vsf ${PWD}/etc/systemd/logind.conf /etc/systemd/logind.conf
	sudo ln -vsf ${PWD}/etc/default/grub /etc/default/grub
	sudo update-grub2
endif

autostart: emacs-start ## GUI起動時の自動処理設定（SSH鍵自動入力・mozc同期）
	ln -vsf {${PWD},${HOME}}/.autostart.sh
	chmod +x ${HOME}/.autostart.sh
	ln -vsf {${PWD},${HOME}}/.config/autostart/autostart.desktop

########################################################
## bin/ スクリプト（シンボリックリンク＋キーボードショートカット登録）
########################################################
BIN_LINK = sudo ln -vsfn ${PWD}/bin/$(1) /usr/local/bin/$(2) && sudo chmod +x /usr/local/bin/$(2)

emacs-toggle: ## emacs-toggle のリンク作成 + F12ショートカット登録（Emacs最小化・復元）
	$(call BIN_LINK,emacs-toggle,emacs-toggle)
	xfconf-query -c xfce4-keyboard-shortcuts -p "/commands/custom/F12" -n -t string -s "emacs-toggle"

emacs-start: ## emacs-start.sh のリンク作成（autostart.sh から呼ばれる起動ラッパー）
	$(call BIN_LINK,emacs-start.sh,emacs-start.sh)

power-menu: ## power-menu.sh のリンク作成 + 全角半角ショートカット登録（電源メニュー）
	$(call BIN_LINK,power-menu.sh,power-menu.sh)
	xfconf-query -c xfce4-keyboard-shortcuts \
		-p "/commands/custom/Zenkaku_Hankaku" -n -t string \
		-s 'gnome-terminal --window --geometry=80x24+2000+100 -- bash -c "power-menu.sh"'

tile-toggle: ## tile-toggle.sh のリンク作成 + F15ショートカット登録（左右タイル切替）
	$(call BIN_LINK,tile-toggle.sh,tile-toggle)
	xfconf-query -c xfce4-keyboard-shortcuts -p "/commands/custom/F15" -n -t string -s "tile-toggle"

make-run: ## make-run.sh のリンク作成（Emacs経由のmake実行を安全化）
	$(call BIN_LINK,make-run.sh,make-run.sh)

.PHONY: cron automerge autobackup

automerge: ## automerge.sh のシンボリックリンク作成（cron自動実行用、メイン機のみ）
	sudo ln -vsfn ${PWD}/backup/automerge.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/automerge.sh

autobackup: ## cron スクリプト群のシンボリックリンク作成（cron自動実行用、メイン機のみ）
	sudo ln -vsfn ${PWD}/backup/autobackup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/autobackup.sh
	sudo ln -vsfn ${PWD}/backup/mozc-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/mozc-backup.sh
	sudo ln -vsfn ${PWD}/backup/melpa-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/melpa-backup.sh
	sudo ln -vsfn ${PWD}/backup/filezilla-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/filezilla-backup.sh
	sudo ln -vsfn ${PWD}/backup/gitea-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/gitea-backup.sh
	sudo ln -vsfn ${PWD}/backup/abook-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/abook-backup.sh
	sudo ln -vsfn ${PWD}/backup/xsrv-backup.sh /usr/local/bin
	sudo chmod +x /usr/local/bin/xsrv-backup.sh

emacs-trash: ## Emacs ゴミ箱スイープスクリプトのリンク作成
	sudo ln -vsfn ${PWD}/backup/emacs-trash-sweep.sh /usr/local/bin/emacs-trash-sweep.sh
	sudo chmod +x /usr/local/bin/emacs-trash-sweep.sh

cron: ## メイン機: automerge/autobackup のリンク作成+crontab反映 / サブ機: crontab反映のみ
ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
	$(MAKE) automerge
	$(MAKE) autobackup
	crontab ${PWD}/cron/crontab.p1
else
	crontab ${PWD}/cron/crontab.sub
endif

dropbox-watch: ## dropbox-watch.service のリンク作成+有効化
	$(APT) libnet-dbus-perl
	mkdir -p ${HOME}/.config/systemd/user
	ln -vsf ${PWD}/.config/systemd/user/dropbox-watch.service ${HOME}/.config/systemd/user/dropbox-watch.service
	systemctl --user daemon-reload
	systemctl --user enable --now dropbox-watch.service

ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
night-suspend: ## night-suspend.service/timer のリンク作成+有効化（メイン機のみ・復帰は手動）
	mkdir -p ${HOME}/.config/systemd/user
	ln -vsf ${PWD}/.config/systemd/user/night-suspend.service ${HOME}/.config/systemd/user/night-suspend.service
	ln -vsf ${PWD}/.config/systemd/user/night-suspend.timer ${HOME}/.config/systemd/user/night-suspend.timer
	systemctl --user daemon-reload
	systemctl --user enable --now night-suspend.timer
endif

########################################################
## パッケージ・ツール・フォント
########################################################
install: ## Debian パッケージの一括インストール
	$(APT) $(PACKAGES)

base: ## ビルド用ベースパッケージのインストール
	$(APT) $(BASE_PKGS)

tlp: ## 省電力・バッテリー劣化防止設定（TLP）
	$(APT) $@ tlp-rdw powertop
	sudo ln -vsf ${PWD}/etc/tlp.conf /etc/tlp.conf
	sudo tlp start
# TLP 1.4以降の正式設定ファイルは /etc/tlp.conf（旧 /etc/default/tlp は廃止）
# 充電しきい値の変更は dotfiles/cron/Makefile の bat-set-60 / bat-set-80 を使うこと

keyring: ## Gnome keyring の初期化（両機: Dropboxからコピー / メイン機: 夜間バックアップcron登録）
	$(APT) seahorse libsecret-tools
	test -L ${HOME}/.local/share/keyrings && rm -f ${HOME}/.local/share/keyrings || true
	mkdir -p ${HOME}/.local/share/keyrings
	rsync -av --delete ${HOME}/Dropbox/backup/keyrings/ ${HOME}/.local/share/keyrings/

fzf-tools: power-menu ## fzf最新版インストール
	sudo apt remove -y fzf 2>/dev/null || true
	cd ${HOME}/Downloads && \
	wget -q https://github.com/junegunn/fzf/releases/download/v0.72.0/fzf-0.72.0-linux_amd64.tar.gz && \
	tar xzf fzf-0.72.0-linux_amd64.tar.gz && \
	sudo mv fzf /usr/local/bin/fzf && \
	rm -f fzf-0.72.0-linux_amd64.tar.gz

icons: ## アイコン・壁紙のシンボリックリンク作成
	ln -vsf ${HOME}/Dropbox/backup/icons/* ${HOME}/Pictures

fonts: ## ユーザーフォントのシンボリックリンク作成
	test -L ${HOME}/.local/share/fonts || rm -rf ${HOME}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.local/share/fonts
	ln -vsfn {${PWD},${HOME}}/.fonts

gist: ## gist コマンドのインストールと設定
	gem install gist
	sudo cp $$(ruby -e 'puts Gem.user_dir')/bin/gist /usr/local/bin/gist
	ln -vsf ${ENV_SOURCE_DIR}/tokens/gist ${HOME}/.gist

########################################################
## アプリケーション（baseinstall / nextinstall 対象）
########################################################
emacs-mozc: ## Emacs + Mozc のインストール（Emacs本体もここで導入）
	$(APT) $@ fcitx-mozc
	test -L ${HOME}/.mozc || rm -rf ${HOME}/.mozc
	ln -vsn ~/Dropbox/backup/mozc/.mozc ~/
# Debian12: emacs 29.1 がインストールされる
# 現在は emacs-stable/devel で自前ビルド(30.1)を使用中
# Debian13以降では emacs 30.1 が標準になる見込みのため自前ビルド不要になるかも
# fcitx の設定: ターミナルで im-config を実行 → Enter → fcitx を選択

google-chrome: ## Google Chrome のインストール
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
	$(APT) ./google-chrome-stable_current_amd64.deb
	rm -f ./google-chrome-stable_current_amd64.deb

filezilla: ## FileZilla のインストールと設定 + filezilla.sh のリンク作成
	$(APT) $@
	rm -rf ${HOME}/.config/filezilla
	mkdir -p ${HOME}/.config/filezilla
	cp -a ${HOME}/Dropbox/backup/filezilla/config/. ${HOME}/.config/filezilla/
	chmod 700 ${HOME}/.config/filezilla
	mkdir -p ${HOME}/.local/bin
	ln -vsf {${PWD},${HOME}/.local}/bin/filezilla.sh
	chmod +x ${HOME}/.local/bin/filezilla.sh
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/filezilla.desktop

gitk: ## gitkのインストールと設定ファイル
	$(APT) $@
	mkdir -p ${HOME}/.config/git
	sudo ln -vsfn ${PWD}/.config/git/gitk ${HOME}/.config/git/gitk

neomutt: ## NeoMutt の設定 + neomutt.sh のリンク作成 + Super+Zショートカット登録
	$(APT) $@ urlscan abook
	mkdir -p ${HOME}/.mutt/cache/headers ${HOME}/.mutt/cache/bodies
	ln -vsf ${PWD}/.muttrc ${HOME}/.muttrc
	for item in mailcap certificates abook-add.sh; do \
		ln -vsf {${PWD},${HOME}}/.mutt/$$item; \
	done
	$(call BIN_LINK,neomutt.sh,neomutt.sh)
	xfconf-query -c xfce4-keyboard-shortcuts -p "/commands/custom/<Super>z" -n -t string -s "neomutt.sh"
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/neomutt.desktop
	mkdir -p ${HOME}/.config/urlscan
	ln -vsfn {${PWD},${HOME}}/.config/urlscan/config.toml

sxiv: ## sxiv の設定
	$(APT) $@
	mkdir -p ${HOME}/.config/sxiv/exec
	ln -vsf {${PWD},${HOME}}/.config/$@/exec/image-info && chmod +x $$_
# ファイル名に & などの特殊文字が含まれる場合は表示できないことがある

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

printer: ## CUPS導入＋cups-browsed重複プリンター対策（Brother HL-L2375DW）
	sudo apt install -y cups cups-browsed
	grep -qF 'BrowseFilter NOT name Brother_HL_L2375DW_series' /etc/cups/cups-browsed.conf || \
		echo 'BrowseFilter NOT name Brother_HL_L2375DW_series' | sudo tee -a /etc/cups/cups-browsed.conf
	sudo systemctl restart cups-browsed
	sudo lpadmin -x Brother_HL_L2375DW_series 2>/dev/null || true
	@echo "✓ CUPS セットアップ完了。http://localhost:631 でプリンターを追加してください。"
# Brother HL-L2375DW はドライバ不要（driverless対応）
# cups-browsed がアンダースコア版を自動追加して2枚印刷になる問題を抑制

########################################################
## 言語・ビルド（Go / Hugo / Emacs / TeX）
########################################################
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

emacs-stable: ##! Emacs 安定版のソースビルド（apt build-depで対話確認あり）
	cd ~/src/ && \
	wget -c https://ftpmirror.gnu.org/emacs/emacs-30.2.tar.gz && \
	tar xvfz emacs-30.2.tar.gz && \
	cd emacs-30.2 && \
	sudo apt build-dep emacs-gtk && \
	./configure \
	  --with-native-compilation=aot \
	  --with-x-toolkit=gtk3 \
	  --without-xim \
	  --without-gsettings \
	  --without-dbus \
	  --without-toolkit-scroll-bars \
	  --without-imagemagick \
	  --without-mailutils \
	  --without-pop \
	  --without-gpm \
	  --without-selinux \
	  --without-compress-install && \
	NATIVE_FULL_AOT=1 make -j$(nproc) && \
	sudo make install
	@echo "## アンインストール: ~/src/emacs-30.2 で sudo make uninstall && make distclean"

texlive: ##! TeX Live のインストール（scheme-medium + 日本語）
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

texlive-full: ##! TeX Live フルインストール（全パッケージ約7GB）
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

github: ## GitHub リポジトリのクローン
	mkdir -p ${HOME}/src/github.com/minorugh
	cd ${HOME}/src/github.com/minorugh; \
	git clone git@github.com:minorugh/GH.git; \
	git clone git@github.com:minorugh/minorugh.com.git; \
	git clone git@github.com:minorugh/minorugh.github.io.git; \
	git clone git@github.com:minorugh/upsftp.git; \
	git clone git@github.com:minorugh/env-import.git; \
	git clone git@github.com:minorugh/git-peek.git; \
	git clone git@github.com:minorugh/xsrv-GH.git; \
	git clone git@github.com:minorugh/xsrv-minorugh.git
# GH.git minorugh.com.git は .git のみ残して他は削除（本体は~/Dropbox）

github-remote-add: ## xsrv-GH / xsrv-minorugh に Gitea pushurl を追加（GitHub + Gitea の両方に push）
	git -C ${HOME}/src/github.com/minorugh/xsrv-GH remote set-url --push origin git@github.com:minorugh/xsrv-GH.git
	git -C ${HOME}/src/github.com/minorugh/xsrv-minorugh remote set-url --push origin git@github.com:minorugh/xsrv-minorugh.git
	git -C ${HOME}/src/github.com/minorugh/xsrv-GH remote set-url --add --push origin http://localhost:3000/minoru/xsrv-GH.git
	git -C ${HOME}/src/github.com/minorugh/xsrv-minorugh remote set-url --add --push origin http://localhost:3000/minoru/xsrv-minorugh.git
	@echo "Gitea pushurl added to xsrv-GH and xsrv-minorugh."
# git clone 直後は GitHub のみが remote。このターゲットで Gitea を pushurl に追加する。
# push 時は GitHub と Gitea 両方に送信される（fetch は GitHub のみ）


########################################################
## 対話実行系（個別に手動で実行するターゲット）
########################################################
dracula-theme: ##! gnome-terminal に Dracula テーマを dconf で適用
	dconf reset -f /org/gnome/terminal/legacy/profiles:/
	dconf load /org/gnome/terminal/legacy/profiles:/ < ${PWD}/etc/gnome-terminal/dracula.dconf

keepassxc: ##! KeePassXC のインストールと自動起動設定 + keepassxc.sh のリンク作成
	$(APT) $@ libsecret-tools
	$(call BIN_LINK,keepassxc.sh,keepassxc.sh)
	ln -vsfn {${PWD},${HOME}}/.local/share/applications/keepass-auto.desktop
# 初回セットアップ時に一度だけ実行してパスワードを登録する:
#   sudo secret-tool store --label "KeePassXC master password" type kbd
# 以降は以下で起動可能:
#   secret-tool lookup type kdb | keepassxc --pw-stdin /path/to/keepassxc.kdb

google-earth: ##! Google Earth のインストール
	cd ${HOME}/Downloads && \
	wget https://dl.google.com/dl/earth/client/current/google-earth-pro-stable_current_amd64.deb
	$(APT) ./google-earth-pro-stable_current_amd64.deb
	rm -f ./google-earth-pro-stable_current_amd64.deb

slack: ##! Slack デスクトップのインストール
	cd ${HOME}/Downloads && \
	wget https://slack.com/downloads/instructions/linux?ddl=1&build=deb
	sudo gdebi slack-desktop-4.49.89-amd64.deb # ファイル名はダウンロード時に確認すること
	rm -f slack-desktop-4.49.89-amd64.deb
	ln -vsf {${PWD},${HOME}}/.config/autostart/slack.desktop

flatpak: ##! flatpak 経由で Pinta・Spotify をインストール
	$(APT) $@
	sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
	flatpak install flathub com.github.PintaProject.Pinta
	flatpak install flathub com.spotify.Client
# アンインストール: flatpak uninstall --delete-data com.github.PintaProject.Pinta
# アンインストール: flatpak uninstall --delete-data com.spotify.Client

########################################################
## Docker（Step 0 → Step 1 の順で実行）
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
# 確認: docker --version && docker compose version

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

########################################################
## Debian インストール後の手動カスタマイズ覚書
########################################################
# 1. ウィンドウマネージャー>> スタイル: Arc-Dark、ウィンドウ切り替え(Super+Alt)、アプリ切り替え(Ctrl+Super)、非表示(End)
# 2. 外観設定>> スタイル: Arc-Dark、フォントサイズ: 14
# 3. 印刷設定>> sudo system-config-printer で設定
# 4. キーボード設定>> emacs:s-e, chrome:s-x, gnome-terminal:C-z, screenshot -r:Alt+Shift, -w:Alt+Ctrl
# 5. セッションと起動>> 自動起動アプリに .autostart.sh を追加（最小化起動）
# 6. セッションと起動>> 「ログアウト時にセッションを自動保存」のチェックを外す
# 7. パネルのアクションボタン>> ログアウト時の「セッションを保存する」のチェックを外す

########################################################
## git（実体は git/Makefile。日々の運用ワークフロー）
########################################################
.PHONY: git git-fix

git: ##! Auto commit+push（詳細は git/Makefile 参照）
	$(MAKE) -s -C git git

git-fix: ## rebase失敗時の自動修復
	$(MAKE) -s -C git git-fix


# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# タブ崩れなどの構文エラー確認（missing separator対策）
#   make -n >/dev/null
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
