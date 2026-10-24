## backup/Makefile
#### 夜間自動バックアップ（autobackup.sh から呼び出される）
##
## 修正履歴:
## 2026-03-14  dotfiles/gh ターゲットを rsync から git push 方式に移行
## 2026-06-16  git-push ターゲットに xsrv-GH・xsrv-minorugh を追加
## 2026-06-20  cronにあったスクリプトをbackup-dir に移動
## 2026-07-10  Thunderbird廃止（neomutt+Gmail Webへ移行）に伴い thunderbird-backup ターゲットを削除
## 2026-10-16  Gitea 廃止に伴い gitea-backup ターゲットと gitea-backup.sh を削除
## 2026-10-17  melpa-backup.sh 廃止に伴い melpa ターゲットを削除（all: からも除去）
## 2026-10-17  abook は ~/.env_source の暗号化 bundle で保護されるため all: から abook-backup を削除

a.out: all

## 毎夜自動実行するバックアップ一式
all: git-push mozc-backup filezilla-backup keyring-backup zsh-history-backup keychron-rules-backup readmes-backup

.ONESHELL:

## 各リポジトリの日次 auto-commit + push（pushurl で GitHub・Xserver に同時送信）
git-push: ## GH minorugh.com xsrv-GH xsrv-minorugh を auto commit
	$(MAKE) -C ${HOME}/Dropbox/GH git
	$(MAKE) -C ${HOME}/Dropbox/minorugh.com git
	$(MAKE) -C ${HOME}/src/github.com/minorugh/xsrv-GH git
	$(MAKE) -C ${HOME}/src/github.com/minorugh/xsrv-minorugh git

## P1 の ~/.mozc を Dropbox にバックアップ
mozc-backup: ## P1 の ~/.mozc を Dropbox にバックアップ
	/usr/local/bin/mozc-backup.sh

## FileZilla 設定を Dropbox にバックアップ
filezilla-backup: ## FileZilla 設定を Dropbox にバックアップ
	/usr/local/bin/filezilla-backup.sh


## Gnome keyring を Dropbox にバックアップ（P1 の実体 → Dropbox）
keyring-backup: ## Gnome keyring を Dropbox にバックアップ
	rsync -av --delete ${HOME}/.local/share/keyrings/ ${HOME}/Dropbox/backup/keyrings/

## zsh履歴を Dropbox にバックアップ
zsh-history-backup: ## ~/.zsh_history を Dropbox にバックアップ
	cp -p ${HOME}/.zsh_history ${HOME}/Dropbox/backup/env/zsh/.zsh_history

## Keychron udevルール（Launcherが書き換える実体）を Dropbox にバックアップ
keychron-rules-backup: ## Keychron udevルールを Dropbox にバックアップ
	mkdir -p ${HOME}/Dropbox/backup/keychron
	cp -p /etc/udev/rules.d/50-keychron.rules ${HOME}/Dropbox/backup/keychron/50-keychron.rules

READMES_DIR := ${HOME}/Dropbox/backup/READMES

gsync: ## GitHub と xserver の HEAD が一致しているか確認（4 リポジトリ）
	@for d in $$HOME/src/github.com/minorugh/GH $$HOME/src/github.com/minorugh/minorugh.com $$HOME/cl $$HOME/i.minorugh.com; do \
		o=$$(git -C $$d ls-remote origin HEAD | cut -f1); \
		x=$$(git -C $$d ls-remote $$(git -C $$d config --get-all remote.origin.pushurl | tail -1) HEAD | cut -f1); \
		if [ -n "$$o" ] && [ "$$o" = "$$x" ]; then echo "$$(basename $$d) OK"; else echo "$$(basename $$d) NG"; ng=1; fi; \
	done; \
	[ -z "$$ng" ]

readmes-backup: ## 各所のREADMEを$(READMES_DIR)にコピー
	@mkdir -p $(READMES_DIR)
	@cp ${HOME}/src/github.com/minorugh/env-import/README.md       $(READMES_DIR)/00-env-import-README.md
	@cp ${HOME}/.env_source/README.md                              $(READMES_DIR)/01-env_source-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/README.md         $(READMES_DIR)/02-dotfiles-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/backup/README.md  $(READMES_DIR)/03-backup-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/cron/README.md    $(READMES_DIR)/04-cron-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/.mutt/README.md   $(READMES_DIR)/05-mutt-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/bin/README.md     $(READMES_DIR)/06-bin-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/git/README.md     $(READMES_DIR)/07-git-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/docker/README.md  $(READMES_DIR)/08-docker-README.md
	@echo "$$(date '+%Y-%m-%d %H:%M:%S') readmes backup done"

## endfile

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
