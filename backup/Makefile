## ~/src/github.com/minorugh/dotfiles/backup/Makefile
##
## cron: autobackup.sh から呼び出される（直接 cron 実行ではない）
##
## 修正履歴:
## 2026-03-14  dotfiles/gh ターゲットを rsync から git push 方式に移行
## 2026-03-26  melpa を Dropbox makefile に直接統合（.emacs.d/Makefile 委譲廃止）
##             rsync + git push（Gitea・Xserver）方式に変更

# Inherit SSH_AUTH_SOCK from keychain
a.out: all

## 毎夜自動実行するバックアップ一式
all: melpa git-push mozc-backup thunderbird-backup filezilla-backup gitea-backup keyring-backup zsh-history-backup abook-backup readmes-backup

.ONESHELL:

## Emacs ELPAパッケージを ~/Dropbox/backup/elpa へ rsync + git push（Gitea・Xserver）
ELPA_SRC = ${HOME}/src/github.com/minorugh/dotfiles/.emacs.d/elpa
ELPA_BAK = ${HOME}/Dropbox/backup/elpa

melpa: ## MELPAパッケージを ~/Dropbox/backup/elpa へ バックアップ
	rsync -av --delete --exclude='.git' \
	  --itemize-changes $(ELPA_SRC)/ $(ELPA_BAK)/ > /tmp/elpa-rsync.log 2>&1
	cd $(ELPA_BAK) && git add -A && \
	git diff --cached --quiet && \
	echo "[melpa] 更新はありません。" || \
	{ ADDED=$$(grep '^>f+' /tmp/elpa-rsync.log | wc -l); \
	  UPDATED=$$(grep '^>f.' /tmp/elpa-rsync.log | grep -v '^>f+' | wc -l); \
	  DELETED=$$(grep '^*deleting' /tmp/elpa-rsync.log | wc -l); \
	  LOGFILE="$(HOME)/Dropbox/CHANGELOG/elpa/elpa-changes.log"; \
	  mkdir -p $$(dirname $$LOGFILE); \
	  echo "$$(date '+%Y-%m-%d %H:%M') [+$$ADDED ~$$UPDATED -$$DELETED]" >> $$LOGFILE; \
	  grep -E '^>f|^\*deleting' /tmp/elpa-rsync.log >> $$LOGFILE; \
	  echo "" >> $$LOGFILE; \
	  git commit -m "auto ELPA: $$(date '+%Y-%m-%d %H:%M:%S') [+$$ADDED ~$$UPDATED -$$DELETED]"; \
	  echo "[melpa] コミット＆プッシュしました。"; }

## 各リポジトリの日次 auto-commit + push（pushurl で GitHub・Xserver に同時送信）
git-push: ## dropbox/GH dropbox/minorugh.com を auto commit
	$(MAKE) -C ${HOME}/Dropbox/GH git
	$(MAKE) -C ${HOME}/Dropbox/minorugh.com git

## P1 の ~/.mozc を Dropbox にバックアップ
mozc-backup: ## P1 の ~/.mozc を Dropbox にバックアップ
	/usr/local/bin/mozc-backup.sh

## Thunderbird バックアップ
thunderbird-backup: ## Thunderbird バックアップ
	/usr/local/bin/thunderbird-backup.sh

## FileZilla 設定を Dropbox にバックアップ
filezilla-backup: ## FileZilla 設定を Dropbox にバックアップ
	/usr/local/bin/filezilla-backup.sh

## Gitea data を Dropbox にバックアップ
gitea-backup: ## Gitea data を Dropbox にバックアップ
	/usr/local/bin/gitea-backup.sh

## Gnome keyring を Dropbox にバックアップ（P1 の実体 → Dropbox）
keyring-backup: ## Gnome keyring を Dropbox にバックアップ
	rsync -av --delete ${HOME}/.local/share/keyrings/ ${HOME}/Dropbox/backup/keyrings/

## zsh履歴を Dropbox にバックアップ
zsh-history-backup: ## ~/.zsh_history を Dropbox にバックアップ
	cp -p ${HOME}/.zsh_history ${HOME}/Dropbox/backup/env/zsh/.zsh_history

abook-backup:  ## ~/.abook/addressbook を GPG 暗号化して Dropbox にバックアップ
	/usr/local/bin/abook-backup.sh


readmes-backup: ## 各所のREADMEをDropbox/READMESにコピー
	@mkdir -p ${HOME}/Dropbox/READMES
	@cp ${HOME}/src/github.com/minorugh/env-import/README.md       ${HOME}/Dropbox/READMES/00-env-import-README.md
	@cp ${HOME}/.env_source/README.md                              ${HOME}/Dropbox/READMES/01-env_source-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/README.md         ${HOME}/Dropbox/READMES/02-dotfiles-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/backup/README.md  ${HOME}/Dropbox/READMES/03-backup-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/cron/README.md    ${HOME}/Dropbox/READMES/04-cron-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/.mutt/README.md   ${HOME}/Dropbox/READMES/05-mutt-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/bin/README.md     ${HOME}/Dropbox/READMES/06-bin-README.md
	@cp ${HOME}/src/github.com/minorugh/dotfiles/docker/README.md  ${HOME}/Dropbox/READMES/07-docker-README.md
	@cp ${HOME}/Dropbox/backup/README.md                           ${HOME}/Dropbox/READMES/08-Dropbox-README.md
	@echo "$$(date '+%Y-%m-%d %H:%M:%S') readmes backup done"

## endfile

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
