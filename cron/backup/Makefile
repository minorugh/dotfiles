## ~/src/github.com/minorugh/dotfiles/cron/backup/Makefile
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
all: melpa git-push mattermost-backup mozc-backup thunderbird-backup filezilla-backup gitea-backup keyring-backup

.ONESHELL:

## Emacs ELPAパッケージを ~/Dropbox/backup/elpa へ rsync + git push（Gitea・Xserver）
ELPA_SRC = ${HOME}/src/github.com/minorugh/dotfiles/.emacs.d/elpa
ELPA_BAK = ${HOME}/Dropbox/backup/elpa

melpa: ## MELPAパッケージを ~/Dropbox/backup/elpa へ バックアップ
	rsync -av $(ELPA_SRC)/ $(ELPA_BAK)/
	cd $(ELPA_BAK) && \
	git add . && \
	git commit -m "auto ELPA: $$(date '+%Y-%m-%d %H:%M:%S')" || echo "No changes to commit" && \
	git push

## 各リポジトリの日次 auto-commit + push（pushurl で GitHub・Xserver に同時送信）
git-push: ## dropbox/GH dropbox/minorugh.com を auto commit 
	$(MAKE) -C ${HOME}/Dropbox/GH git
	$(MAKE) -C ${HOME}/Dropbox/minorugh.com git

## Mattermost の config+data+DB を Dropbox にバックアップ
mattermost-backup: ## Mattermost config+data+DB を Dropbox にバックアップ
	/usr/local/bin/mattermost-backup.sh

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

## endfile

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
