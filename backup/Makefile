## backup/Makefile
#### 夜間自動バックアップ（autobackup.sh から呼び出される）
##
## 修正履歴:
## 2026-03-14  dotfiles/gh ターゲットを rsync から git push 方式に移行
## 2026-03-26  melpa を Dropbox makefile に直接統合（.emacs.d/Makefile 委譲廃止）
##             rsync + git push（Gitea・Xserver）方式に変更
## 2026-06-07  melpa ターゲットを melpa-backup.sh に移行
##             ディレクトリ単位の変更ログを Dropbox/CHANGELOG/elpa/ に蓄積
## 2026-06-16  git-push ターゲットに xsrv-GH・xsrv-minorugh を追加
## 2026-06-20  cronにあったスクリプトをbackup-dir に移動
## 2026-06-21  melpa-backup.sh から git管理を廃止
##             PAST/<パッケージ名>/ への世代退避方式（最大5世代、FIFO）に変更
##             変更ログの出力先を Dropbox/CHANGELOG/elpa/ から
##             Dropbox/backup/elpa/LOG/ に変更## 詳細: dotfiles/backup/README.md
## 2026-07-10  Thunderbird廃止（neomutt+Gmail Webへ移行）に伴い thunderbird-backup ターゲットを削除

a.out: all

## 毎夜自動実行するバックアップ一式
all: melpa git-push mozc-backup filezilla-backup gitea-backup keyring-backup zsh-history-backup abook-backup readmes-backup

.ONESHELL:

melpa: ## MELPAパッケージを ~/Dropbox/backup/elpa へ バックアップ
	/usr/local/bin/melpa-backup.sh

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
