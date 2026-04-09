## ~/src/github.com/minorugh/dotfiles/cron/Makefile
## cron/systemd-user 手動操作パネル
## 作成: 2026-04-09
##
## 緊急停止:    make（または make -k）
## crontab反映: make crontab
## commitリセット: make xsrv-reset（半年に1回程度）

XSRV_GH_DST  = $(HOME)/src/github.com/minorugh/xsrv-GH
XSRV_MN_DST  = $(HOME)/src/github.com/minorugh/xsrv-minorugh

all: xsrv-stop

cron-log: ## cron 実行ログ表示
	emacsclient -n -e '(my-open-cron-log)'

xsrv-log: ## xsrv-backup ログ表示
	emacsclient -n -e '(my-open-xsrv-log)'

crontab: ## crontab をバックアップして反映、ターミナルで確認
	crontab -l > crontab.backup.$$(date +%Y%m%d) 2>/dev/null || true
	crontab crontab
	gnome-terminal -- bash -c "crontab -l; echo; read -p 'Press Enter to close...'"

xsrv-stop: ## xsrv-backup 緊急停止
	systemctl --user stop xsrv-backup.timer
	@echo "xsrv-backup: stopped."

xsrv-start: ## xsrv-backup 再開
	systemctl --user start xsrv-backup.timer
	@echo "xsrv-backup: started."

xsrv-status: ## xsrv-backup 状態確認
	systemctl --user status xsrv-backup.timer

xsrv-reload: ## systemd 設定変更後のリロード
	systemctl --user daemon-reload
	@echo "xsrv-backup: daemon reloaded."

xsrv-run: ## xsrv-backup 今すぐ手動実行
	systemctl --user start xsrv-backup.service
	@echo "xsrv-backup: manual run triggered."

## バックアップのcommitログが肥大するので適宜初期化する（半年に1回程度手動で）
## 実行すると両リポジトリの履歴が1件にリセットされ、GitHub に force push される
## ローカルファイルは一切消えない（最新状態のまま履歴だけリセット）
xsrv-commit-lean: ## バックアップの git commitログを初期化
	cd $(XSRV_GH_DST) && \
	git checkout --orphan newbranch && \
	git add -A && \
	git commit -m "reset: history truncated at $$(date '+%Y-%m-%d')" && \
	git branch -D main && \
	git branch -m main && \
	git push --force
	cd $(XSRV_MN_DST) && \
	git checkout --orphan newbranch && \
	git add -A && \
	git commit -m "reset: history truncated at $$(date '+%Y-%m-%d')" && \
	git branch -D main && \
	git branch -m main && \
	git push --force
