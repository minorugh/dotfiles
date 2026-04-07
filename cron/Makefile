# cron/systemd-user 緊急操作パネル
# 緊急停止: make（または make -k）
# crontab反映: make crontab

all: xsrv-stop

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

xsrv-log: ## xsrv-backup ログ表示
	@cat /tmp/xsrv-backup.log

xsrv-run: ## xsrv-backup 今すぐ手動実行
	systemctl --user start xsrv-backup.service
	@echo "xsrv-backup: manual run triggered."
