# cron/systemd-user 緊急操作パネル
# 緊急停止: make（または make -k）
# crontab反映: make crontab

all: xsrv-stop

EMACS_MSG = emacsclient -n --eval '(my-make-message "$1")'

cron-log: ## cron 実行ログ表示
	emacsclient -n -e '(my-open-cron-log)'
# emacsclient -n -e '(progn (find-file "/tmp/cron.log") (goto-char (point-max)) (recenter -30) (delete-other-windows))'

xsrv-log: ## xsrv-backup ログ表示
	emacsclient -n -e '(my-open-xsrv-log)'
# emacsclient -n -e '(progn (find-file "/tmp/xsrv-backup.log") (goto-char (point-max)) (delete-other-windows))'

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
