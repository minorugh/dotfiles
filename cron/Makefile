# crontab deploy helper
# 現在のcrontabを日付付きでbackupしてから反映
# 実行: make -k（cron/crontab を編集後）
# ターミナルを起動して crontab -l を表示

all:
	crontab -l > crontab.backup.$$(date +%Y%m%d) 2>/dev/null || true
	crontab crontab
	gnome-terminal -- bash -c "crontab -l; echo; read -p 'Press Enter to close...'"
