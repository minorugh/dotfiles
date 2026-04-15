## ~/src/github.com/minorugh/dotfiles/cron/Makefile
## cron/systemd-user 手動操作パネル
## 作成: 2026-04-09
##
## --- compilation バッファー表示ルール ----------------------------
## @echo "##>" >&2          → 全画面表示（q で閉じる）※出力を確認したいターゲット
## @echo "##> メッセージ" >&2 → ウィンドウを閉じてminibufferにメッセージ表示
## echoなし                 → Compile successful. がminibufferに表示されるだけ
## ----------------------------------------------------------------

## 緊急停止:      make（または make -k）
## crontab編集:   make cron-edit
## crontab反映:   make cron-update
## commitリセット: make xsrv-commit-lean（半年に1回程度）
## バッテリー確認: make bat-status
## バッテリーしきい値変更: make bat-set-60 / make bat-set-80
## 温度確認:      make temp
## SSD確認:       make smart

XSRV_GH_DST  = $(HOME)/src/github.com/minorugh/xsrv-GH
XSRV_MN_DST  = $(HOME)/src/github.com/minorugh/xsrv-minorugh
HOSTNAME := $(shell hostname)

all: xsrv-stop

cron-log: ## cron 実行ログ表示
	emacsclient -n -e '(progn (find-file "/tmp/cron.log")(goto-char (point-max)) (delete-other-windows))'

cron-edit: ## crontab を編集（dotfiles/cron/crontab を直接編集）
	gnome-terminal -- bash -c "nano ${PWD}/crontab; exec bash"

cron-update: ## 編集した crontab を反映（バックアップ→適用→確認）
	crontab -l > crontab.backup.$$(date +%Y%m%d) 2>/dev/null || true
	crontab crontab
	gnome-terminal -- bash -c "crontab -l; echo; read -p 'Press Enter to close...'"

automerge: ## automerge.sh を今すぐ手動実行
	/usr/local/bin/automerge.sh >> /tmp/cron.log 2>&1

autobackup: ## autobackup.sh を今すぐ手動実行
	/usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1

xsrv-log: ## xsrv-backup ログ表示
	emacsclient -n -e '(progn (find-file "/tmp/xsrv-backup.log")(goto-char (point-max)) (delete-other-windows))'

xsrv-stop: ## xsrv-backup 緊急停止
	systemctl --user stop xsrv-backup.timer
	@echo "##> xsrv-backup: stopped." >&2

xsrv-start: ## xsrv-backup 再開
	systemctl --user start xsrv-backup.timer
	@echo "##> xsrv-backup: started." >&2

xsrv-status: ## xsrv-backup 状態確認
	systemctl --user status xsrv-backup.timer

xsrv-reload: ## systemd 設定変更後のリロード
	systemctl --user daemon-reload
	@echo "##> xsrv-backup: daemon reloaded." >&2

xsrv-run: ## xsrv-backup 今すぐ手動実行
	systemctl --user start xsrv-backup.service
	@echo "##> xsrv-backup: manual run triggered." >&2

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

## -------------------------------------------------------------------------
## バッテリーメンテ（TLP / ThinkPad P1 + Debian12）
## -------------------------------------------------------------------------
## 常時AC接続運用のため、充電量を抑えてバッテリー劣化を防ぐ。
## /etc/tlp.conf に START=40 / STOP=60 を設定済み（再起動後も永続）。
##
## 通常確認:   make bat-status
## 節約モード: make bat-set-60  （長期AC接続・外出なし）
## 標準モード: make bat-set-80  （外出予定あり・持ち出し前など）
## -------------------------------------------------------------------------

bat-status: ## バッテリー状態・しきい値を確認
	sudo tlp-stat -b
	@echo "##>" >&2

bat-set-60: ## 充電上限を60%に変更（長期AC接続時推奨）
	sudo sed -i 's/^STOP_CHARGE_THRESH_BAT0=.*/STOP_CHARGE_THRESH_BAT0=60/' /etc/tlp.conf
	sudo sed -i 's/^STOP_CHARGE_THRESH_BAT1=.*/STOP_CHARGE_THRESH_BAT1=60/' /etc/tlp.conf
	sudo tlp start
ifeq ($(HOSTNAME),P1)
	@echo "##> BAT0 charge threshold: START=40 STOP=60" >&2
else
	@echo "##> BAT0/BAT1 charge threshold: START=40 STOP=60" >&2
endif

bat-set-80: ## 充電上限を80%に変更（外出前・標準運用）
	sudo sed -i 's/^STOP_CHARGE_THRESH_BAT0=.*/STOP_CHARGE_THRESH_BAT0=80/' /etc/tlp.conf
	sudo sed -i 's/^STOP_CHARGE_THRESH_BAT1=.*/STOP_CHARGE_THRESH_BAT1=80/' /etc/tlp.conf
	sudo tlp start
ifeq ($(HOSTNAME),P1)
	@echo "##> BAT0 charge threshold: START=40 STOP=80" >&2
else
	@echo "##> BAT0/BAT1 charge threshold: START=40 STOP=80" >&2
endif

bat-discharge: ## 強制放電開始（P1=BAT0:force-discharge / x250=BAT0:inhibit+BAT1:force）
ifeq ($(HOSTNAME),P1)
	echo force-discharge | sudo tee /sys/class/power_supply/BAT0/charge_behaviour
	@echo "##> BAT0: force-discharge started. Run 'make bat-discharge-stop' when below 60%." >&2
else
	echo inhibit-charge | sudo tee /sys/class/power_supply/BAT0/charge_behaviour
	echo force-discharge | sudo tee /sys/class/power_supply/BAT1/charge_behaviour
	@echo "##> BAT0: inhibit-charge / BAT1: force-discharge started. Run 'make bat-discharge-stop' when below 60%." >&2
endif

bat-discharge-stop: ## 強制放電停止（autoに戻す）
ifeq ($(HOSTNAME),P1)
	echo auto | sudo tee /sys/class/power_supply/BAT0/charge_behaviour
	@echo "##> BAT0: charge_behaviour restored to auto." >&2
else
	echo auto | sudo tee /sys/class/power_supply/BAT0/charge_behaviour
	echo auto | sudo tee /sys/class/power_supply/BAT1/charge_behaviour
	@echo "##> BAT0/BAT1: charge_behaviour restored to auto." >&2
endif

bat-capacity: ## 現在の充電残量を確認
ifeq ($(HOSTNAME),P1)
	@cat /sys/class/power_supply/BAT0/capacity
else
	@cat /sys/class/power_supply/BAT0/capacity
	@cat /sys/class/power_supply/BAT1/capacity
endif
	@echo "##>" >&2

## -------------------------------------------------------------------------
## ハードウェア健康診断（常時稼働サーバー用途向け）
## -------------------------------------------------------------------------
## Docker常時稼働・蓋閉め運用のため、定期的に温度とSSD状態を確認する。
##
## 温度確認:    make temp    （月1回程度、高負荷時は随時）
## SSD確認:     make smart   （3〜6ヶ月に1回程度）
##
## 温度の目安（要注意ライン）:
##   CPU 80°C以上 / GPU 85°C以上 / NVMe 70°C以上
## SSDの要注意サイン:
##   Percentage Used が 90% 以上
##   Available Spare が 10% 以下
##   Media and Data Integrity Errors が 1 以上
## -------------------------------------------------------------------------

temp: ## CPU/GPU/SSD温度を確認（lm-sensors）
	sensors
	@echo "##>" >&2

smart: ## SSD健康状態を確認（nvme0n1 / nvme1n1）
	sudo smartctl -a /dev/nvme0n1
	sudo smartctl -a /dev/nvme1n1
	@echo "##>" >&2


# HOSTNAME := $(shell hostname)
# Hostname-based git push control:
# P1 (main): commit + push / others (sub): pull --rebase only
git: ## Auto commit+push (main only, sub: pull only)
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
ifeq ($(HOSTNAME),P1)
	git push
else
	@echo "##> $(HOSTNAME): サブ機からはpushしません" >&2
	git pull --rebase
endif

debug:
	@echo "HOSTNAME=[$(HOSTNAME)]"
	@echo "shell=[$(shell hostname)]"
	@echo "##>" >&2

# Local Variables:
# buffer-read-only: t
# End:
