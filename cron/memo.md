# Xserver Auto Backup System (systemd-user 版)

## 1. 設定ファイル
以下の2ファイルを ~/.config/systemd/user/ に配置する。

### [xsrv-backup.service]
```conf
[Unit]
Description=Xserver Backup Sync and Git Push

[Service]
Type=simple
ExecStart=/bin/bash -c "/usr/local/bin/xsrv-backup.sh > /tmp/xsrv-backup.log 2>&1"
Environment="HOME=%h"
Environment="PATH=/usr/local/bin:/usr/bin:/bin"
Environment="SHELL=/bin/bash"

[Install]
WantedBy=default.target
```

### [xsrv-backup.timer]

```conf
[Unit]
Description=Run Xserver Backup (0,9,12,15,18,21)

[Timer]
OnCalendar=00,09,12,15,18,21:00:00
Persistent=true
Unit=xsrv-backup.service

[Install]
WantedBy=timers.target
```

## 2. 運用コマンド
- ログ確認: cat /tmp/xsrv-backup.log
- 予定確認: systemctl --user list-timers
- 反映(編集後): systemctl --user daemon-reload
- 今すぐ実行: systemctl --user start xsrv-backup.service
- 緊急停止: systemctl --user stop xsrv-backup.timer
- タイマー再開: systemctl --user start xsrv-backup.timer

## 3. 仕様
- 実行のたびに /tmp/xsrv-backup.log を上書き（最新1回分のみ保持）。
- ログインセッションを共有するため、SSH鍵（keychain）環境を継承。
- スリープ復帰時、予定時刻を過ぎていれば即座に実行される。
