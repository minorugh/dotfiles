#!/bin/bash

# スクリプトの二重起動を防止（ロックファイル作成）
LOCKFILE="/tmp/dropbox-watch.lock"
exec 9>"$LOCKFILE"
flock -n 9 || exit 0

HOME_DIR=$(eval echo "~$USER")
HEARTBEAT_FILE="$HOME_DIR/.cache/dropbox-watch.heartbeat"
LOGFILE="$HOME_DIR/.cache/dropbox-watch.log"
GAP_THRESHOLD=180

NOW=$(date +%s)

if [ -f "$HEARTBEAT_FILE" ]; then
    LAST=$(cat "$HEARTBEAT_FILE")
    GAP=$(( NOW - LAST ))
    if [ "$GAP" -ge "$GAP_THRESHOLD" ]; then
        # ネットワーク接続の安定を待つ
        sleep 15
        pkill -x dropbox
        sleep 3

        # 動的にユーザーIDを取得して環境変数をセット
        USER_ID=$(id -u)
        export DISPLAY=:0
        export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/${USER_ID}/bus"

        dropbox start -i > /dev/null 2>&1
        echo "$(date '+%Y-%m-%d %H:%M:%S') gap ${GAP}s detected (suspend likely), dropbox restarted" >> "$LOGFILE"
    fi
fi

echo "$NOW" > "$HEARTBEAT_FILE"
