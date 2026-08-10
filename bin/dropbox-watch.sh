#!/bin/bash
#
# dropbox-watch.sh
#
# 判定方法: cronで実行されるたびに現在時刻をHEARTBEAT_FILEへ書き込む。
# 次回実行時前回時刻との差（gap）を計算しGAP_THRESHOLDを超えていたら
# 「その間cronが動けなかった＝サスペンドしていた」とみなし
# `pkill -x dropbox' で強制終了してから `dropbox start -i' で再起動する。
# ただし、その間にDropboxの通知（NOTIFIED_FILE）が確認できれば
# 自力で同期再開できたとみなしpkillはスキップする。
# 結果はLOGFILEに記録する。
#
# 使い方: cronで1分おきに実行する
#   * * * * * sleep 30; /path/to/dropbox-watch.sh >> /tmp/cron.log 2>&1
#
# GAP_THRESHOLD は cron間隔を前提にした値。
# cron間隔を変える場合は平常時の実行間隔の2〜3倍程度を目安に調整すること。
#
# Author: Minoru Yamada (aodamo)
# Created: 2026-08-07
# Updated: 2026-08-10
#
LOCKFILE="/tmp/dropbox-watch.lock"
exec 9>"$LOCKFILE"
flock -n 9 || exit 0

HOME_DIR=$(eval echo "~$USER")
HEARTBEAT_FILE="$HOME_DIR/.cache/dropbox-watch.heartbeat"
NOTIFIED_FILE="$HOME_DIR/.cache/dropbox-watch.notified"
MONITOR_PIDFILE="$HOME_DIR/.cache/dropbox-watch.monitor.pid"
LOGFILE="$HOME_DIR/.cache/dropbox-watch.log"
GAP_THRESHOLD=120

export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"

# Dropbox通知の監視プロセスが生きていなければ起動する
if ! kill -0 "$(cat "$MONITOR_PIDFILE" 2>/dev/null)" 2>/dev/null; then
    setsid dbus-monitor "interface='org.freedesktop.Notifications',member='Notify'" 2>/dev/null |
        while read -r line; do
            echo "$line" | grep -q 'string "Dropbox"' && date +%s > "$NOTIFIED_FILE"
        done &
    disown
    echo $! > "$MONITOR_PIDFILE"
fi

NOW=$(date +%s)

# GAPがGAP_THRESHOLD以上なら dropbox を再起動する
if [ -f "$HEARTBEAT_FILE" ]; then
    LAST=$(cat "$HEARTBEAT_FILE")
    GAP=$(( NOW - LAST ))
    if [ "$GAP" -ge "$GAP_THRESHOLD" ]; then
        # 復帰直後のWi-Fi/DBUS安定待ち。環境により調整（目安: Wi-Fi復帰時間+マージン）
        sleep 15

        NOTIFIED=0
        [ -f "$NOTIFIED_FILE" ] && NOTIFIED=$(cat "$NOTIFIED_FILE")

        if [ "$NOTIFIED" -gt "$LAST" ]; then
            echo "$(date '+%Y-%m-%d %H:%M:%S') last=$(date -d @${LAST} '+%m-%d %H:%M:%S') already synced, skip" >> "$LOGFILE"
        else
            pkill -x dropbox
            sleep 3
            dropbox start -i > /dev/null 2>&1
            echo "$(date '+%Y-%m-%d %H:%M:%S') last=$(date -d @${LAST} '+%m-%d %H:%M:%S') dropbox restarted" >> "$LOGFILE"
        fi
    fi
fi

# if文の分岐に関わらず、cronが実行されるたびに必ず上書きする
FINISH_TIME=$(date +%s)
echo "$FINISH_TIME" > "$HEARTBEAT_FILE"
