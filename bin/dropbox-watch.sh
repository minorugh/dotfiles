#!/bin/bash
#
# dropbox-watch.sh
#
# サスペンド復帰後にDropbox同期が「同期中…」のまま固まる問題への対策。
# dropbox status の文字列は固まっていても「最新の状態」と誤った自己申告を
# することがあるため、判定には使わず、cronの実行間隔（heartbeat）そのもの
# でサスペンド発生を検知する。
#
# 使い方: cronで1分おきに実行する
#   * * * * * sleep 30; /path/to/dropbox-watch.sh >> /tmp/cron.log 2>&1
#
# GAP_THRESHOLD は cron間隔を前提にした値。cron間隔を変える場合は
# 平常時の実行間隔の2〜3倍程度を目安に調整すること。
#
# Author: Minoru Yamada (aodamo)
# Created: 2026-08-07
#
LOCKFILE="/tmp/dropbox-watch.lock"
exec 9>"$LOCKFILE"
flock -n 9 || exit 0

HOME_DIR=$(eval echo "~$USER")
HEARTBEAT_FILE="$HOME_DIR/.cache/dropbox-watch.heartbeat"
LOGFILE="$HOME_DIR/.cache/dropbox-watch.log"
GAP_THRESHOLD=180

export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"

NOW=$(date +%s)

if [ -f "$HEARTBEAT_FILE" ]; then
    LAST=$(cat "$HEARTBEAT_FILE")
    GAP=$(( NOW - LAST ))
    if [ "$GAP" -ge "$GAP_THRESHOLD" ]; then
        sleep 15
        pkill -x dropbox
        sleep 3
        dropbox start -i > /dev/null 2>&1
        echo "$(date '+%Y-%m-%d %H:%M:%S') gap ${GAP}s detected (suspend likely), dropbox restarted" >> "$LOGFILE"
    fi
fi

echo "$NOW" > "$HEARTBEAT_FILE"
