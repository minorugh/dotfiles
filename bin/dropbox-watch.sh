#!/bin/bash
#
# dropbox-watch.sh
#
# 判定方法: cronで実行されるたびに現在時刻をHEARTBEAT_FILEへ書き込む。
# 次回実行時前回時刻との差（gap）を計算しGAP_THRESHOLDを超えていたら
# 「その間cronが動けなかった＝サスペンドしていた」とみなし
# `pkill -x dropbox' で強制終了してから `dropbox start -i' で再起動する。
# 結果はLOGFILEに記録する。
#
# 使い方: cronで1分おきに実行する
#   * * * * * sleep 30; /path/to/dropbox-watch.sh >> /tmp/cron.log 2>&1
#
# GAP_THRESHOLD は cron間隔を前提にした値。
# cron間隔を変える場合は平常時の実行間隔の2〜3倍程度を目安に調整すること。
#
# Author: Minoru Yamada (aodamo)
# Created: 2026-08-09
#
LOCKFILE="/tmp/dropbox-watch.lock"
exec 9>"$LOCKFILE"
flock -n 9 || exit 0

HOME_DIR=$(eval echo "~$USER")
HEARTBEAT_FILE="$HOME_DIR/.cache/dropbox-watch.heartbeat"
LOGFILE="$HOME_DIR/.cache/dropbox-watch.log"
GAP_THRESHOLD=120

export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"

NOW=$(date +%s)

# GAPがGAP_THRESHOLD以上なら dropbox を再起動する
if [ -f "$HEARTBEAT_FILE" ]; then
    LAST=$(cat "$HEARTBEAT_FILE")
    GAP=$(( NOW - LAST ))
    if [ "$GAP" -ge "$GAP_THRESHOLD" ]; then
	# 復帰直後のWi-Fi/DBUS安定待ち。環境により調整（目安: Wi-Fi復帰時間+マージン）
        sleep 10
        pkill -x dropbox
        sleep 3
        dropbox start -i > /dev/null 2>&1
        echo "$(date '+%Y-%m-%d %H:%M:%S') gap=${GAP}s last=$(date -d @${LAST} '+%m-%d %H:%M:%S') dropbox restarted" >> "$LOGFILE"
    fi
fi

# if文の分岐に関わらず、cronが実行されるたびに必ず上書きする
FINISH_TIME=$(date +%s)
echo "$FINISH_TIME" > "$HEARTBEAT_FILE"
