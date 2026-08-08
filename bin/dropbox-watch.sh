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
# --- 2026-08-09 修正 ---
# 問題: サスペンド復帰直後のrestart処理中に古いNOW値でheartbeatを
#       書き込んでいたため、直後の実行が古いheartbeatを読んで誤検知し、
#       起動直後のdropboxを再度pkillしてしまう事象が発生していた
#       (21:49:31の再起動から52秒後の21:50:23に再度gap9458s誤検知)。
# 対策1: heartbeatは処理完了直前に再取得した時刻で書き込む(NOW使い回し廃止)
# 対策2: 直近RESTART_COOLDOWN秒以内に再起動済みなら、再度gap超過を
#        検知してもpkillは行わずスキップする(連続kill防止)
#
# Author: Minoru Yamada (aodamo)
# Created: 2026-08-07
# Updated: 2026-08-09
#
LOCKFILE="/tmp/dropbox-watch.lock"
exec 9>"$LOCKFILE"
flock -n 9 || exit 0

HOME_DIR=$(eval echo "~$USER")
HEARTBEAT_FILE="$HOME_DIR/.cache/dropbox-watch.heartbeat"
LAST_RESTART_FILE="$HOME_DIR/.cache/dropbox-watch.last_restart"
LOGFILE="/tmp/dropbox-watch.log"
GAP_THRESHOLD=180
RESTART_COOLDOWN=300   # 直近の再起動からこの秒数以内は再pkillしない

export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"

NOW=$(date +%s)

if [ -f "$HEARTBEAT_FILE" ]; then
    LAST=$(cat "$HEARTBEAT_FILE")
    GAP=$(( NOW - LAST ))
    if [ "$GAP" -ge "$GAP_THRESHOLD" ]; then

        # 直近で再起動済みならクールダウン期間内としてスキップ
        LAST_RESTART=0
        [ -f "$LAST_RESTART_FILE" ] && LAST_RESTART=$(cat "$LAST_RESTART_FILE")
        SINCE_RESTART=$(( NOW - LAST_RESTART ))

        if [ "$SINCE_RESTART" -lt "$RESTART_COOLDOWN" ]; then
            echo "$(date '+%Y-%m-%d %H:%M:%S') gap ${GAP}s detected (LAST=${LAST}) but skipped: restarted ${SINCE_RESTART}s ago (< ${RESTART_COOLDOWN}s cooldown)" >> "$LOGFILE"
        else
            sleep 15
            pkill -x dropbox
            sleep 3
            dropbox start -i > /dev/null 2>&1
            echo "$(date '+%Y-%m-%d %H:%M:%S') gap ${GAP}s detected (LAST=${LAST}, suspend likely), dropbox restarted" >> "$LOGFILE"
            date +%s > "$LAST_RESTART_FILE"
        fi
    fi
fi

# heartbeatは処理完了直前の時刻で書き込む（NOWの使い回しはしない）
date +%s > "$HEARTBEAT_FILE"
