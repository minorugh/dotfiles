#!/bin/bash
#
# dropbox-resume-watch.sh
#
# 判定方法: systemd-logindが発する PrepareForSleep シグナルをD-Bus経由で直接購読する。
# サスペンド復帰時（boolean false）を検知した瞬間にDropboxを再起動する。
# GAP計算によるポーリング推測ではなく、OSからの確定イベントに基づくため
# サスペンド時間の長短に関わらず確実に検知できる。
#
# 実行方法: systemd --user サービスとして常駐させる（sudo不要）
#
# Author: Minoru Yamada (aodamo)
# Created: 2026-08-10
#
LOCKFILE="/tmp/dropbox-resume-watch.lock"
exec 9>"$LOCKFILE"
flock -n 9 || exit 0

LOGFILE="$HOME/.cache/dropbox-watch.log"

export DISPLAY=:0
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"

restart_dropbox() {
    sleep 10
    pkill -x dropbox
    sleep 3
    dropbox start > /dev/null 2>&1 &
    echo "$(date '+%Y-%m-%d %H:%M:%S') dropbox restarted (resume detected via dbus)" >> "$LOGFILE"
}

dbus-monitor --system "type='signal',interface='org.freedesktop.login1.Manager',member='PrepareForSleep'" |
while read -r line; do
    if echo "$line" | grep -q "boolean false"; then
        restart_dropbox
    fi
done

if [ -f "$LOGFILE" ] && [ "$(wc -l < "$LOGFILE")" -gt 1000 ]; then
    tail -n 200 "$LOGFILE" > "${LOGFILE}.tmp" && mv "${LOGFILE}.tmp" "$LOGFILE"
fi
