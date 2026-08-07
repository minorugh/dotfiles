#!/bin/bash
HOME="/home/minoru"
HEARTBEAT_FILE="$HOME/.cache/dropbox-watch.heartbeat"
LOGFILE="$HOME/.cache/dropbox-watch.log"
GAP_THRESHOLD=180

NOW=$(date +%s)

if [ -f "$HEARTBEAT_FILE" ]; then
    LAST=$(cat "$HEARTBEAT_FILE")
    GAP=$(( NOW - LAST ))
    if [ "$GAP" -ge "$GAP_THRESHOLD" ]; then
        sleep 15
        pkill -x dropbox
        sleep 3
        DISPLAY=:0 DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus dropbox start -i > /dev/null 2>&1
        echo "$(date '+%Y-%m-%d %H:%M:%S') gap ${GAP}s detected (suspend likely), dropbox restarted" >> "$LOGFILE"
    fi
fi

echo "$NOW" > "$HEARTBEAT_FILE"
