#!/bin/bash
STATE_FILE="$HOME/.cache/dropbox-watch.state"
LOGFILE="$HOME/.cache/dropbox-watch.log"
THRESHOLD=120

restart_dropbox() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') $1, [DRY-RUN] would restart here" >> "$LOGFILE"
    rm -f "$STATE_FILE"
}

# restart_dropbox() {
#     echo "$(date '+%Y-%m-%d %H:%M:%S') $1, restarting" >> "$LOGFILE"
#     pkill -x dropbox
#     sleep 3
#     dropbox start -i > /dev/null 2>&1
#     rm -f "$STATE_FILE"
# }

STATUS=$(dropbox status 2>/dev/null | tail -1)

if [ "$STATUS" = "最新の状態" ]; then
    rm -f "$STATE_FILE"
    exit 0
fi

if [ "$1" = "--now" ]; then
    restart_dropbox "manual check: status=$STATUS"
    exit 0
fi

NOW=$(date +%s)

if [ -f "$STATE_FILE" ]; then
    FIRST_SEEN=$(cat "$STATE_FILE")
    ELAPSED=$(( NOW - FIRST_SEEN ))
    if [ "$ELAPSED" -ge "$THRESHOLD" ]; then
        restart_dropbox "stuck ${ELAPSED}s status=$STATUS"
    fi
else
    echo "$NOW" > "$STATE_FILE"
fi
