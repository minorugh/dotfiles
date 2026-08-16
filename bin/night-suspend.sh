#!/bin/bash

LOG="$HOME/.cache/night-suspend.log"

log() { echo "$(date '+%F %T') $*" >> "$LOG"; }

for pat in automerge.sh autobackup.sh xsrv-backup.sh; do
    if pgrep -f "$pat" > /dev/null; then
        log "SKIP: $pat 実行中のためサスペンドを見送り"
        exit 0
    fi
done

log "SUSPEND"

exec 3< <(dbus-monitor --system "type='signal',interface='org.freedesktop.login1.Manager',member='PrepareForSleep'")

systemctl suspend

while read -r -u 3 line; do
    if [[ "$line" == *"member=PrepareForSleep"* ]]; then
        read -r -u 3 arg
        [[ "$arg" == *"false"* ]] && break
    fi
done

exec 3<&-

log "RESUMED"
