#!/bin/bash
# night-suspend.sh
# 深夜サスペンド（復帰は手動）

LOG="$HOME/.cache/night-suspend.log"

log() { echo "$(date '+%F %T') $*" >> "$LOG"; }

for pat in automerge.sh autobackup.sh xsrv-backup.sh; do
    if pgrep -f "$pat" > /dev/null; then
        log "SKIP: $pat 実行中のためサスペンドを見送り"
        exit 0
    fi
done

log "SUSPEND"
systemctl suspend
log "RESUMED at $(date '+%F %T')"
