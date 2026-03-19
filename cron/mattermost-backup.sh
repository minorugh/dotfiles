#!/bin/bash
# mattermost-backup.sh - Mattermost DBバックアップ（pg_dump → Dropbox）
# cron: 0 3 * * * /usr/local/bin/mattermost-backup.sh >> /tmp/cron.log 2>&1

BACKUP_DIR=~/Dropbox/docker-data/mattermost/backup
DATE=$(date +%Y%m%d_%H%M%S)
LOG="$BACKUP_DIR/backup.log"

mkdir -p "$BACKUP_DIR"

if ! docker ps --format '{{.Names}}' | grep -q '^mattermost-postgres$'; then
    echo "[mattermost-backup] ERROR: $(date '+%Y-%m-%d %H:%M:%S') (container not running)"
    echo "[$DATE] ERROR: mattermost-postgres is not running" >> "$LOG"
    exit 1
fi

docker exec mattermost-postgres pg_dump -U mattermost mattermost \
    > "$BACKUP_DIR/mattermost_${DATE}.sql" 2>> "$LOG"

if [ $? -eq 0 ]; then
    echo "[mattermost-backup] OK: $(date '+%Y-%m-%d %H:%M:%S')"
    echo "[$DATE] OK: mattermost_${DATE}.sql" >> "$LOG"
else
    echo "[mattermost-backup] ERROR: $(date '+%Y-%m-%d %H:%M:%S') (pg_dump failed)"
    echo "[$DATE] ERROR: pg_dump failed" >> "$LOG"
    exit 1
fi

# 7日以上古いバックアップを削除
find "$BACKUP_DIR" -name "*.sql" -mtime +7 -delete
