#!/bin/bash
# mattermost-backup.sh
# Mattermost の config+data tar.gz + pg_dump を毎晩 Dropbox にバックアップ
# 7世代保持、古いものは自動削除
# cron: autobackup.sh 経由で実行

BACKUP_DIR="${HOME}/Dropbox/backup/mattermost"
DATE=$(date +%Y%m%d)
KEEP=7

mkdir -p "${BACKUP_DIR}"

# config + data アーカイブ（logs, plugins, db は除外）
sudo tar czf "${BACKUP_DIR}/${DATE}.tar.gz" \
     -C "${BACKUP_DIR}" config data
echo "$(date '+%Y-%m-%d %H:%M:%S') tar backup done: ${DATE}.tar.gz"

# pg_dump
if ! docker ps --format '{{.Names}}' | grep -q '^mattermost-postgres$'; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: mattermost-postgres is not running"
    exit 1
fi

docker exec mattermost-postgres pg_dump -U mattermost mattermost \
       > "${BACKUP_DIR}/mattermost_${DATE}.sql"

if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') pg_dump done: mattermost_${DATE}.sql"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: pg_dump failed"
    exit 1
fi

# 7世代より古いファイルを削除
ls -t "${BACKUP_DIR}"/*.tar.gz 2>/dev/null | tail -n +$((KEEP + 1)) | xargs rm -f
ls -t "${BACKUP_DIR}"/*.sql   2>/dev/null | tail -n +$((KEEP + 1)) | xargs rm -f
