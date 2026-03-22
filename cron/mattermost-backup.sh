#!/bin/bash
# mattermost-backup.sh
# Mattermost データを Dropbox へ rsync バックアップ
# DB は pg_dump で上書き保存（Dropbox がバージョン管理）
# cron: autobackup.sh 経由で実行

LOCAL_DIR="/home/minoru/Docker/mattermost"
BACKUP_DIR="${HOME}/Dropbox/backup/mattermost"

mkdir -p "${BACKUP_DIR}"

# data / config / logs / plugins を rsync で上書き同期
for dir in data config logs plugins; do
    rsync -av --delete "${LOCAL_DIR}/${dir}/" "${BACKUP_DIR}/${dir}/"
    if [ $? -eq 0 ]; then
        echo "$(date '+%Y-%m-%d %H:%M:%S') rsync done: ${dir}"
    else
        echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: rsync failed: ${dir}"
    fi
done

# pg_dump で DB を上書き保存
if ! docker ps --format '{{.Names}}' | grep -q '^mattermost-postgres$'; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: mattermost-postgres is not running"
    exit 1
fi

docker exec mattermost-postgres pg_dump -U mattermost mattermost \
    > "${BACKUP_DIR}/mattermost.sql"

if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') pg_dump done: mattermost.sql"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: pg_dump failed"
    exit 1
fi
