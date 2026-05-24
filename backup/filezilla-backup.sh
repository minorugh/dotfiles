#!/bin/bash
# filezilla-backup.sh
# FileZilla 設定を Dropbox へ rsync バックアップ
# cron: autobackup.sh 経由で実行

SRC="${HOME}/.config/filezilla/"
BACKUP_DIR="${HOME}/Dropbox/backup/filezilla/config"

mkdir -p "${BACKUP_DIR}"

if pgrep -x "filezilla" > /dev/null; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') [FileZilla] Stopping..."
    pkill -x filezilla || true

    for i in {1..10}; do
        if ! pgrep -x "filezilla" > /dev/null; then break; fi
        sleep 1
    done

    if pgrep -x "filezilla" > /dev/null; then
        pkill -9 -x filezilla || true
        sleep 1
    fi
fi

rsync -av --delete "${SRC}" "${BACKUP_DIR}/"
if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') rsync done: filezilla"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: rsync failed: filezilla"
    exit 1
fi
