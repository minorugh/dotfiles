#!/bin/bash
# gitea-backup.sh
# Gitea data を Dropbox へ rsync バックアップ（停止なし）
# DB は SQLite。git リポジトリ本体が無事なら再設定可能なため停止不要と判断
# cron: autobackup.sh 経由で実行

LOCAL_DIR="${HOME}/Docker/gitea/data"
BACKUP_DIR="${HOME}/Dropbox/backup/gitea"

mkdir -p "${BACKUP_DIR}"

rsync -av --delete "${LOCAL_DIR}/" "${BACKUP_DIR}/"
if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') rsync done: gitea"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: rsync failed: gitea"
    exit 1
fi
