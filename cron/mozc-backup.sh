#!/bin/bash
# mozc-backup.sh
# P1 の ~/.mozc を毎晩 Dropbox にバックアップする

BACKUP_DIR="${HOME}/Dropbox/backup/mozc"

mkdir -p "${BACKUP_DIR}"
rm -rf "${BACKUP_DIR}/.mozc"
cp -rf "${HOME}/.mozc" "${BACKUP_DIR}/.mozc"

echo "$(date '+%Y-%m-%d %H:%M:%S') mozc backup done"
