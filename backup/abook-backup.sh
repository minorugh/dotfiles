#!/bin/bash
# abook-backup.sh
# P1 の ~/.abook/addressbook を毎晩 GPG 暗号化して Dropbox にバックアップする

BACKUP_DIR="${HOME}/Dropbox/backup/abook"
SRC="${HOME}/.abook/addressbook"
DATE=$(date '+%Y-%m-%d')

# GPG_KEY が未設定の場合は .env_local から読み込む
if [[ -z "${GPG_KEY:-}" ]]; then
    source "${HOME}/.env_source/.env_local" 2>/dev/null || true
fi

mkdir -p "${BACKUP_DIR}"

gpg --yes --batch -e -r "${GPG_KEY}" \
    -o "${BACKUP_DIR}/addressbook_${DATE}.gpg" \
    "${SRC}"

# 7日より古いものを削除
find "${BACKUP_DIR}" -name 'addressbook_*.gpg' -mtime +7 -delete

echo "$(date '+%Y-%m-%d %H:%M:%S') abook backup done"
