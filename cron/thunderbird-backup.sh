#!/bin/bash
# thunderbird-backup.sh

BASE="$HOME/Dropbox/backup/thunderbird"
SRC="$HOME/.thunderbird/"
DST="$BASE/profile/"

mkdir -p "$DST"

if pgrep -x "thunderbird" > /dev/null; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') [Thunderbird] Stopping..."
    pkill -x thunderbird || true

    for i in {1..10}; do
        if ! pgrep -x "thunderbird" > /dev/null; then break; fi
        sleep 1
    done

    if pgrep -x "thunderbird" > /dev/null; then
        pkill -9 -x thunderbird || true
        sleep 1
    fi
fi

echo "$(date '+%Y-%m-%d %H:%M:%S') [Thunderbird] Start backup"

rsync -av --delete \
      --exclude='*.lock' \
      --exclude='lock' \
      "$SRC" "$DST"

echo "$(date '+%Y-%m-%d %H:%M:%S') [Thunderbird] DONE"
