#!/bin/bash
set -e

# ===== 設定 =====
BASE="$HOME/Dropbox/backup/thunderbird"
SRC="$HOME/.thunderbird/"
DST="$BASE/.thunderbird/"

# Thunderbirdを終了させる
# まずは通常の終了を試み、ダメなら強制終了(SIGKILL)
if pgrep -x "thunderbird" > /dev/null; then
    echo "[Thunderbird] Stopping Thunderbird..."
    pkill -x thunderbird
    
    # 最大10秒間、正常終了を待機
    for i in {1..10}; do
        if ! pgrep -x "thunderbird" > /dev/null; then
            break
        fi
        sleep 1
    done

    # まだプロセスが残っている場合は強制終了して次へ進む
    if pgrep -x "thunderbird" > /dev/null; then
        pkill -9 -x thunderbird
        sleep 1
    fi
fi

echo "[Thunderbird] Start backup"

# ===== Thunderbird本体 =====
rsync -av --delete \
      --exclude='*.lock' \
      --exclude='lock' \
      "$SRC" "$DST"

echo "[Thunderbird] DONE"
