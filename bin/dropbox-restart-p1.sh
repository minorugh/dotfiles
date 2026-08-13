#!/bin/bash
#
# P1-dropbox-restart.sh
#
# メイン機(P1)専用。
# P1はサスペンドしない常時稼働運用のため dropbox-watch.pl の
# PrepareForSleep監視が発火しない。その代替として、cronから
# Dropboxを強制再起動し、dropbox-watch.pl と同じログ書式で
# ~/.cache/dropbox-watch.log に記録する。
#
# dropbox-watch.pl / dropbox-watch.service 側は無改造。
# ログを dropbox-watch-log (cron-Makefile) と共用するため書式を合わせている。
#
set -eu

LOGFILE="$HOME/.cache/dropbox-watch.log"

pkill -x dropbox || true
sleep 1
dropbox start
sleep 1

bytes=$(du -s -B1 "$HOME/Dropbox" | awk '{print $1}')
gb=$(awk "BEGIN{printf \"%.1f\", $bytes/1024**3}")

echo "$(date '+%Y-%m-%d %H:%M:%S') dropbox restarted (daily forced, main host, size ${gb}GB)" >> "$LOGFILE"
