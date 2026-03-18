#!/bin/bash
#######################################################################
## Execute nightly makefile for backup
## Updated 2026.3.18
##
## cron: 50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
######################################################################

HOME=/home/minoru
LOG_PREFIX="[autobackup]"
LOGFILE="/tmp/cron.log"
TMPLOG=$(mktemp)

# SSHエージェントの設定を読み込む
if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

START=$(date '+%Y-%m-%d %H:%M:%S')

# makefile を実行（出力は一時ファイルへ）
make -f "$HOME/Dropbox/makefile" >> "$TMPLOG" 2>&1
STATUS=$?

if [ $STATUS -eq 0 ]; then
    echo "${LOG_PREFIX} OK: ${START} → $(date '+%Y-%m-%d %H:%M:%S')" >> "$LOGFILE"
else
    echo "${LOG_PREFIX} ERROR: ${START} → $(date '+%Y-%m-%d %H:%M:%S') (exit=${STATUS})" >> "$LOGFILE"
    cat "$TMPLOG" >> "$LOGFILE"
fi

rm -f "$TMPLOG"
