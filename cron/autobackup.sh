#!/bin/bash
#######################################################################
## Execute nightly makefile for backup
## Updated 2026.3.16
##
## cron: 50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
######################################################################

HOME=/home/minoru
LOG_PREFIX="[autobackup]"
LOGFILE="/tmp/cron.log"

# SSHエージェントの設定を読み込む
if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

echo "${LOG_PREFIX} 開始: $(date '+%Y-%m-%d %H:%M:%S')" >> "$LOGFILE"

# makefile を実行してログに追記
make -f "$HOME/Dropbox/makefile" >> "$LOGFILE" 2>&1

echo "${LOG_PREFIX} 完了: $(date '+%Y-%m-%d %H:%M:%S')" >> "$LOGFILE"
