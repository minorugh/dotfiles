#!/bin/bash
# autobackup.sh
# 毎晩23:50にmakefileを実行するラッパースクリプト（タイムスタンプ付きログ）

# ログファイル
LOGFILE="/tmp/autobackup.log"

# 現在時刻を取得
NOW=$(date '+%Y-%m-%d %H:%M:%S')

# SSHエージェントの設定を読み込む
if [ -f "/home/minoru/.keychain/$(hostname)-sh" ]; then
    . /home/minoru/.keychain/$(hostname)-sh 2>/dev/null
fi

# 実行開始ログ
echo "[$NOW] autobackup.sh start" >> "$LOGFILE"

# makefile を実行してログに追記
make -f /home/minoru/Dropbox/makefile >> "$LOGFILE" 2>&1

# 実行終了ログ
NOW_END=$(date '+%Y-%m-%d %H:%M:%S')
echo "[$NOW_END] autobackup.sh end" >> "$LOGFILE"
