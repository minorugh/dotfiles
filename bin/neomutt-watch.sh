#!/bin/bash
#######################################################################
## neomutt-watch.sh
## neomutt (tmux セッション) の死活監視スクリプト
##
## cron から呼ばれ、tmux セッションが死んでいれば再作成する。
## 再起動した場合のみ /tmp/cron.log に日時を記録する。
##
## cron 設定：
##   */5 5,6,7 * * * /usr/local/bin/neomutt-watch.sh >> /tmp/cron.log 2>&1
##
#######################################################################
if ! tmux has-session -t mail 2>/dev/null; then
    cd ~/Downloads && tmux new-session -d -s mail 'neomutt'
    tmux set -t mail status off
    echo "$(date '+%Y-%m-%d %H:%M:%S'): neomutt restarted" >> /tmp/cron.log
fi
