#!/bin/bash
#######################################################################
## neomutt.sh
## NeoMutt 起動スクリプト
##
## - mail セッションが無ければ作成
## - 既に表示中なら何もしない
## - 非表示なら attach
##
#######################################################################

# 既に表示中なら終了
if tmux list-clients -t mail 2>/dev/null | grep -q .; then
    exit 0
fi

# mail セッションが無ければ作成
if ! tmux has-session -t mail 2>/dev/null; then
    cd ~/Downloads || exit 1
    tmux new-session -d -s mail 'neomutt'
    tmux set -t mail status off
fi

# 表示
xfce4-terminal --maximize --title="NeoMutt Mail" \
    -e "tmux attach -t mail"

exit 0

