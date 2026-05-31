#!/bin/bash

if ! tmux has-session -t mail 2>/dev/null; then
    cd ~/Downloads && tmux new-session -d -s mail 'neomutt'
    tmux set -t mail status off
fi

xfce4-terminal --maximize --title="NeoMutt Mail" -e "tmux attach -t mail"

exit
