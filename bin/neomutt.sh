#!/bin/bash

tmux has-session -t mail 2>/dev/null

if [ $? -ne 0 ]; then
    cd ~/Downloads && tmux new-session -d -s mail 'neomutt'
    tmux set -t mail status off  # ここで直接設定
fi

# gnome-terminal --maximize --title="NeoMutt Mail" -- tmux attach -t mail
xfce4-terminal --maximize --title="NeoMutt Mail" -e "tmux attach -t mail"

exit

