#!/bin/bash

# 'mail' という名前の tmux セッションがあるか確認
tmux has-session -t mail 2>/dev/null

if [ $? -ne 0 ]; then
    # セッションがなければ、~/Downloads に移動してから裏で NeoMutt を起動
    cd ~/Downloads && tmux new-session -d -s mail 'neomutt'
fi

# 余計なオプションを全削除。元の確実な起動コマンドに戻す
gnome-terminal --maximize --title="NeoMutt Mail" -- tmux attach -t mail

exit
