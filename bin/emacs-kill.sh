#!/bin/bash
# ゾンビを除外してEmacs関連プロセスを抽出
# fzfで選択して、選ばれたPIDを強制終了する
PID=$(ps -u $USER -o pid,stat,time,command | grep -i emacs | grep -v "<defunct>" | grep -v "grep" | fzf --reverse --header="[Kill Process] 選択してEnterで処刑" --multi | awk '{print $1}')

if [ -n "$PID" ]; then
    echo $PID | xargs kill -9
    echo "Done."
else
    echo "Cancelled."
fi
