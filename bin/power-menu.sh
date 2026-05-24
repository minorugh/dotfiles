#!/bin/bash
# power-menu.sh
#
# 全角/半角キーで起動するemacsプロセス管理＋電源メニュー
# xfce keyboard shortcut: gnome-terminal --window -- bash -c "power-menu.sh"
#
# 操作:
#   1 キー : SLEEP（サスペンド）
#   2 キー : POWEROFF
#   3 キー : REBOOT
#   Enter  : emacsプロセスをkill
#   ESC    : キャンセル

CHOICE=$( (
    ps -u $USER -o pid,stat,time,command | grep -i emacs | grep -v -e "<defunct>" -e "grep" -e "emacs-kill"
    echo "1. SLEEP"
    echo "2. POWEROFF"
    echo "3. REBOOT"
) | fzf --reverse --no-input --color='pointer:white' \
    --bind '1:pos(4)+accept' \
    --bind '2:pos(5)+accept' \
    --bind '3:pos(6)+accept')

case "$CHOICE" in
    1.*) xset dpms force off; kill $PPID ;;
    2.*) systemctl poweroff ;;
    3.*) systemctl reboot ;;
    "")  echo "Cancelled."; kill $PPID ;;
    *)
        PID=$(echo "$CHOICE" | awk '{print $1}')
        if [[ "$PID" =~ ^[0-9]+$ ]]; then
            kill -9 "$PID" && echo "Killed $PID"
        fi
        kill $PPID ;;
esac
