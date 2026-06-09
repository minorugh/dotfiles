#!/bin/bash
# power-menu.sh
#
# 全角/半角キーで起動するemacs/tmuxプロセス管理＋電源メニュー
# xfce keyboard shortcut: gnome-terminal --window -- bash -c "power-menu.sh"
#
# 操作:
#   1 キー : SLEEP（画面オフのみ・プロセス継続、任意キーで復帰）
#   2 キー : POWEROFF
#   3 キー : REBOOT
#   4 キー : NEOMUTT RESTART
#   5 キー : KILL TMUX
#   Enter  : emacsプロセスをkill
#   ESC    : キャンセル

EMACS_LINES=$(ps -u $USER -o pid,stat,time,command \
    | grep -i emacs | grep -v -e "<defunct>" -e "grep" -e "emacs-kill")
EMACS_COUNT=$(echo "$EMACS_LINES" | grep -c . 2>/dev/null || echo 0)

# メニュー行数: emacs行の後に固定メニューが続く
# pos() は 1-based。メニュー先頭 = EMACS_COUNT+1
POS1=$((EMACS_COUNT + 1))
POS2=$((EMACS_COUNT + 2))
POS3=$((EMACS_COUNT + 3))
POS4=$((EMACS_COUNT + 4))

CHOICE=$( (
    [[ -n "$EMACS_LINES" ]] && echo "$EMACS_LINES"
    echo "1. SLEEP"
    echo "2. POWEROFF"
    echo "3. REBOOT"
    echo "4. MUTT RESTART"
) | fzf --reverse --no-input --color='pointer:white' \
    --bind "1:pos(${POS1})+accept" \
    --bind "2:pos(${POS2})+accept" \
    --bind "3:pos(${POS3})+accept" \
    --bind "4:pos(${POS4})+accept" \
    --bind "5:pos(${POS5})+accept")

case "$CHOICE" in
    1.*) xset dpms force off; kill $PPID ;;
    2.*) systemctl poweroff ;;
    3.*) systemctl reboot ;;
    4.*) tmux kill-session -t mail 2>/dev/null; setsid neomutt.sh; kill $PPID ;;
    "")  echo "Cancelled."; kill $PPID ;;
    *)
        PID=$(echo "$CHOICE" | awk '{print $1}')
        if [[ "$PID" =~ ^[0-9]+$ ]]; then
            kill -9 "$PID" && echo "Killed $PID"
        fi
        kill $PPID ;;
esac
