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
#   4 キー : BACKUP STOP
#   5 キー : BACKUP START
#   Enter  : emacsプロセスをkill
#   ESC    : キャンセル

XSRV_STOP="$HOME/.xsrv-backup-stop"

EMACS_LINES=$(ps -u $USER -o pid,stat,time,command \
		  | grep -E "emacs-start|/usr/(local/)?bin/emacs" \
		  | grep -v -e "<defunct>" -e "grep" -e "emacs-kill" -e "mozc" -e "cmigemo")

EMACS_COUNT=$(echo "$EMACS_LINES" | grep -c . 2>/dev/null || echo 0)

POS1=$((EMACS_COUNT + 1))
POS2=$((EMACS_COUNT + 2))
POS3=$((EMACS_COUNT + 3))
POS4=$((EMACS_COUNT + 4))
POS5=$((EMACS_COUNT + 5))

# stop/start のラベルに現在状態を表示
if [[ -f "$XSRV_STOP" ]]; then
    BACKUP_STATUS=$'\e[31m[STOPPED]\e[0m'
else
    BACKUP_STATUS="[RUNNING]"
fi

CHOICE=$( (
	    [[ -n "$EMACS_LINES" ]] && echo "$EMACS_LINES"
	    echo "1. SLEEP"
	    echo "2. POWEROFF"
	    echo "3. REBOOT"
	    echo "4. BACKUP STOP  $BACKUP_STATUS"
	    echo "5. BACKUP START $BACKUP_STATUS"
	) | fzf --ansi --reverse --color='pointer:white' \
		--bind "1:pos(${POS1})+accept" \
		--bind "2:pos(${POS2})+accept" \
		--bind "3:pos(${POS3})+accept" \
		--bind "4:pos(${POS4})+accept" \
		--bind "5:pos(${POS5})+accept" )


case "$CHOICE" in
    1.*) xset dpms force off; kill $PPID ;;
    2.*) systemctl poweroff ;;
    3.*) systemctl reboot ;;
    4.*) touch "$XSRV_STOP"; echo "xsrv-backup: stopped."; sleep 1; kill $PPID ;;
    5.*) rm -f "$XSRV_STOP"; echo "xsrv-backup: started."; sleep 1; kill $PPID ;;
    "")  echo "Cancelled."; kill $PPID ;;
    *)
        PID=$(echo "$CHOICE" | awk '{print $1}')
        if [[ "$PID" =~ ^[0-9]+$ ]]; then
            kill -9 "$PID" && echo "Killed $PID"
        fi
        kill $PPID ;;
esac
