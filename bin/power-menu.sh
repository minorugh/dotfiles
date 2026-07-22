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
#   6 キー : VE（.elc削除 + ~/.emacs.d/ をVimで開く）
#   7 キー : EQ（.elc削除 + emacs -q ミニ構成で起動）
#   Enter  : emacsプロセスをkill
#   ESC    : キャンセル

XSRV_STOP="$HOME/.xsrv-backup-stop"

EMACS_LINES=$(ps -u $USER -o pid,stat,time,command \
		  | grep -E "emacs-start|/usr/(local/)?bin/emacs" \
		  | grep -v -e "<defunct>" -e "grep" -e "emacs-kill" -e "mozc" -e "cmigemo")

if [[ -z "$EMACS_LINES" ]]; then
    EMACS_COUNT=0
else
    EMACS_COUNT=$(echo "$EMACS_LINES" | wc -l)
fi

POS1=$((EMACS_COUNT + 1))
POS2=$((EMACS_COUNT + 2))
POS3=$((EMACS_COUNT + 3))
POS4=$((EMACS_COUNT + 4))
POS5=$((EMACS_COUNT + 5))
POS6=$((EMACS_COUNT + 6))
POS7=$((EMACS_COUNT + 7))

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
	    echo "6. VE (Emacs設定をVimで開く)"
	    echo "7. EQ (Emacsをミニ構成で起動)"
	) | fzf --ansi --reverse --no-input --color='pointer:white' \
		--bind "1:pos(${POS1})+accept" \
		--bind "2:pos(${POS2})+accept" \
		--bind "3:pos(${POS3})+accept" \
		--bind "4:pos(${POS4})+accept" \
		--bind "5:pos(${POS5})+accept" \
		--bind "6:pos(${POS6})+accept" \
		--bind "7:pos(${POS7})+accept" )


case "$CHOICE" in
    1.*) xset dpms force off; kill $PPID ;;
    2.*) systemctl poweroff ;;
    3.*) systemctl reboot ;;
    4.*) touch "$XSRV_STOP"; echo "xsrv-backup: stopped."; sleep 1; kill $PPID ;;
    5.*) rm -f "$XSRV_STOP"; echo "xsrv-backup: started."; sleep 1; kill $PPID ;;
    6.*)
        echo "Removing Emacs .elc files..."
        find -L ~/.emacs.d -name "*.elc" -print -delete
        vim ~/.emacs.d/
        kill $PPID ;;
    7.*)
        echo "Removing Emacs .elc files..."
        find -L ~/.emacs.d -name "*.elc" -print -delete
        emacs -q -l ~/.emacs.d/init-mini.el
        kill $PPID ;;
    "")  echo "Cancelled."; kill $PPID ;;
    *)
        PID=$(echo "$CHOICE" | awk '{print $1}')
        if [[ "$PID" =~ ^[0-9]+$ ]]; then
            kill -9 "$PID" && echo "Killed $PID"
        fi
        kill $PPID ;;
esac
