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
#   4 キー : BACKUP toggle（STOP⇔START）
#   5 キー : VE（.elc削除 + ~/.emacs.d/ をVimで開く）
#   6 キー : EQ（.elc削除 + emacs -q ミニ構成で起動）
#   7 キー : XSRV-ROOT（xsrv home-root へ接続）
#   8 キー : GH（xsrv gospel-haiku.com へ接続）
#   9 キー : minorugh.com（xsrv minorugh.com へ接続）
#   0 キー : docker/httpd（コンテナへ接続）
#   Enter  : emacsプロセスをkill
#   ESC    : キャンセル

XSRV_STOP="$HOME/.xsrv-backup-stop"
EMACS_ELC_DIRS=("$HOME/.emacs.d/elisp" "$HOME/.emacs.d/inits")

# keychain(ssh-agent)を読み込む（.zshrcと同じ処理。bash -c起動のためここでも必要）
[[ -f "$HOME/.keychain/${HOSTNAME}-sh" ]] && source "$HOME/.keychain/${HOSTNAME}-sh"

# Remote (xsrv / Docker) ターゲット定義
HOME_ROOT="/home/minorugh/"
GH_ROOT="${HOME_ROOT}gospel-haiku.com/public_html/"
MN_ROOT="${HOME_ROOT}minorugh.com/public_html/"

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
POS8=$((EMACS_COUNT + 8))
POS9=$((EMACS_COUNT + 9))
POS0=$((EMACS_COUNT + 10))

# BACKUPのラベルに現在状態を表示（トグル対象）
if [[ -f "$XSRV_STOP" ]]; then
    BACKUP_STATUS=$'\e[31m[STOPPED]\e[0m'
else
    BACKUP_STATUS=$'\e[32m[RUNNING]\e[0m'
fi

CHOICE=$( (
	    [[ -n "$EMACS_LINES" ]] && echo "$EMACS_LINES"
	    echo "1. SLEEP"
	    echo "2. POWEROFF"
	    echo "3. REBOOT"
	    echo "4. BACKUP $BACKUP_STATUS"
	    echo "5. VE (Emacs設定をVimで開く)"
	    echo "6. EQ (Emacsをミニ構成で起動)"
	    echo "7. XSRV-ROOT"
	    echo "8. GH"
	    echo "9. minorugh.com"
	    echo "0. docker/httpd"
	) | fzf --ansi --reverse --color='pointer:white' \
		--bind "1:pos(${POS1})+accept" \
		--bind "2:pos(${POS2})+accept" \
		--bind "3:pos(${POS3})+accept" \
		--bind "4:pos(${POS4})+accept" \
		--bind "5:pos(${POS5})+accept" \
		--bind "6:pos(${POS6})+accept" \
		--bind "7:pos(${POS7})+accept" \
		--bind "8:pos(${POS8})+accept" \
		--bind "9:pos(${POS9})+accept" \
		--bind "0:pos(${POS0})+accept" )


case "$CHOICE" in
    1.*) xset dpms force off; kill $PPID ;;
    2.*) systemctl poweroff ;;
    3.*) systemctl reboot ;;
    4.*)
        if [[ -f "$XSRV_STOP" ]]; then
            rm -f "$XSRV_STOP"
            echo "xsrv-backup: started."
        else
            touch "$XSRV_STOP"
            echo "xsrv-backup: stopped."
        fi
        sleep 1; kill $PPID ;;
    5.*)
        echo "Removing Emacs .elc files under elisp/ and inits/..."
        find -L "${EMACS_ELC_DIRS[@]}" -name "*.elc" -print -delete
        vim ~/.emacs.d/
        kill $PPID ;;
    6.*)
        echo "Removing Emacs .elc files under elisp/ and inits/..."
        find -L "${EMACS_ELC_DIRS[@]}" -name "*.elc" -print -delete
        emacs -q -l ~/.emacs.d/init-mini.el
        kill $PPID ;;
    7.*)
        exec ssh -t xsrv "cd '$HOME_ROOT' && exec \$SHELL -il" ;;
    8.*)
        exec ssh -t xsrv "cd '$GH_ROOT' && printf '%s\n' '${GH_ROOT%public_html/}' && exec \$SHELL -il" ;;
    9.*)
        exec ssh -t xsrv "cd '$MN_ROOT' && printf '%s\n' '${MN_ROOT%public_html/}' && exec \$SHELL -il" ;;
    0.*)
        exec docker exec -it httpd /bin/bash ;;
    "")  echo "Cancelled."; kill $PPID ;;
    *)
        PID=$(echo "$CHOICE" | awk '{print $1}')
        if [[ "$PID" =~ ^[0-9]+$ ]]; then
            kill -9 "$PID" && echo "Killed $PID"
        fi
        kill $PPID ;;
esac
