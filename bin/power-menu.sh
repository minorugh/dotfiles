#!/bin/bash
# power-menu.sh
#
# 全角/半角キーで起動するemacs/tmuxプロセス管理＋電源メニュー
# xfce keyboard shortcut: gnome-terminal --window -- bash -c "power-menu.sh"
#
# メニュー項目はホストごとに異なる（P1: 全10項目 / それ以外: 1〜4のみ）
# Enter  : emacsプロセスをkill
# ESC    : キャンセル

XSRV_STOP="$HOME/.xsrv-backup-stop"
NIGHT_SUSPEND_TIMER="night-suspend.timer"
MAIN_HOSTNAME="P1"
EMACS_ELC_DIRS=("$HOME/.emacs.d/elisp" "$HOME/.emacs.d/inits")

[[ -f "$HOME/.keychain/${HOSTNAME}-sh" ]] && source "$HOME/.keychain/${HOSTNAME}-sh"

HOME_ROOT="/home/minorugh/"
GH_ROOT="${HOME_ROOT}gospel-haiku.com/public_html/"

EMACS_LINES=$(ps -u $USER -o pid,stat,time,command \
                  | grep -E "emacs-start|/usr/(local/)?bin/emacs" \
                  | grep -v -e "<defunct>" -e "grep" -e "emacs-kill" -e "mozc" -e "cmigemo")

if [[ -z "$EMACS_LINES" ]]; then
    EMACS_COUNT=0
else
    EMACS_COUNT=$(echo "$EMACS_LINES" | wc -l)
fi

if [[ -f "$XSRV_STOP" ]]; then
    BACKUP_STATUS=$'\e[31m[STOPPED]\e[0m'
else
    BACKUP_STATUS=$'\e[32m[RUNNING]\e[0m'
fi

if systemctl --user is-active --quiet "$NIGHT_SUSPEND_TIMER"; then
    NIGHT_STATUS=$'\e[32m[RUNNING]\e[0m'
else
    NIGHT_STATUS=$'\e[31m[STOPPED]\e[0m'
fi

if [[ "$HOSTNAME" == "$MAIN_HOSTNAME" ]]; then
    LABELS=(
        "SLEEP"
        "POWEROFF"
        "REBOOT"
        "VE"
        "XSRV BACKUP $BACKUP_STATUS"
        "NIGHT SUSPEND $NIGHT_STATUS"
        "PEEK ENV_GPG"
        "SSH XSRV"
        "SSH GH"
        "SH docker/httpd"
    )
    ACTIONS=(sleep poweroff reboot ve xsrv_backup night_suspend peek_env ssh_xsrv ssh_gh docker)
else
    LABELS=("SLEEP" "POWEROFF" "REBOOT" "VE")
    ACTIONS=(sleep poweroff reboot ve)
fi

MENU_LINES=()
BINDS=()
for i in "${!LABELS[@]}"; do
    n=$(( (i + 1) % 10 ))
    MENU_LINES+=("${n}. ${LABELS[$i]}")
    POS=$(( EMACS_COUNT + i + 1 ))
    BINDS+=(--bind "${n}:pos(${POS})+accept")
done

CHOICE=$( (
            [[ -n "$EMACS_LINES" ]] && echo "$EMACS_LINES"
            printf '%s\n' "${MENU_LINES[@]}"
        ) | fzf --ansi --reverse --color='pointer:white' "${BINDS[@]}" )

ACTION=""
if [[ "$CHOICE" =~ ^([0-9])\.\  ]]; then
    KEY="${BASH_REMATCH[1]}"
    [[ "$KEY" == "0" ]] && idx=9 || idx=$((KEY - 1))
    ACTION="${ACTIONS[$idx]}"
fi

case "$ACTION" in
    sleep) xset dpms force off; kill $PPID ;;
    poweroff) systemctl poweroff ;;
    reboot) systemctl reboot ;;
    ve)
        echo "Removing Emacs .elc files under elisp/ and inits/..."
        find -L "${EMACS_ELC_DIRS[@]}" -name "*.elc" -print -delete
        vim ~/.emacs.d/
        kill $PPID ;;
    xsrv_backup)
        if [[ -f "$XSRV_STOP" ]]; then
            rm -f "$XSRV_STOP"
            echo "xsrv-backup: started."
        else
            touch "$XSRV_STOP"
            echo "xsrv-backup: stopped."
        fi
        sleep 1
        kill $PPID ;;
    night_suspend)
        if systemctl --user is-active --quiet "$NIGHT_SUSPEND_TIMER"; then
            systemctl --user stop "$NIGHT_SUSPEND_TIMER"
            systemctl --user disable "$NIGHT_SUSPEND_TIMER"
            echo "night-suspend: stopped."
        else
            systemctl --user enable --now "$NIGHT_SUSPEND_TIMER"
            echo "night-suspend: started."
        fi
        sleep 1
        kill $PPID ;;
    peek_env)
        ~/.env_source/check-backup.sh
        kill $PPID ;;
    ssh_xsrv)
        exec ssh -t xsrv "cd '$HOME_ROOT' && exec \$SHELL -il" ;;
    ssh_gh)
        exec ssh -t xsrv "cd '$GH_ROOT' && printf '%s\n' '${GH_ROOT%public_html/}' && exec \$SHELL -il" ;;
    docker)
        exec docker exec -it httpd /bin/bash ;;
    "")
        if [[ -z "$CHOICE" ]]; then
            echo "Cancelled."
            kill $PPID
        else
            PID=$(echo "$CHOICE" | awk '{print $1}')
            if [[ "$PID" =~ ^[0-9]+$ ]]; then
                kill -9 "$PID" && echo "Killed $PID"
            fi
            kill $PPID
        fi
        ;;
esac
