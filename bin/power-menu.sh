#!/bin/bash
# power-menu.sh
#
# 全角/半角キーで起動するemacs/tmuxプロセス管理＋電源メニュー
# xfce keyboard shortcut: gnome-terminal --window -- bash -c "power-menu.sh"
#
# x250: SLEEP/POWEROFF/REBOOT
# P1  : 上記 + VE/XSRV BACKUP/NIGHT SUSPEND/PEEK ENV_GPG/SSH XSRV/SSH GH/docker
# Enter: emacsプロセスをkill / ESC: キャンセル

# ═══════════════ 環境設定 ═══════════════
XSRV_STOP="$HOME/.xsrv-backup-stop"
NIGHT_SUSPEND_TIMER="night-suspend.timer"
MAIN_HOSTNAME="P1"
EMACS_ELC_DIRS=("$HOME/.emacs.d/elisp" "$HOME/.emacs.d/inits")
HOME_ROOT="/home/minorugh/"
GH_ROOT="${HOME_ROOT}gospel-haiku.com/public_html/"

[[ -f "$HOME/.keychain/${HOSTNAME}-sh" ]] && source "$HOME/.keychain/${HOSTNAME}-sh"

# ═══════════════ P1/x250 共通設定 ═══════════════
# ここは両ホストで全く同じコードが実行される部分。
# action_*関数はP1でしか呼ばれないものも含めて全部ここで定義しておくだけで、
# x250側では「メニューに出てこない＝呼ばれない」だけなので害はない。
EMACS_LINES=$(ps -u "$USER" -o pid,stat,time,command \
                  | grep -E "emacs-start|/usr/(local/)?bin/emacs" \
                  | grep -v -e "<defunct>" -e "grep" -e "emacs-kill" -e "mozc" -e "cmigemo")
EMACS_COUNT=$([[ -z "$EMACS_LINES" ]] && echo 0 || wc -l <<< "$EMACS_LINES")

action_sleep()   { xset dpms force off; kill "$PPID"; }
action_poweroff(){ systemctl poweroff; }
action_reboot()  { systemctl reboot; }
action_ve() {
    echo "Removing Emacs .elc files under elisp/ and inits/..."
    find -L "${EMACS_ELC_DIRS[@]}" -name "*.elc" -print -delete
    vim ~/.emacs.d/
    kill "$PPID"
}
action_xsrv_backup() {
    if [[ -f "$XSRV_STOP" ]]; then
        rm -f "$XSRV_STOP"; echo "xsrv-backup: started."
    else
        touch "$XSRV_STOP"; echo "xsrv-backup: stopped."
    fi
    sleep 1; kill "$PPID"
}
action_night_suspend() {
    if systemctl --user is-active --quiet "$NIGHT_SUSPEND_TIMER"; then
        systemctl --user disable --now "$NIGHT_SUSPEND_TIMER"
        echo "night-suspend: stopped."
    else
        systemctl --user enable --now "$NIGHT_SUSPEND_TIMER"
        echo "night-suspend: started."
    fi
    sleep 1; kill "$PPID"
}
action_peek_env() { ~/.env_source/check-backup.sh; kill "$PPID"; }
action_ssh_xsrv() { exec ssh -t xsrv "cd '$HOME_ROOT' && exec \$SHELL -il"; }
action_ssh_gh()   { exec ssh -t xsrv "cd '$GH_ROOT' && printf '%s\n' '${GH_ROOT%public_html/}' && exec \$SHELL -il"; }
action_docker()   { exec docker exec -it httpd /bin/bash; }

status_badge() { [[ "$1" == run ]] && echo $'\e[32m[RUNNING]\e[0m' || echo $'\e[31m[STOPPED]\e[0m'; }
[[ -f "$XSRV_STOP" ]] && BACKUP_STATUS=$(status_badge stop) || BACKUP_STATUS=$(status_badge run)
systemctl --user is-active --quiet "$NIGHT_SUSPEND_TIMER" \
    && NIGHT_STATUS=$(status_badge run) || NIGHT_STATUS=$(status_badge stop)

# 共通の基本3項目（x250はここまで）
LABELS=("SLEEP" "POWEROFF" "REBOOT")
ACTIONS=(action_sleep action_poweroff action_reboot)

# ═══════════════ P1 専用設定 ═══════════════
# 分岐するのはここだけ。x250はこのif自体がスキップされ、
# LABELS/ACTIONSは上の3項目のまま次のセクションへ進む。
if [[ "$HOSTNAME" == "$MAIN_HOSTNAME" ]]; then
    # P1だけ、ここから7項目を追加
    LABELS+=(
        "VE"
        "XSRV BACKUP $BACKUP_STATUS"
        "NIGHT SUSPEND $NIGHT_STATUS"
        "PEEK ENV_GPG"
        "SSH XSRV"
        "SSH GH"
        "SH docker/httpd"
    )
    ACTIONS+=(action_ve action_xsrv_backup action_night_suspend action_peek_env action_ssh_xsrv action_ssh_gh action_docker)
fi

# ═══════════════ P1/x250 共通設定（メニュー構築〜実行） ═══════════════
# ここから先も両ホスト共通。LABELS/ACTIONSの中身が3個か10個かの違いだけで
# コードの分岐は一切ない。
MENU_TEXT=""
[[ -n "$EMACS_LINES" ]] && MENU_TEXT="$EMACS_LINES"$'\n'

BIND_ARGS=()
for i in "${!LABELS[@]}"; do
    DIGIT=$(( (i + 1) % 10 ))
    POS=$(( EMACS_COUNT + i + 1 ))
    MENU_TEXT+="${DIGIT}. ${LABELS[$i]}"$'\n'
    BIND_ARGS+=(--bind "${DIGIT}:pos(${POS})+accept")
done

CHOICE=$(printf '%s' "$MENU_TEXT" | fzf --ansi --reverse --color='pointer:white' "${BIND_ARGS[@]}")

if [[ -z "$CHOICE" ]]; then
    echo "Cancelled."
    kill "$PPID"
elif [[ "$CHOICE" =~ ^([0-9])\.\  ]]; then
    DIGIT="${BASH_REMATCH[1]}"
    IDX=$(( (DIGIT == 0 ? 10 : DIGIT) - 1 ))
    "${ACTIONS[$IDX]}"
else
    PID=$(awk '{print $1}' <<< "$CHOICE")
    if [[ "$PID" =~ ^[0-9]+$ ]]; then
        kill -9 "$PID" && echo "Killed $PID"
    fi
    kill "$PPID"
fi
