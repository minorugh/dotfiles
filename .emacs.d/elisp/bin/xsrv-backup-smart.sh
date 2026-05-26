#!/bin/bash
#######################################################################
## xsrv-backup-smart.sh
## xserver → ローカルへ rsync のみ（変更なければスキップ）
## git push は systemd timer の xsrv-backup.sh が担当
##
## ~/.emacs.d/elisp/bin/ に置いて Emacs から呼ぶ専用スクリプト
#######################################################################

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"

XSRV_HOST="minorugh@sv13268.xserver.jp"
XSRV_SSH="ssh -p 10022"

XSRV_GH_SRC="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html/"
XSRV_GH_DST="$HOME/src/github.com/minorugh/xsrv-GH"

XSRV_MN_SRC="$XSRV_HOST:/home/minorugh/minorugh.com/public_html/"
XSRV_MN_DST="$HOME/src/github.com/minorugh/xsrv-minorugh"

TMPLOG=$(mktemp)
ERRORS=0

# SSHエージェントの設定を読み込む
if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() {
    echo "${LOG_PREFIX} $1"
}

run_rsync() {
    local label="$1"
    local src="$2"
    local dst="$3"

    rsync -avz --delete --exclude='.git' --exclude='.gitignore' \
          -e "$XSRV_SSH" "$src" "$dst/" > "$TMPLOG" 2>&1

    if [ $? -ne 0 ]; then
        log "rsync ${label}: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
        > "$TMPLOG"
        return
    fi

    local transferred=$(grep -c "^[<>ch]" "$TMPLOG" 2>/dev/null | tr -d '[:space:]')
    > "$TMPLOG"

    if [ "$transferred" -eq 0 ]; then
        log "rsync ${label}: NO CHANGES"
    else
        log "rsync ${label}: OK (${transferred} files)"
    fi
}

START=$(date '+%Y-%m-%d %H:%M:%S')
log "START: ${START}"

run_rsync "gospel-haiku.com" "$XSRV_GH_SRC" "$XSRV_GH_DST"
run_rsync "minorugh.com"     "$XSRV_MN_SRC" "$XSRV_MN_DST"

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
