#!/bin/bash
#######################################################################
## xsrv-backup.sh
## xserver → ローカルへ rsync + git commit + push（2ドメイン）
##
## cron: 0 7-22/2 * * * /usr/local/bin/xsrv-backup.sh >> /tmp/xsrv-backup.log 2>&1
##
## 緊急時（xserverトラブル等）は上記cron行をコメントアウトするだけでOK
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
    rsync -avz --delete --exclude='.git' --exclude='.gitignore' -e "$XSRV_SSH" "$src" "$dst/" >> "$TMPLOG" 2>&1
    if [ $? -eq 0 ]; then
        log "rsync ${label}: OK"
    else
        log "rsync ${label}: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
    fi
    > "$TMPLOG"
}

run_git() {
    local label="$1"
    local dir="$2"
    cd "$dir" && \
        git add -A >> "$TMPLOG" 2>&1 && \
        git commit -m "auto: $(date '+%Y-%m-%d %H:%M:%S')" >> "$TMPLOG" 2>&1 || echo "No changes" >> "$TMPLOG" 2>&1
    git push >> "$TMPLOG" 2>&1
    if [ $? -eq 0 ]; then
        log "git push ${label}: OK"
    else
        log "git push ${label}: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
    fi
    > "$TMPLOG"
}

START=$(date '+%Y-%m-%d %H:%M:%S')
log "START: ${START}"

run_rsync "gospel-haiku.com"  "$XSRV_GH_SRC" "$XSRV_GH_DST"
run_git   "gospel-haiku.com"  "$XSRV_GH_DST"

run_rsync "minorugh.com"      "$XSRV_MN_SRC" "$XSRV_MN_DST"
run_git   "minorugh.com"      "$XSRV_MN_DST"

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
echo ""
