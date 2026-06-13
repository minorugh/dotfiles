#!/bin/bash
#######################################################################
## xsrv-backup.sh
## xserver → ローカルへ rsync + git commit + push（2ドメイン）
## systemd user timer で毎日 22:05 に実行
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
COMMIT_LEAN_MAX=200

if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() { echo "${LOG_PREFIX} $1"; }

wait_for_network() {
    local max_attempts=10
    local wait_sec=15
    for i in $(seq 1 $max_attempts); do
        if ping -c 1 -W 3 8.8.8.8 > /dev/null 2>&1; then
            return 0
        fi
        log "ネットワーク未接続、${wait_sec}秒待機... (${i}/${max_attempts})"
        sleep $wait_sec
    done
    log "ネットワーク接続タイムアウト、処理を中止します"
    rm -f "$TMPLOG"
    exit 1
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
    cd "$dir"
    git add -A >> "$TMPLOG" 2>&1
    if git diff --cached --quiet; then
        log "git ${label}: 変更なし、スキップ"
    else
        git commit -m "auto: $(date '+%Y-%m-%d %H:%M:%S')" >> "$TMPLOG" 2>&1
        git push >> "$TMPLOG" 2>&1
        if [ $? -eq 0 ]; then
            log "git push ${label}: OK"
        else
            log "git push ${label}: ERROR"
            cat "$TMPLOG"
            ERRORS=$((ERRORS + 1))
        fi
    fi
    > "$TMPLOG"
}

commit_lean() {
    local label="$1"
    local dir="$2"
    cd "$dir"
    local count=$(git log --oneline | wc -l)
    if [ "$count" -gt "$COMMIT_LEAN_MAX" ]; then
        log "${label}: commit履歴 ${count}件、200件超えのためリセット"
        git checkout --orphan newbranch
        git add -A
        git commit -m "reset: history truncated at $(date '+%Y-%m-%d')"
        git branch -D main
        git branch -m main
        git push --force
        log "${label}: commit履歴リセット完了"
    fi
}

START=$(date '+%Y-%m-%d %H:%M:%S')
log "START: ${START}"

wait_for_network

run_rsync "gospel-haiku.com"  "$XSRV_GH_SRC" "$XSRV_GH_DST"
run_git   "gospel-haiku.com"  "$XSRV_GH_DST"

run_rsync "minorugh.com"      "$XSRV_MN_SRC" "$XSRV_MN_DST"
run_git   "minorugh.com"      "$XSRV_MN_DST"

commit_lean "gospel-haiku.com" "$XSRV_GH_DST"
commit_lean "minorugh.com"     "$XSRV_MN_DST"

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
