#!/bin/bash
#######################################################################
## xsrv-backup.sh
## xserver → ローカルへ rsync + git commit + push（2ドメイン）
## systemd user timer で 07:00〜22:00 毎時実行
## xsrv-backup.service の ExecStart 1行目（2行目は xsrv-backup-data.sh）
##
## 【注意】xsrv-backup-data.sh と同一リポジトリ（xsrv-GH）を共有
## rsync の --exclude はサーバーからの取得を防ぐだけで、
## xsrv-backup-data.sh がローカルに書き込んだ変化は防げない。
## そのため run_git() 内で git reset HEAD により
## d_kukai/data 等と fstat/log を git の対象から除外している。
#######################################################################

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"
LOGFILE=/tmp/xsrv-backup.log

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

log() { echo "${LOG_PREFIX} $1" | tee -a "$LOGFILE"; }

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
    rsync -az --delete --exclude='.git' --exclude='.gitignore' \
          --exclude='d_kukai/data/' \
          --exclude='m_kukai/data/' \
          --exclude='s_kukai/data/' \
          --exclude='w_kukai/data/' \
	  --exclude='fstat/log/' \
          -e "$XSRV_SSH" "$src" "$dst/" >> "$LOGFILE" 2>&1
    if [ $? -eq 0 ]; then
        log "rsync ${label}: OK"
    else
        log "rsync ${label}: ERROR"
        ERRORS=$((ERRORS + 1))
    fi
}

run_git() {
    local label="$1"
    local dir="$2"
    cd "$dir"
    git add -A >> /dev/null 2>&1
    # fstat/log はアクセスログで毎時変化。d_kukai 等は xsrv-backup-data.sh が別管理
    git reset HEAD d_kukai/data m_kukai/data s_kukai/data w_kukai/data fstat/log >> /dev/null 2>&1
    if git diff --cached --quiet; then
        log "git ${label}: 変更なし、スキップ"
    else
        git commit -m "auto: $(date '+%Y-%m-%d %H:%M:%S')" >> /dev/null 2>&1
        git push >> /dev/null 2>&1
        if [ $? -eq 0 ]; then
            log "git push ${label}: OK"
        else
            log "git push ${label}: ERROR"
            ERRORS=$((ERRORS + 1))
        fi
    fi
}

commit_lean() {
    local label="$1"
    local dir="$2"
    cd "$dir"
    local count=$(git log --oneline | wc -l)
    if [ "$count" -gt "$COMMIT_LEAN_MAX" ]; then
        log "${label}: commit履歴 ${count}件、${COMMIT_LEAN_MAX}件超えのためリセット"
        git checkout --orphan newbranch >> /dev/null 2>&1
        git add -A >> /dev/null 2>&1
        git commit -m "reset: history truncated at $(date '+%Y-%m-%d')" >> /dev/null 2>&1
        git branch -D main >> /dev/null 2>&1
        git branch -m main >> /dev/null 2>&1
        git push --force >> /dev/null 2>&1
        log "${label}: commit履歴リセット完了"
    fi
}

# ログファイルをリセット
echo "" > "$LOGFILE"

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
