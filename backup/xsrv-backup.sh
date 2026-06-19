#!/bin/bash
#######################################################################
## xsrv-backup.sh
## xserver → Dropbox/GH へ動的ファイルを rsync（毎時）
# cron で 0:00 & 07:00〜23:00 毎時実行
## git push は autobackup.sh（毎晩）に委譲
##
## 【注意】~/xsrv-rsync.lock が存在する間はスキップする
## ローカルで動的ファイルを編集・deploy する際は lock を発行すること
## （Emacs の 60-xsrv-dired.el で自動連携）
#######################################################################

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"
LOGFILE=/tmp/xsrv-backup.log
LOCKFILE="$HOME/xsrv-rsync.lock"
XSRV_SSH="ssh -p 10022"
XSRV_HOST="minorugh@sv13268.xserver.jp"
XSRV_BASE="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html"
DST="$HOME/Dropbox/GH"
ERRORS=0

if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() { echo "${LOG_PREFIX} $1" | tee -a "$LOGFILE"; }

# ログファイルをリセット
echo "" > "$LOGFILE"
log "START: $(date '+%Y-%m-%d %H:%M:%S')"

# ロックチェック
if [ -f "$LOCKFILE" ]; then
    log "ロックファイルあり、スキップします"
    log "END: $(date '+%Y-%m-%d %H:%M:%S')"
    exit 0
fi

# 緊急停止チェック（make cron-stop で発行、make cron-start で解除）
[ -f "$HOME/.xsrv-backup-stop" ] && exit 0

run_rsync() {
    local label="$1"
    local src="$2"
    local dst="$3"
    rsync -az --mkpath -e "$XSRV_SSH" "$src" "$dst" >> "$LOGFILE" 2>&1
    if [ $? -eq 0 ]; then
        log "rsync ${label}: OK"
    else
        log "rsync ${label}: ERROR"
        ERRORS=$((ERRORS + 1))
    fi
}

# フォルダー単位
for dir in \
    danwa/data \
    danwa/html \
    dia/divoice \
    d_kukai/data \
    d_kukai/score \
    m_kukai/data \
    m_kukai/score \
    s_kukai/data \
    s_kukai/score \
    w_kukai/data \
    w_kukai/score; do
    run_rsync "$dir" "$XSRV_BASE/${dir}/" "$DST/${dir}/"
done

# 特定ファイル単位
for file in \
    d_kukai/_datefrag.dat \
    m_kukai/_progfrag.dat \
    s_kukai/_progfrag.dat \
    w_kukai/_progfrag.dat; do
    run_rsync "$file" "$XSRV_BASE/${file}" "$DST/${file}"
done

# make -C "$DST" git >> /dev/null 2>&1 && log "git push: OK" || log "git push: ERROR"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
