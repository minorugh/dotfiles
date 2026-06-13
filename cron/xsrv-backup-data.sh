#!/bin/bash
HOME=/home/minoru
LOG_PREFIX="[xsrv-backup-data]"
XSRV_SSH="ssh -p 10022"
XSRV_HOST="minorugh@sv13268.xserver.jp"
XSRV_BASE="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html"
DST="$HOME/src/github.com/minorugh/xsrv-GH"

if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() { echo "${LOG_PREFIX} $1"; }

echo "${LOG_PREFIX} START: $(date '+%Y-%m-%d %H:%M:%S')"

for kukai in d_kukai m_kukai s_kukai w_kukai; do
    rsync -az --mkpath -e "$XSRV_SSH" \
        "$XSRV_BASE/${kukai}/data/" "$DST/${kukai}/data/"
done

cd "$DST"
git add -A
if git diff --cached --quiet; then
    log "変更なし、スキップ"
else
    git commit -m "auto: $(date '+%Y-%m-%d %H:%M:%S')"
    git push
    log "push完了"
fi

echo "${LOG_PREFIX} END: $(date '+%Y-%m-%d %H:%M:%S')"
