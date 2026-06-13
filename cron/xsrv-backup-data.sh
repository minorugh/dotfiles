#!/bin/bash
HOME=/home/minoru
LOG_PREFIX="[xsrv-backup-data]"
XSRV_SSH="ssh -p 10022"
XSRV_HOST="minorugh@sv13268.xserver.jp"
XSRV_BASE="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html"
DST="$HOME/src/github.com/minorugh/xsrv-GH"
COMMIT_LEAN_MAX=200

if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() { echo "${LOG_PREFIX} $1"; }

commit_lean() {
    local count=$(git log --oneline | wc -l)
    if [ "$count" -gt "$COMMIT_LEAN_MAX" ]; then
        log "commit履歴 ${count}件、200件超えのためリセット"
        git checkout --orphan newbranch
        git add -A
        git commit -m "reset: history truncated at $(date '+%Y-%m-%d')"
        git branch -D main
        git branch -m main
        git push --force
        log "commit履歴リセット完了"
    fi
}

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

commit_lean

echo "${LOG_PREFIX} END: $(date '+%Y-%m-%d %H:%M:%S')"
