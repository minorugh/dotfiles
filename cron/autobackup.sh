#!/bin/bash
#######################################################################
## Execute nightly makefile for backup
## Updated 2026.3.23
##
## cron: 50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
######################################################################

HOME=/home/minoru
LOG_PREFIX="[autobackup]"
MAKEFILE="$HOME/Dropbox/makefile"
TMPLOG=$(mktemp)
ERRORS=0

# SSHエージェントの設定を読み込む
if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() {
    echo "${LOG_PREFIX} $1"
}

run_target() {
    local label="$1"
    local target="$2"
    make -f "$MAKEFILE" "$target" >> "$TMPLOG" 2>&1
    if [ $? -eq 0 ]; then
        log "${label}: OK"
    else
        log "${label}: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
    fi
    > "$TMPLOG"
}

START=$(date '+%Y-%m-%d %H:%M:%S')
log "START: ${START}"

run_target "melpa"      melpa
run_target "git-push (GH+minorugh.com)"   git-push
run_target "mattermost"  mattermost-backup
run_target "mozc"        mozc-backup
run_target "keyring"     keyring-backup
run_target "gitea"       gitea-backup
run_target "filezilla"   filezilla-backup
run_target "thunderbird" thunderbird-backup

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
