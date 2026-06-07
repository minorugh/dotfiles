#!/bin/bash
#######################################################################
## Execute nightly makefile for backup
## Updated 2026.3.23
##
## cron: 50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
######################################################################

HOME=/home/minoru
LOG_PREFIX="[autobackup]"
MAKEFILE="$HOME/src/github.com/minorugh/dotfiles/backup/Makefile"
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

run_melpa() {
    local before=$(git -C "$HOME/Dropbox/backup/elpa" rev-parse HEAD 2>/dev/null)
    make -f "$MAKEFILE" melpa >> "$TMPLOG" 2>&1
    if [ $? -ne 0 ]; then
        log "melpa: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
    else
        local after=$(git -C "$HOME/Dropbox/backup/elpa" rev-parse HEAD 2>/dev/null)
        if [ "$before" != "$after" ]; then
            log "melpa: コミット＆プッシュしました"
        else
            log "melpa: 更新はありません"
        fi
    fi
    > "$TMPLOG"
}

START=$(date '+%Y-%m-%d %H:%M:%S')
log "START: ${START}"

run_melpa
run_target "git-push (GH+minorugh.com)"   git-push
run_target "mozc"        mozc-backup
run_target "keyring"     keyring-backup
run_target "zsh-history" zsh-history-backup
run_target "gitea"       gitea-backup
run_target "filezilla"   filezilla-backup
run_target "thunderbird" thunderbird-backup
run_target "abook"       abook-backup
run_target "readmes"     readmes-backup

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi

