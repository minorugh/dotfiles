#!/bin/bash
#######################################################################
## Execute nightly makefile for backup
## Updated 2026.6.19
##
## cron 本実行:     50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
## cron フォールバック: 5 5-12 * * * /usr/local/bin/autobackup.sh --check >> /tmp/cron.log 2>&1
##   --check: フラグの日付が「今日」でも「昨日」でもなければ実行する
##            （定時実行が飛んだ日だけ、5-12時のどこかで一度補う）
######################################################################

HOME=/home/minoru
LOG_PREFIX="[autobackup]"
MAKEFILE="$HOME/src/github.com/minorugh/dotfiles/backup/Makefile"
FLAG_FILE="$HOME/.cache/autobackup/last-success"
TMPLOG=$(mktemp)
ERRORS=0

# SSHエージェントの設定を読み込む
if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() {
    echo "${LOG_PREFIX} $1"
}

TODAY=$(date '+%Y%m%d')
YESTERDAY=$(date -d 'yesterday' '+%Y%m%d')

## --check: フラグの状態に応じて判定する
if [ "$1" = "--check" ]; then
    FLAG=$(cat "$FLAG_FILE" 2>/dev/null)
    if [ "$FLAG" = "$TODAY" ]; then
        rm -f "$TMPLOG"
        exit 0
    fi
    if [ "$FLAG" = "$YESTERDAY" ]; then
        mkdir -p "$(dirname "$FLAG_FILE")"
        echo "$TODAY" > "$FLAG_FILE"
        log "skip: $(date '+%Y-%m-%d %H:%M')"
        rm -f "$TMPLOG"
        exit 0
    fi
    log "check: フラグは${FLAG:-未記録}、実行します"
fi

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
    mkdir -p "$(dirname "$FLAG_FILE")"
    echo "$TODAY" > "$FLAG_FILE"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
