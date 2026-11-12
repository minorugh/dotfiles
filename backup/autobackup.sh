#!/bin/bash
#######################################################################
## Execute nightly makefile for backup
## Updated 2026.11.12 (フラグを日付から成功時刻に変更。NIGHT SUSPEND 運用でも毎朝補えるように。旧: 2026.11.10 git-push から dotfiles を除外、2026.11.3 kdbx-gdrive 追加、2026.7.10 thunderbird-backup 廃止)
##
## cron 本実行:     50 23 * * * /usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1
## cron フォールバック: 5 5-12 * * * /usr/local/bin/autobackup.sh --check >> /tmp/cron.log 2>&1
##   --check: 昨夜 23:50 以降に成功していなければ実行する
##            （本実行が飛んだ日、たとえば NIGHT SUSPEND した翌朝に、5-12時のどこかで一度補う）
######################################################################

HOME=/home/minoru
LOG_PREFIX="[autobackup]"
MAKEFILE="$HOME/src/github.com/minorugh/dotfiles/backup/Makefile"
FLAG_FILE="$HOME/.cache/autobackup/last-success"
TMPLOG=$(mktemp)
ERRORS=0


log() {
    echo "${LOG_PREFIX} $1"
}

## --check: 昨夜の本実行の予定時刻以降に成功していれば skip
if [ "$1" = "--check" ]; then
    THRESHOLD=$(date -d 'yesterday 23:50' '+%s')
    FLAG=$(cat "$FLAG_FILE" 2>/dev/null)
    if [ "${FLAG:-0}" -ge "$THRESHOLD" ]; then
        log "skip: $(date '+%Y-%m-%d %H:%M')"
        rm -f "$TMPLOG"
        exit 0
    fi
    log "check: 前回成功は${FLAG:-未記録}、実行します"
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

START=$(date '+%Y-%m-%d %H:%M:%S')
log "START: ${START}"

run_target "git-push (GH+minorugh.com)"   git-push
run_target "mozc"        mozc-backup
run_target "keyring"     keyring-backup
run_target "kdbx-gdrive" kdbx-gdrive-backup
run_target "zsh-history" zsh-history-backup
run_target "filezilla"   filezilla-backup
run_target "keychron-rules" keychron-rules-backup
run_target "readmes"     readmes-backup
run_target "gsync"       gsync

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
    mkdir -p "$(dirname "$FLAG_FILE")"
    date '+%s' > "$FLAG_FILE"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
