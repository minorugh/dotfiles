#!/bin/bash
###########################################################################
## Sync, merge and backup GH member password files
## Updated 2026.3.23
##
## cron: 40 23 * * * /usr/local/bin/automerge.sh >> /tmp/cron.log 2>&1
###########################################################################

HOME=/home/minoru
source $HOME/.keychain/$HOSTNAME-sh
BACKUP_DIR="${HOME}/Dropbox/passwd/backup"
PASSWD_DIR="${HOME}/Dropbox/passwd"
MERGE_SCRIPT="${PASSWD_DIR}/lib/mergepasswd.pl"
REMOTE="xsrv:/home/minorugh/gospel-haiku.com/passwd"
LOG_PREFIX="[automerge]"

START=$(date '+%Y-%m-%d %H:%M:%S')
TMPLOG=$(mktemp)
ERRORS=0

log() {
    echo "${LOG_PREFIX} $1"
}

run() {
    local label="$1"; shift
    "$@" >> "$TMPLOG" 2>&1
    if [ $? -eq 0 ]; then
        log "${label}: OK"
    else
        log "${label}: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
    fi
    > "$TMPLOG"
}

log "START: ${START}"

## Step 1: サーバーから4ファイルをダウンロード
run "Step1 rsync dmember" rsync -azu "${REMOTE}/dmember.cgi" "${PASSWD_DIR}/dmember.cgi"
run "Step1 rsync wmember" rsync -azu "${REMOTE}/wmember.cgi" "${PASSWD_DIR}/wmember.cgi"
run "Step1 rsync smember" rsync -azu "${REMOTE}/smember.cgi" "${PASSWD_DIR}/smember.cgi"
run "Step1 rsync mmember" rsync -azu "${REMOTE}/mmember.cgi" "${PASSWD_DIR}/mmember.cgi"

## Step 2: ローカルでマージ（wmemberを再生成）
run "Step2 merge" perl "${MERGE_SCRIPT}"

## Step 3: smember=dmember、mmember=wmember にコピー
run "Step3 copy smember" cp "${PASSWD_DIR}/dmember.cgi" "${PASSWD_DIR}/smember.cgi"
run "Step3 copy mmember" cp "${PASSWD_DIR}/wmember.cgi" "${PASSWD_DIR}/mmember.cgi"

## Step 4: git commit + 直近7日分のzipバックアップ
git -C "${PASSWD_DIR}" add -A >> "$TMPLOG" 2>&1
git -C "${PASSWD_DIR}" commit -m "automerge: $(date '+%Y-%m-%d %H:%M:%S')" >> "$TMPLOG" 2>&1 || true
log "Step4 git: OK"

mkdir -p "${BACKUP_DIR}"
ZIPFILE="${BACKUP_DIR}/passwd_$(date '+%Y%m%d').zip"
run "Step4 zip" bash -c "cd '${PASSWD_DIR}' && zip -q '${ZIPFILE}' dmember.cgi wmember.cgi smember.cgi mmember.cgi"
find "${BACKUP_DIR}" -name "passwd_*.zip" -mtime +7 -delete

## Step 5: 全4ファイルをサーバーへ同期
run "Step5 upload dmember" rsync -azu "${PASSWD_DIR}/dmember.cgi" "${REMOTE}/dmember.cgi"
run "Step5 upload smember" rsync -azu "${PASSWD_DIR}/smember.cgi" "${REMOTE}/smember.cgi"
run "Step5 upload wmember" rsync -azu "${PASSWD_DIR}/wmember.cgi" "${REMOTE}/wmember.cgi"
run "Step5 upload mmember" rsync -azu "${PASSWD_DIR}/mmember.cgi" "${REMOTE}/mmember.cgi"

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
