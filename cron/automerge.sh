#!/bin/bash
###########################################################################
## Sync, merge and backup GH member password files
## Updated 2026.3.18
##
## cron: 40 23 * * * /usr/local/bin/automerge.sh >> /tmp/automerge.log 2>&1
###########################################################################

HOME=/home/minoru
source $HOME/.keychain/$HOSTNAME-sh
# BACKUP_DIR="${HOME}/Dropbox/GH/reg/passwd/backup"
# PASSWD_DIR="${HOME}/Dropbox/GH/reg/passwd"
BACKUP_DIR="${HOME}/Dropbox/passwd/backup"
PASSWD_DIR="${HOME}/Dropbox/passwd"
MERGE_SCRIPT="${PASSWD_DIR}/mergepasswd.pl"
REMOTE="xsrv:/home/minorugh/gospel-haiku.com/passwd"
LOG_PREFIX="[automerge]"

START=$(date '+%Y-%m-%d %H:%M:%S')
TMPLOG=$(mktemp)

run() {
    "$@" >> "$TMPLOG" 2>&1
    return $?
}

## Step 1: サーバーから4ファイルをダウンロード
run rsync -azu "${REMOTE}/dmember.cgi" "${PASSWD_DIR}/dmember.cgi" || { echo "${LOG_PREFIX} ERROR Step1(dmember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
run rsync -azu "${REMOTE}/wmember.cgi" "${PASSWD_DIR}/wmember.cgi" || { echo "${LOG_PREFIX} ERROR Step1(wmember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
run rsync -azu "${REMOTE}/smember.cgi" "${PASSWD_DIR}/smember.cgi" || { echo "${LOG_PREFIX} ERROR Step1(smember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
run rsync -azu "${REMOTE}/mmember.cgi" "${PASSWD_DIR}/mmember.cgi" || { echo "${LOG_PREFIX} ERROR Step1(mmember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }

## Step 2: ローカルでマージ（wmemberを再生成）
run perl "${MERGE_SCRIPT}" || { echo "${LOG_PREFIX} ERROR Step2(merge): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }

## Step 3: smember=dmember、mmember=wmember にコピー
run cp "${PASSWD_DIR}/dmember.cgi" "${PASSWD_DIR}/smember.cgi" || { echo "${LOG_PREFIX} ERROR Step3(copy): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
run cp "${PASSWD_DIR}/wmember.cgi" "${PASSWD_DIR}/mmember.cgi" || { echo "${LOG_PREFIX} ERROR Step3(copy): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }

## Step 4: バックアップ zip作成（90日以上古いものは削除）
mkdir -p "${BACKUP_DIR}"
ZIPFILE="${BACKUP_DIR}/passwd_$(date '+%Y%m%d').zip"
run bash -c "cd '${PASSWD_DIR}' && zip -q '${ZIPFILE}' dmember.cgi wmember.cgi smember.cgi mmember.cgi" || { echo "${LOG_PREFIX} ERROR Step4(zip): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
find "${BACKUP_DIR}" -name "passwd_*.zip" -mtime +90 -delete

## Step 5: 全4ファイルをサーバーへ同期
run rsync -azu "${PASSWD_DIR}/dmember.cgi" "${REMOTE}/dmember.cgi" || { echo "${LOG_PREFIX} ERROR Step5(dmember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
run rsync -azu "${PASSWD_DIR}/smember.cgi" "${REMOTE}/smember.cgi" || { echo "${LOG_PREFIX} ERROR Step5(smember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
run rsync -azu "${PASSWD_DIR}/wmember.cgi" "${REMOTE}/wmember.cgi" || { echo "${LOG_PREFIX} ERROR Step5(wmember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }
run rsync -azu "${PASSWD_DIR}/mmember.cgi" "${REMOTE}/mmember.cgi" || { echo "${LOG_PREFIX} ERROR Step5(mmember): $(date '+%Y-%m-%d %H:%M:%S')"; cat "$TMPLOG"; rm -f "$TMPLOG"; exit 1; }

rm -f "$TMPLOG"
echo "${LOG_PREFIX} OK: ${START} → $(date '+%Y-%m-%d %H:%M:%S')"
