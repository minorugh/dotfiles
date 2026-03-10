#!/bin/bash
##################################################
## Sync, merge and backup GH member password files
## Updated 2026.3.8
##
## 処理の流れ:
##   Step1: サーバーから4ファイルをダウンロード
##   Step2: mergepasswd.pl で wmember を再生成
##   Step3: smember=dmember、mmember=wmember にコピー
##   Step4: バックアップ zip 作成（90日保持）
##   Step5: 全4ファイルをサーバーへアップロード
##
## cron: 40 23 * * * /usr/local/bin/myjob.sh >> /tmp/myjob.log 2>&1
## 詳細: ~/src/github.com/minorugh/dotfiles/cron/README.md
##################################################

HOME=/home/minoru
source $HOME/.keychain/$HOSTNAME-sh
BACKUP_DIR="${HOME}/Dropbox/GH/reg/passwd/backup"
PASSWD_DIR="${HOME}/Dropbox/GH/reg/passwd"
MERGE_SCRIPT="${PASSWD_DIR}/mergepasswd.pl"
REMOTE="xsrv:/home/minorugh/gospel-haiku.com/passwd"
LOG_PREFIX="[myjob]"

echo "${LOG_PREFIX} 開始: $(date '+%Y-%m-%d %H:%M:%S')"

## Step 1: サーバーから4ファイルをダウンロード
echo "${LOG_PREFIX} Step1: サーバーからダウンロード"
rsync -azu "${REMOTE}/dmember.cgi" "${PASSWD_DIR}/dmember.cgi"
rsync -azu "${REMOTE}/wmember.cgi" "${PASSWD_DIR}/wmember.cgi"
rsync -azu "${REMOTE}/smember.cgi" "${PASSWD_DIR}/smember.cgi"
rsync -azu "${REMOTE}/mmember.cgi" "${PASSWD_DIR}/mmember.cgi"

## Step 2: ローカルでマージ（wmemberを再生成）
echo "${LOG_PREFIX} Step2: wmemberにdmemberをマージ"
perl "${MERGE_SCRIPT}"

## Step 3: smember=dmember、mmember=wmember にコピー
echo "${LOG_PREFIX} Step3: smember/mmemberを生成"
cp "${PASSWD_DIR}/dmember.cgi" "${PASSWD_DIR}/smember.cgi"
cp "${PASSWD_DIR}/wmember.cgi" "${PASSWD_DIR}/mmember.cgi"

## Step 4: バックアップ zip作成（90日以上古いものは削除）
echo "${LOG_PREFIX} Step4: バックアップ作成"
mkdir -p "${BACKUP_DIR}"
ZIPFILE="${BACKUP_DIR}/passwd_$(date '+%Y%m%d').zip"
cd "${PASSWD_DIR}" && zip -q "${ZIPFILE}" dmember.cgi wmember.cgi smember.cgi mmember.cgi
find "${BACKUP_DIR}" -name "passwd_*.zip" -mtime +90 -delete

## Step 5: 全4ファイルをサーバーへ同期
echo "${LOG_PREFIX} Step5: サーバーへアップロード"
rsync -azu "${PASSWD_DIR}/dmember.cgi" "${REMOTE}/dmember.cgi"
rsync -azu "${PASSWD_DIR}/smember.cgi" "${REMOTE}/smember.cgi"
rsync -azu "${PASSWD_DIR}/wmember.cgi" "${REMOTE}/wmember.cgi"
rsync -azu "${PASSWD_DIR}/mmember.cgi" "${REMOTE}/mmember.cgi"

echo "${LOG_PREFIX} 完了: $(date '+%Y-%m-%d %H:%M:%S')"
