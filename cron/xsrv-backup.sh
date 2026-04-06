#!/bin/bash
#######################################################################
## xsrv-backup.sh
## xserver → ローカルへ rsync + git commit + push（2ドメイン）
##
## cron: 0 7-22/2 * * * /usr/local/bin/xsrv-backup.sh >> /tmp/xsrv-backup.log 2>&1
##
## 緊急時（xserverトラブル等）は上記cron行をコメントアウトするだけでOK
#######################################################################

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"

XSRV_HOST="minorugh@sv13268.xserver.jp"
XSRV_SSH="ssh -p 10022"

XSRV_GH_SRC="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html/"
XSRV_GH_DST="$HOME/src/github.com/minorugh/xsrv-GH"

XSRV_MN_SRC="$XSRV_HOST:/home/minorugh/minorugh.com/public_html/"
XSRV_MN_DST="$HOME/src/github.com/minorugh/xsrv-minorugh"

# SSHエージェントの設定を読み込む
if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

START=$(date '+%Y-%m-%d %H:%M:%S')
echo "${START} [xsrv-backup] START"

## --- gospel-haiku.com ---
rsync -avz --delete --exclude='.git' --exclude='.gitignore' -e "$XSRV_SSH" "$XSRV_GH_SRC" "$XSRV_GH_DST/"
if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') rsync done: gospel-haiku.com"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: rsync failed: gospel-haiku.com"
    exit 1
fi

cd "$XSRV_GH_DST" && \
    git add -A && \
    git commit -m "auto-backup: $(date '+%Y-%m-%d %H:%M:%S')" || echo "No changes" && \
	    git push
if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') git push done: gospel-haiku.com"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: git push failed: gospel-haiku.com"
    exit 1
fi

## --- minorugh.com ---
rsync -avz --delete --exclude='.git' --exclude='.gitignore' -e "$XSRV_SSH" "$XSRV_MN_SRC" "$XSRV_MN_DST/"
if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') rsync done: minorugh.com"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: rsync failed: minorugh.com"
    exit 1
fi

cd "$XSRV_MN_DST" && \
    git add -A && \
    git commit -m "auto-backup: $(date '+%Y-%m-%d %H:%M:%S')" || echo "No changes" && \
	    git push
if [ $? -eq 0 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') git push done: minorugh.com"
else
    echo "$(date '+%Y-%m-%d %H:%M:%S') ERROR: git push failed: minorugh.com"
    exit 1
fi

echo "$(date '+%Y-%m-%d %H:%M:%S') [xsrv-backup] END"
