#!/bin/bash
# cron 用自動バックアップ & git push

# ------------------------
# 環境設定
# ------------------------
export PATH=/usr/bin:/bin:/usr/local/bin
export HOME=/home/minoru
export LOGNAME=minoru
export LANG=ja_JP.UTF-8
export SHELL=/bin/bash

LOGFILE=${HOME}/auto-backup.log
echo "===== $(date '+%Y-%m-%d %H:%M:%S') =====" >> $LOGFILE

# ------------------------
# 1. MELPA スナップショット
# ------------------------
echo "[MELPA] Start" >> $LOGFILE
ELPA_DIR=${HOME}/Dropbox/backup/emacs/elpa
OLDEST=$(ls -rt $ELPA_DIR | head -n 1)
if [ -n "$OLDEST" ]; then
    rm -rf "$ELDEST"
    echo "[MELPA] Removed oldest snapshot: $OLDEST" >> $LOGFILE
fi
SNAPSHOT_NAME=$(date '+%Y%m%d%H%M%S').tar.gz
tar -C ${HOME}/.emacs.d -czf $ELPA_DIR/$SNAPSHOT_NAME elpa
echo "[MELPA] Created snapshot: $SNAPSHOT_NAME" >> $LOGFILE

# ------------------------
# 2. 各リポジトリ git push
# ------------------------
push_repo() {
    local REPO=$1
    local DESC=$2
    echo "[GIT] Pushing $DESC..." >> $LOGFILE
    if [ -d "$REPO/.git" ]; then
        cd "$REPO"
        git add -A
        git commit -m "auto commit $(date '+%Y-%m-%d %H:%M:%S')" || echo "[GIT] No changes to commit" >> $LOGFILE
        git push >> $LOGFILE 2>&1
        if [ $? -eq 0 ]; then
            echo "[GIT] Push success: $DESC" >> $LOGFILE
        else
            echo "[GIT] Push FAILED: $DESC" >> $LOGFILE
        fi
    else
        echo "[GIT] Not a git repo: $DESC" >> $LOGFILE
    fi
}

push_repo "${HOME}/src/github.com/minorugh/dotfiles" "dotfiles"
push_repo "${HOME}/Dropbox/GH" "Dropbox/GH"
push_repo "${HOME}/Dropbox/minorugh.com" "Dropbox/minorugh.com"

echo "===== Done =====" >> $LOGFILE
