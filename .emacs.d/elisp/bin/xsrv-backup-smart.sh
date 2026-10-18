#!/bin/bash
#######################################################################
## xsrv-backup-smart.sh
## xserver → ローカルへ rsync のみ（gospel-haiku.com / minorugh.com）
## 手動実行専用。~/.emacs.d/elisp/bin/ に置いて Emacs から呼ぶ。
## commit と push は夜間処理（autobackup.sh）が担当。
#######################################################################

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"
XSRV_HOST="xsrv"

log() { echo "${LOG_PREFIX} $1"; }

sync_site() {
    local label="$1" src="$2" dst="$3"
    log "START ${label}: $(date '+%Y-%m-%d %H:%M:%S')"
    rsync -a --delete --exclude='.git' --exclude='.gitignore' --exclude='Makefile' \
          -e "ssh -p 10022" "$src" "$dst/"
    local rc=$?
    if [ $rc -eq 0 ]; then
        log "END ${label}: OK"
    else
        log "END ${label}: ERROR (rsync rc=${rc})"
    fi
}

sync_site GH "$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html/" "$HOME/src/github.com/minorugh/xsrv-GH"
sync_site minorugh "$XSRV_HOST:/home/minorugh/minorugh.com/public_html/" "$HOME/src/github.com/minorugh/xsrv-minorugh"
