#!/bin/bash
#######################################################################
## xsrv-backup-smart.sh
## xserver → ローカルへ rsync のみ（gospel-haiku.com のみ）
## git push は systemd timer の xsrv-backup.sh が担当
## ~/.emacs.d/elisp/bin/ に置いて Emacs から呼ぶ専用スクリプト
#######################################################################

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"
XSRV_HOST="minorugh@sv13268.xserver.jp"
XSRV_GH_SRC="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html/"
XSRV_GH_DST="$HOME/src/github.com/minorugh/xsrv-GH"

if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

echo "${LOG_PREFIX} START: $(date '+%Y-%m-%d %H:%M:%S')"
rsync -az --delete --exclude='.git' --exclude='.gitignore' \
      -e "ssh -p 10022" "$XSRV_GH_SRC" "$XSRV_GH_DST/"
echo "${LOG_PREFIX} END: $(date '+%Y-%m-%d %H:%M:%S') ($( [ $? -eq 0 ] && echo OK || echo ERROR))"
