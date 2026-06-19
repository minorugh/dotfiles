#!/bin/bash
# emacs-trash-sweep.sh
# Emacs 専用ゴミ箱をシステムゴミ箱へ定期移送する
#
# cron: 45 23 1 * * /usr/local/bin/emacs-trash-sweep.sh >> /tmp/cron.log 2>&1

HOME=/home/minoru
LOG_PREFIX="[emacs-trash]"
TRASH_DIR="${HOME}/.emacs.d/tmp/trash"

START=$(date '+%Y-%m-%d %H:%M:%S')
echo "${LOG_PREFIX} START: ${START}"

if [ -d "$TRASH_DIR" ] && [ "$(ls -A "$TRASH_DIR")" ]; then
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    ARCHIVE="${TRASH_DIR}_${TIMESTAMP}"

    mv "$TRASH_DIR" "$ARCHIVE"
    mkdir -p "$TRASH_DIR"

    if /usr/bin/trash-put "$ARCHIVE"; then
        echo "${LOG_PREFIX} swept: $(basename "$ARCHIVE"): OK"
    else
        mv "$ARCHIVE" "$TRASH_DIR"
        echo "${LOG_PREFIX} trash-put failed: ERROR"
    fi
else
    echo "${LOG_PREFIX} nothing to sweep: OK"
fi

END=$(date '+%Y-%m-%d %H:%M:%S')
echo "${LOG_PREFIX} END: ${END}"

