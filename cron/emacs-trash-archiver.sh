#!/bin/bash

# パスの設定
TRASH_DIR="$HOME/src/github.com/minorugh/dotfiles/.emacs.d/tmp/trash"
# フォルダが存在し、かつ空でない場合のみ実行
if [ -d "$TRASH_DIR" ] && [ "$(ls -A "$TRASH_DIR")" ]; then
    # タイムスタンプ付きの名前を生成
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    ARCHIVE_NAME="${TRASH_DIR}_${TIMESTAMP}"

    # 1. フォルダをリネーム
    mv "$TRASH_DIR" "$ARCHIVE_NAME"

    # 2. trash-putでシステムのゴミ箱へ送る
    /usr/bin/trash-put "$ARCHIVE_NAME"
fi
