#!/bin/bash
# autobackup.sh
# 毎晩23:50にmakefileを実行するためのラッパースクリプト

# SSHエージェントの設定を読み込む
if [ -f "/home/minoru/.keychain/$(hostname)-sh" ]; then
    . /home/minoru/.keychain/$(hostname)-sh 2>/dev/null
fi

# makefile を実行
make -f /home/minoru/Dropbox/makefile
