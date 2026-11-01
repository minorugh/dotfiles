#!/bin/bash

# サスペンド前のKeePassXCを終了
pkill -x keepassxc

# 終了を少し待つ
sleep 2

# KeePassXCを再起動・自動アンロック
/home/minoru/bin/keepassxc.sh
