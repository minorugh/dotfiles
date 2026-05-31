#!/bin/bash
#######################################################################
## neomutt-watch.sh
## NeoMutt tmux セッション 強制リフレッシュスクリプト
##
## cron から呼ばれ、夜間 IMAP 接続切れで固まるのを防ぐため
## tmux セッションを無条件で再作成する。
## 実行のたびに /tmp/cron.log に日時を記録する。
##
## cron 設定：
##   */5 5,6,7 * * * /usr/local/bin/neomutt-watch.sh
##
#######################################################################

tmux kill-session -t mail 2>/dev/null

cd ~/Downloads && tmux new-session -d -s mail 'neomutt'
tmux set -t mail status off
echo "[neomutt-watch] END: $(date '+%Y-%m-%d %H:%M:%S') (OK)" >> /tmp/cron.log


