#!/bin/bash
# .autostart.sh
# Created : 2024-10-01
# Updated : 2026-06-27
#
# GUI ログイン時に autostart.desktop 経由で自動実行されるスクリプト。
# 以下の処理を順に行う：
#
# 1. env/ を bindfs で ~/.env_source にマウント（新規ファイル自動反映）
# 2. mozc・keyring を Dropbox からリストア
# 3. Emacs を起動し xdotool で最小化（--iconic はちらつくため非採用）
# 4. neomutt を tmux セッションで起動（古いセッションをクリアしてから）
# 5. X スクリーンセーバー・DPMS タイマー無効化（xscreensaver 削除後のフリッカ対策）
#
# 依存: secret-tool, rsync, xdotool
# 関連: .config/autostart/autostart.desktop, bin/emacs-toggle

# env/ を ~/.env_source にbindfsで透過マウント（新規ファイル即反映のため）
ENV_MNT=~/src/github.com/minorugh/dotfiles/env
mountpoint -q "$ENV_MNT" || bindfs ~/.env_source "$ENV_MNT"

# ssh-agent / keychain は使わない（2026-09-30 廃止）。空パスフレーズ + IdentityFile で ssh は agent 不要
rsync -av --delete ~/Dropbox/backup/mozc/.mozc/ ~/.mozc/
cp -a ~/Dropbox/backup/keyrings/. ~/.local/share/keyrings/

# Emacs を起動し、表示されたウィンドウを xdotool で最小化する
# --iconic だとちらつくため、起動後に windowminimize で対処
emacs-start.sh &
sleep 3
wid=$(xdotool search --class Emacs 2>/dev/null | tail -n1)
[ -n "$wid" ] && xdotool windowminimize "$wid"

# neomutt を tmux セッションで起動
# 再起動時に前回セッションが残っていればクリアしてから起動
tmux kill-session -t mail 2>/dev/null
cd ~/Downloads && tmux new-session -d -s mail 'neomutt'
tmux set -t mail status off

# X スクリーンセーバー・DPMS タイマー無効化
# xscreensaver 削除後に X 本体の timeout（デフォルト600秒）が直接発火しフリッカが起きるため
xset s off
xset dpms 0 0 0
