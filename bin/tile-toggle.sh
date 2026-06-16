#!/bin/bash
# =============================================================================
# tile-toggle
# アクティブウィンドウの左右タイル表示をトグルするスクリプト。
# デュアルモニター対応。F15などのショートカットから呼び出す。
#
# 動作:
#   1. 現在アクティブなウィンドウIDとX座標を取得する
#   2. xrandr でモニター情報を取得し、ウィンドウがどのモニターにいるか特定する
#   3. そのモニターの中央X座標と比較して左タイルか右タイルかを判定する
#   4. 左タイルなら Ctrl+Right で右タイルへ、それ以外は Ctrl+Left で左タイルへ
#
# 依存: xdotool wmctrl xrandr
# =============================================================================

active=$(xdotool getactivewindow 2>/dev/null)

if [ -z "$active" ]; then
    exit 1
fi

win_x=$(wmctrl -l -G | awk -v id="$active" '{
    dec=strtonum($1)
    if (dec == id) print $3
}')

monitor=$(xrandr | awk '/ connected/ && /[0-9]+x[0-9]+\+[0-9]+\+[0-9]+/ {
    match($0, /([0-9]+)x[0-9]+\+([0-9]+)/, arr)
    width=arr[1]; ox=arr[2]
    if (win_x >= ox && win_x < ox+width) {
        print ox, width
        exit
    }
}' win_x="$win_x")

mon_x=$(echo $monitor | awk '{print $1}')
mon_w=$(echo $monitor | awk '{print $2}')
center=$((mon_x + mon_w / 2))

if [ "$win_x" -lt "$center" ]; then
    xdotool key ctrl+Right
else
    xdotool key ctrl+Left
fi
