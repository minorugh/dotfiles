#!/bin/bash
#######################################################################
## neomutt.sh
## NeoMutt 起動スクリプト
##
## - mail セッションが無ければ作成して表示
## - 既に表示中なら閉じる（tmux セッションは残す）
## - 非表示なら attach
## - 蓋閉じ時（eDP-1 inactive）は外部モニタ DP-1-2 に表示
##
#######################################################################

# 既に表示中なら閉じる（q キーと同じ挙動）
if tmux list-clients -t mail 2>/dev/null | grep -q .; then
    WID=$(xdotool search --name "NeoMutt Mail" | head -1)
    if [ -n "$WID" ]; then
        wmctrl -ic "$WID"
    fi
    exit 0
fi

# mail セッションが無ければ作成
if ! tmux has-session -t mail 2>/dev/null; then
    cd ~/Downloads || exit 1
    tmux new-session -d -s mail 'neomutt'
    tmux set -t mail status off
fi

# 外部モニタの X オフセットを取得（DP-1-2 が接続されていれば 1920）
EXT_X=$(xrandr | grep "^DP-1-2 connected" | grep -oP '\d+x\d+\+\K\d+(?=\+\d)' | head -1)

# eDP-1 が有効かどうかで表示先を決定
# 蓋閉じ = eDP-1 の解像度行が消える（"eDP-1 connected" のみになる）
if xrandr | grep -q "^eDP-1 connected [0-9]"; then
    # 本体画面が有効 → 通常起動
    GEOMETRY=""
else
    # 蓋閉じ → 外部モニタの座標に配置
    GEOMETRY="--geometry=+${EXT_X:-1920}+0"
fi

# 表示
xfce4-terminal --maximize --title="NeoMutt Mail" \
    $GEOMETRY \
    -e "tmux attach -t mail"

exit 0
