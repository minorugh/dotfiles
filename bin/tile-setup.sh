#!/bin/bash

# Simplenoteが開いていたら両方閉じる、なければ起動してタイル配置
CID=$(wmctrl -l | grep -i "simplenote" | tail -n 1 | awk '{print $1}')

if [ -n "$CID" ]; then
    # --- 閉じる ---
    wmctrl -i -c "$CID"
    wmctrl -lx | grep -i "emacs" | tail -n 1 | awk '{print $1}' | xargs -I{} wmctrl -i -c {}
else
    # --- 起動してタイル配置 ---
    PROFILE="Default"
    read -r _ _ W_X W_Y W_W W_H < <(xprop -root _NET_WORKAREA | sed 's/,//g')
    HALF_W=$((W_W / 2))

    google-chrome --new-window --profile-directory="$PROFILE" --app=https://app.simplenote.com/ &
    emacsclient -c -n &

    echo "Simplenoteウィンドウを待機中..."
    for i in $(seq 1 20); do
        CID=$(wmctrl -l | grep -i "simplenote" | tail -n 1 | awk '{print $1}')
        [ -n "$CID" ] && break
        sleep 1
    done
    [ -z "$CID" ] && CID=$(wmctrl -lx | grep -i "google-chrome" | tail -n 1 | awk '{print $1}')

    for i in $(seq 1 20); do
        EID=$(wmctrl -lx | grep -i "emacs" | tail -n 1 | awk '{print $1}')
        [ -n "$EID" ] && break
        sleep 1
    done

    if [ -n "$CID" ]; then
        wmctrl -i -r "$CID" -b remove,maximized_vert,maximized_horz
        wmctrl -i -r "$CID" -e 0,0,0,$HALF_W,$W_H
        wmctrl -i -r "$CID" -b add,maximized_vert
    fi

    if [ -n "$EID" ]; then
        wmctrl -i -r "$EID" -b remove,maximized_vert,maximized_horz
        wmctrl -i -r "$EID" -e 0,$HALF_W,0,$HALF_W,$W_H
        wmctrl -i -r "$EID" -b add,maximized_vert
    fi

    sleep 1
    emacsclient -e '(my-howm-create-with-category)'

    echo "配置完了: Chrome(ID:$CID) / Emacs(ID:$EID)"
fi
