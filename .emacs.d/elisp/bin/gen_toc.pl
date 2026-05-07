#!/bin/bash

# --- 設定項目 ---
# Chromeのプロファイル名。通常は "Default"
PROFILE="Default"
# 画面領域の取得（パネル等を除いた有効範囲）
read -r _ _ W_X W_Y W_W W_H < <(xprop -root _NET_WORKAREA | sed 's/,//g')
HALF_W=$((W_W / 2))

# --- 1. アプリの起動 ---
# app.simplenote.com を直接開く（ログイン済みならノート画面が即表示される）
google-chrome --new-window --profile-directory="$PROFILE" --app=https://app.simplenote.com/ &

# Emacs Client を起動
emacsclient -c -n &

# --- 2. ウィンドウ待機（タイトルが現れるまでポーリング） ---
echo "Simplenoteウィンドウを待機中..."
for i in $(seq 1 20); do
    CID=$(wmctrl -l | grep -i "simplenote" | tail -n 1 | awk '{print $1}')
    [ -n "$CID" ] && break
    sleep 1
done

# タイトル検出できなかった場合のフォールバック
if [ -z "$CID" ]; then
    echo "タイトル未検出、Chrome窓にフォールバック"
    CID=$(wmctrl -lx | grep -i "google-chrome" | tail -n 1 | awk '{print $1}')
fi

# Emacs も同様にポーリング
for i in $(seq 1 20); do
    EID=$(wmctrl -lx | grep -i "emacs" | tail -n 1 | awk '{print $1}')
    [ -n "$EID" ] && break
    sleep 1
done

# --- 3. Chrome (Simplenote) を左半分に配置 ---
if [ -n "$CID" ]; then
    wmctrl -i -r "$CID" -b remove,maximized_vert,maximized_horz
    wmctrl -i -r "$CID" -e 0,0,0,$HALF_W,$W_H
    wmctrl -i -r "$CID" -b add,maximized_vert
fi

# --- 4. Emacs を右半分に配置 ---
if [ -n "$EID" ]; then
    wmctrl -i -r "$EID" -b remove,maximized_vert,maximized_horz
    wmctrl -i -r "$EID" -e 0,$HALF_W,0,$HALF_W,$W_H
    wmctrl -i -r "$EID" -b add,maximized_vert
fi

echo "配置完了: Chrome(ID:$CID) / Emacs(ID:$EID)。*"
