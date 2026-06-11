#!/bin/bash
# .autostart.sh
# Created : 2024-10-01
# Updated : 2026-06-11
#
# GUI ログイン時に autostart.desktop 経由で自動実行されるスクリプト。
# 以下の処理を順に行う：
#
# 1. ssh-agent リセット
# 2. mozc・keyring を Dropbox からリストア
# 3. SSH 鍵を keychain + secret-tool で自動入力（パスフレーズ不要）
# 4. keychain の環境変数をセッションに反映
# 5. Emacs を --iconic で起動（一瞬も前面表示されない）
# 6. neomutt を tmux セッションで起動（古いセッションをクリアしてから）
# 7. X スクリーンセーバー・DPMS タイマー無効化（xscreensaver 削除後のフリッカ対策）
#
# 依存: keychain, secret-tool, rsync
# 関連: .config/autostart/autostart.desktop, bin/emacs-toggle

pkill ssh-agent
rsync -av --delete ~/Dropbox/backup/mozc/.mozc/ ~/.mozc/
cp -a ~/Dropbox/backup/keyrings/. ~/.local/share/keyrings/
ASKPASS_SCRIPT=$(mktemp /tmp/askpass.XXXXXX.sh)
echo '#!/bin/bash' > "$ASKPASS_SCRIPT"
echo 'secret-tool lookup ssh-key id_rsa' >> "$ASKPASS_SCRIPT"
chmod +x "$ASKPASS_SCRIPT"
DISPLAY=:0 SSH_ASKPASS="$ASKPASS_SCRIPT" SSH_ASKPASS_REQUIRE=force \
	   /usr/bin/keychain --eval --quiet ~/.ssh/id_rsa
rm -f "$ASKPASS_SCRIPT"
source ~/.keychain/$(hostname)-sh

# Emacs を iconic（最初から最小化）状態で起動
emacs --maximized &
until xdotool search --class emacs 2>/dev/null | grep -q .; do sleep 0.5; done
sleep 1s
wid=$(xdotool search --class emacs 2>/dev/null | tail -n1)
xdotool windowminimize "$wid"

# thunderbird &
# sleep 8s
# wid=$(xdotool search --class thunderbird 2>/dev/null | tail -n1)
# [ -n "$wid" ] && xdotool windowminimize "$wid"

# neomutt を tmux セッションで起動
# 再起動時に前回セッションが残っていればクリアしてから起動
tmux kill-session -t mail 2>/dev/null
cd ~/Downloads && tmux new-session -d -s mail 'neomutt'
tmux set -t mail status off

# X スクリーンセーバー・DPMS タイマー無効化
# xscreensaver 削除後に X 本体の timeout（デフォルト600秒）が直接発火しフリッカが起きるため
xset s off
xset dpms 0 0 0
