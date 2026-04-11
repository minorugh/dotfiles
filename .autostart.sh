#!/bin/bash
# .autostart.sh
# Created : 2024-10-01
# Updated : 2026-04-11
#
# GUI ログイン時に autostart.desktop 経由で自動実行されるスクリプト。
# 以下の処理を順に行う：
#
# 1. ssh-agent リセット
# 2. mozc・keyring を Dropbox からリストア
# 3. SSH 鍵を keychain + secret-tool で自動入力（パスフレーズ不要）
# 4. keychain の環境変数をセッションに反映
# 5. Emacs を起動して最小化（xdotool）
# 6. Thunderbird を起動して最小化（xdotool）
#
# 依存: keychain, secret-tool, rsync, xdotool
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
emacs &
sleep 5s
wid=$(xdotool search --class emacs 2>/dev/null | tail -n1)
[ -n "$wid" ] && xdotool windowminimize "$wid"
thunderbird &
sleep 8s
wid=$(xdotool search --class thunderbird 2>/dev/null | tail -n1)
[ -n "$wid" ] && xdotool windowminimize "$wid"
