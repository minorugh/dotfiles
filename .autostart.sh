#!/bin/bash
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
