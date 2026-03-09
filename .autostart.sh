#!/bin/bash
# Reset ssh-agent to ensure clean socket
pkill ssh-agent

if [ ! $(hostname) == "P1" ]; then
    rm -rf $HOME/.mozc
    cp -rf ~/Dropbox/backup/mozc/.mozc ~/
    cp -f ~/Dropbox/backup/shell/.zsh_history ./
    # keyrings は親機のみシンボリックリンク、サブ機は起動時にコピー（競合防止）
    cp ~/Dropbox/backup/keyrings/Default_keyring.keyring \
       ~/.local/share/keyrings/Default_keyring.keyring
fi

# run xmodmap at startup
/usr/bin/zsh -c "sleep 5; /usr/bin/xmodmap $HOME/.Xmodmap"

# SSH key auto-add via SSH_ASKPASS
ASKPASS_SCRIPT=$(mktemp /tmp/askpass.XXXXXX.sh)
echo '#!/bin/bash' > "$ASKPASS_SCRIPT"
echo 'secret-tool lookup ssh-key id_rsa' >> "$ASKPASS_SCRIPT"
chmod +x "$ASKPASS_SCRIPT"
DISPLAY=:0 SSH_ASKPASS="$ASKPASS_SCRIPT" SSH_ASKPASS_REQUIRE=force \
	   /usr/bin/keychain --eval --quiet ~/.ssh/id_rsa
rm -f "$ASKPASS_SCRIPT"

# keychainの環境変数をセッションに反映
source ~/.keychain/$(hostname)-sh
