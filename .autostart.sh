#!/bin/bash
# See ${PWD}/.config/autostart/autostart.desktop
# Use symbolic link ~/Dropbox/backup/mozc/.mozc for main machine
# Copy ~/Dropbox/backup/mozc/.mozc to the sub machine
# Do this on every startup so .mozc is always up to date

# Reset ssh-agent to ensure clean socket
pkill ssh-agent
eval $(ssh-agent -s)

if [ ! $(hostname) == "P1" ]; then
    rm -rf $HOME/.mozc
    cp -rf ~/Dropbox/backup/mozc/.mozc ~/
    cp -f ~/Dropbox/backup/shell/.zsh_history ./
fi

# run xmodmap at startup
/usr/bin/zsh -c "sleep 5; /usr/bin/xmodmap $HOME/.Xmodmap"

# SSH key auto-add
# パスフレーズはGNOME Keyring（secret-tool）で管理。
# ローカル保存のためGitHubには同期されない。
# 新しいマシンでは以下のコマンドで初回登録が必要：
#   secret-tool store --label="SSH key passphrase" service ssh-key account id_rsa
# autologin.sh / autologin.desktop は廃止してこのブロックに統合済み（2025年）
/usr/bin/expect -c "
set PW [exec secret-tool lookup service ssh-key account id_rsa]
spawn /usr/bin/keychain --eval --quiet /home/minoru/.ssh/id_rsa
expect {
    \"Enter passphrase\" { send \"\$PW\n\"; interact }
    eof {}
}
"

# keychainの環境変数をセッションに反映
source ~/.keychain/$(hostname)-sh
