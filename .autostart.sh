#!/bin/bash
# See ${PWD}/.config/autostart/autostart.desktop
# Use symbolic link ~/Dropbox/backup/mozc/.mozc for main machine
# Copy ~/Dropbox/backup/mozc/.mozc to the sub machine
# Do this on every startup so .mozc is always up to date

if [ ! $(hostname) == "P1" ]; then
    rm -rf $HOME/.mozc
    cp -rf ~/Dropbox/backup/mozc/.mozc ~/
    cp -f ~/Dropbox/backup/shell/.zsh_history ./
fi

# run xmodmap at startup
/usr/bin/zsh -c "sleep 5; /usr/bin/xmodmap $HOME/.Xmodmap"

exit
