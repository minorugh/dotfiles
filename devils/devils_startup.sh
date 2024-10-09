#!/bin/bash
devilspie &
sylpheed &
emacs &
rm /home/minoru/src/github.com/minorugh/dotfiles/.emacs.d/session*
sleep 30s
killall devilspie
