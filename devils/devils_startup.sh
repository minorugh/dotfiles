#!/bin/bash
devilspie &
emacs &
rm /home/minoru/src/github.com/minorugh/dotfiles/.emacs.d/session*
sleep 5s
killall -9 devilspie
