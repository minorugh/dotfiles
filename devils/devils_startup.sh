#!/bin/bash
devilspie &
sylpheed &
emacs &
sleep 5s
killall -9 devilspie
rm /home/minoru/src/github.com/minorugh/dotfiles/.emacs.d/session*

exit
