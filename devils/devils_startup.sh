#!/bin/bash
devilspie &
emacs &
thunderbird &
sleep 5s
killall -9 devilspie
