#!/bin/bash
devilspie &
emacs &
sleep 5s
killall -9 devilspie
