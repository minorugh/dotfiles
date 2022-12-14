#!/bin/bash
devilspie &
emacs &
sylpheed &
sleep 1s
killall -9 devilspie
