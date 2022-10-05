#!/bin/bash
devilspie &
emacs &
sylpheed &
sleep 5s
killall -9 devilspie
