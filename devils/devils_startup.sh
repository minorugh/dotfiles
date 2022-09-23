#!/bin/bash
devilspie &
sylpheed &
emacs &
sleep 5s
killall -9 devilspie
