#!/bin/bash
devilspie &
emacs &
sylpheed &
sleep 3s
killall -9 devilspie
