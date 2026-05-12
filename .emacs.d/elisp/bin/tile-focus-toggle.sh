#!/bin/bash
# tile-focus-toggle.sh
ACTIVE=$(xdotool getactivewindow)
CID=$(wmctrl -l | grep -i "simplenote" | tail -n 1 | awk '{print $1}')
EID=$(wmctrl -lx | grep -i "emacs" | tail -n 1 | awk '{print $1}')

if [ "$ACTIVE" = "$((CID))" ]; then
    wmctrl -i -a "$EID"
else
    wmctrl -i -a "$CID"
fi
