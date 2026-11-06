#!/bin/sh
#
# Simplenote launcher
#   no argument: open in app mode, maximized on the monitor of the active window
#   copy       : copy the active note, return to Emacs, then close the window
#

if [ "$1" = copy ]; then
    w=$(xdotool search --name '^Simplenote' | tail -n1)
    xdotool windowactivate --sync $w
    sleep 0.3
    xdotool key ctrl+a ctrl+c
    sleep 0.3
    xclip -selection clipboard -t UTF8_STRING -o | xclip -selection clipboard -t UTF8_STRING -i
    emacs-toggle
    wmctrl -i -c $w
    exit
fi

eval $(xdotool getactivewindow getwindowgeometry --shell)
chromium --app="https://app.simplenote.com/" &
w=$(xdotool search --sync --name '^Simplenote' | tail -n1)
sleep 0.5
xdotool windowmove $w $X $Y
wmctrl -i -r $w -b add,maximized_vert,maximized_horz
