#!/bin/bash
# /usr/local/bin/emacs-start.sh
if pgrep -x emacs >/dev/null; then
    wid=$(xdotool search --class emacs 2>/dev/null | tail -n1)
    [ -n "$wid" ] && xdotool windowmap --sync "$wid" \
        windowraise "$wid" windowfocus --sync "$wid" windowactivate --sync "$wid"
else
    exec zsh -lc "emacs --maximized"
fi
