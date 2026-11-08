#!/bin/sh
win=$(xdotool getactivewindow)
case "$1" in
commit) emacsclient -e "(my-commit-file-from-tig \"$PWD/$2\")" > /dev/null ;;
all) emacsclient -e "(my-commit-all-from-tig \"$PWD\")" > /dev/null ;;
push) GIT_PAGER=cat make git || { printf '\nEnter で tig に戻る '; read x; }; exit ;;
esac
while emacsclient -e '(get-buffer "*commit-msg*")' | grep -q buffer; do sleep 0.25; done
xdotool windowactivate "$win"
