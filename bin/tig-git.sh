#!/bin/sh
win=$(xdotool getactivewindow)
case "$1" in
commit) emacsclient -e "(my-commit-file-from-tig \"$PWD/$2\")" ;;
all) emacsclient -e "(my-commit-all-from-tig \"$PWD\")" ;;
push) make git; printf '\nEnter で tig に戻る '; read x; exit ;;
esac
while emacsclient -e '(get-buffer "*commit-msg*")' | grep -q buffer; do sleep 0.25; done
xdotool windowactivate "$win"
