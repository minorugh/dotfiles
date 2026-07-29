#!/bin/bash
# make-run.sh <dir> <target...>
#
# Makefile の対象ターゲットが ##! 付きかどうかを見て、
# TTY を持たない呼び出し(Emacsのcompileバッファ等)からの実行だけ
# gnome-terminal に委譲する。ターミナルから直接呼んだ場合はそのまま実行。
#
set -eu

DIR="$1"; shift
TARGET="$*"
MAKEFILE="$DIR/Makefile"
FIRST_WORD="${TARGET%% *}"

is_interactive() {
    grep -qE "^${FIRST_WORD}:.*##!" "$MAKEFILE" 2>/dev/null
}

if is_interactive && [ ! -t 1 ]; then
    EMACS_WID=$(xdotool getactivewindow 2>/dev/null || true)
    gnome-terminal -- bash -c "
        make -C '$DIR' $TARGET
        echo
        read -n1 -r -p '[Enterで閉じる]'
        ${EMACS_WID:+xdotool windowactivate $EMACS_WID}
    "
else
    exec make -C "$DIR" $TARGET
fi
