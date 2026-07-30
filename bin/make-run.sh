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

from_emacs() {
    [ -n "${INSIDE_EMACS:-}" ]
}

if is_interactive && from_emacs; then
    EMACS_WID=$(xdotool getactivewindow 2>/dev/null || true)
    LOGFILE=$(mktemp /tmp/make-run.XXXXXX.log)
    STATUSFILE="$LOGFILE.exit"

    # --wait で gnome-terminal 側の bash が終わるまでここをブロックする。
    # 終了コードは PIPESTATUS 経由でファイルへ書き出し、後で回収する
    # (gnome-terminal 自体の戻り値は make の終了コードと一致しない場合があるため)。
    gnome-terminal --wait -- bash -c "
        set -o pipefail
        {
            echo \"# make -C $DIR $TARGET\"
            echo \"# started:  \$(date '+%Y-%m-%d %H:%M:%S')\"
            echo
            make -C '$DIR' $TARGET
        } 2>&1 | tee '$LOGFILE'
        STATUS=\${PIPESTATUS[0]}
        {
            echo
            echo \"# finished: \$(date '+%Y-%m-%d %H:%M:%S')\"
            if [ \"\$STATUS\" -eq 0 ]; then
                echo '# ##> Compile successful.'
            else
                echo \"# ##> exited abnormally with code \$STATUS\"
            fi
        } >> '$LOGFILE'
        echo \"\$STATUS\" > '$STATUSFILE'
    "
    STATUS=$(cat "$STATUSFILE" 2>/dev/null || echo 1)
    rm -f "$STATUSFILE"

    # 確認待ちはせず、ログを Emacs 側の compilation-mode バッファへ流し込む
    emacsclient -e "(my-make-show-log \"$LOGFILE\" $STATUS)" >/dev/null 2>&1 || true

    ${EMACS_WID:+xdotool windowactivate "$EMACS_WID"}
    exit "$STATUS"
else
    exec make -C "$DIR" $TARGET
fi
