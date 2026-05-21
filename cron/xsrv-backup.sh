#!/bin/bash
#######################################################################
## xsrv-backup.sh
## xserver → ローカルへ rsync + git commit + push（2ドメイン）
##
## ---------------------------------------------------------------
## 【自動化の仕組み】systemd user timer（cronではない）
## ---------------------------------------------------------------
##
## 設定ファイルの場所：
##   ~/.config/systemd/user/xsrv-backup.timer    ← 実行スケジュール
##   ~/.config/systemd/user/xsrv-backup.service  ← 実行内容
##
## xsrv-backup.timer の中身：
##   [Timer]
##   OnCalendar=00,09,12,15,18,21:00:00   ← 実行時刻（毎日この6回）
##   Persistent=true                       ← スリープ等で飛ばした分を
##                                            復帰後に即実行する
##   Unit=xsrv-backup.service
##
## xsrv-backup.service の中身：
##   [Service]
##   ExecStart=/usr/local/bin/xsrv-backup.sh
##   StandardOutput=truncate:/tmp/xsrv-backup.log
##   StandardError=truncate:/tmp/xsrv-backup.log
##   ※ truncate = 毎回ログを上書きする（appendにすると追記になる）
##
## スケジュールを変更したいとき：
##   1. ~/.config/systemd/user/xsrv-backup.timer を編集
##   2. systemctl --user daemon-reload
##   （Makefileの xsrv-reload ターゲットでも可）
##
## OnCalendar の書き方例：
##   毎日9時と21時         → OnCalendar=09,21:00:00
##   毎時0分               → OnCalendar=*:00:00
##   平日9時               → OnCalendar=Mon..Fri 09:00:00
##
## 日常操作（Makefile経由）：
##   make xsrv-status  … タイマーの状態確認
##   make xsrv-stop    … 緊急停止
##   make xsrv-start   … 再開
##   make xsrv-run     … 今すぐ手動実行
##   make xsrv-log     … ログをEmacsで開く
##   make xsrv-reload  … timer編集後のリロード
##
## Persistent=true の副作用について：
##   スリープ/シャットダウン中に実行時刻を過ぎると、
##   復帰直後にネットワーク未接続のまま発火してエラーになる場合がある。
##   → wait_for_network() で対処済み（最大2.5分待機）
##
## ---------------------------------------------------------------

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"

XSRV_HOST="minorugh@sv13268.xserver.jp"
XSRV_SSH="ssh -p 10022"

XSRV_GH_SRC="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html/"
XSRV_GH_DST="$HOME/src/github.com/minorugh/xsrv-GH"

XSRV_MN_SRC="$XSRV_HOST:/home/minorugh/minorugh.com/public_html/"
XSRV_MN_DST="$HOME/src/github.com/minorugh/xsrv-minorugh"

TMPLOG=$(mktemp)
ERRORS=0

# SSHエージェントの設定を読み込む
if [ -f "$HOME/.keychain/$HOSTNAME-sh" ]; then
    source "$HOME/.keychain/$HOSTNAME-sh"
fi

log() {
    echo "${LOG_PREFIX} $1"
}

# ---------------------------------------------------------------
# ネットワーク到達確認（スリープ復帰直後対策）
# Persistent=true により復帰後即発火するが、その時点でネットワークが
# 未接続の場合がある。15秒×10回（最大2.5分）待機して接続を待つ。
# タイムアウトした場合はスクリプト全体を中止する。
# ---------------------------------------------------------------
wait_for_network() {
    local max_attempts=10
    local wait_sec=15
    for i in $(seq 1 $max_attempts); do
        if ping -c 1 -W 3 8.8.8.8 > /dev/null 2>&1; then
            return 0
        fi
        log "ネットワーク未接続、${wait_sec}秒待機... (${i}/${max_attempts})"
        sleep $wait_sec
    done
    log "ネットワーク接続タイムアウト、処理を中止します"
    rm -f "$TMPLOG"
    exit 1
}

run_rsync() {
    local label="$1"
    local src="$2"
    local dst="$3"
    rsync -avz --delete --exclude='.git' --exclude='.gitignore' -e "$XSRV_SSH" "$src" "$dst/" >> "$TMPLOG" 2>&1
    if [ $? -eq 0 ]; then
        log "rsync ${label}: OK"
    else
        log "rsync ${label}: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
    fi
    > "$TMPLOG"
}

run_git() {
    local label="$1"
    local dir="$2"
    cd "$dir" && \
        git add -A >> "$TMPLOG" 2>&1 && \
        git commit -m "auto: $(date '+%Y-%m-%d %H:%M:%S')" >> "$TMPLOG" 2>&1 || echo "No changes" >> "$TMPLOG" 2>&1
    git push >> "$TMPLOG" 2>&1
    if [ $? -eq 0 ]; then
        log "git push ${label}: OK"
    else
        log "git push ${label}: ERROR"
        cat "$TMPLOG"
        ERRORS=$((ERRORS + 1))
    fi
    > "$TMPLOG"
}

START=$(date '+%Y-%m-%d %H:%M:%S')
log "START: ${START}"

wait_for_network

run_rsync "gospel-haiku.com"  "$XSRV_GH_SRC" "$XSRV_GH_DST"
run_git   "gospel-haiku.com"  "$XSRV_GH_DST"

run_rsync "minorugh.com"      "$XSRV_MN_SRC" "$XSRV_MN_DST"
run_git   "minorugh.com"      "$XSRV_MN_DST"

rm -f "$TMPLOG"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
