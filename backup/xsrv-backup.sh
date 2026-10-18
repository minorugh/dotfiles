#!/bin/bash
#######################################################################
## xsrv-backup.sh
## xserver → Dropbox/GH へ動的ファイルを rsync（毎時）
# cron で 0:00 & 07:00〜23:00 毎時実行
## git push は autobackup.sh（毎晩）に委譲
##
## 【注意】~/xsrv-rsync.lock が存在する間はスキップする
## ローカルで動的ファイルを編集・deploy する際は lock を発行すること
## （Emacs の 60-xsrv-dired.el で自動連携）
##
## 【追加】*.dat は Dropbox の Web でプレビューできないため、
## Dropbox/backup/xsrv/ に *.dat.txt のコピーを作る（内容が変わった時だけ）。
## 対象は d_kukai / m_kukai / s_kukai / w_kukai の data/ 内の *.dat のみ。
## 階層を浅くするため d_kukai/data/ → d_kukai.data/ のように保存する。
## コピーは Dropbox/GH の git の外にあるので、履歴には影響しない。
#######################################################################

HOME=/home/minoru
LOG_PREFIX="[xsrv-backup]"
LOGFILE=/tmp/xsrv-backup.log
LOCKFILE="$HOME/xsrv-rsync.lock"
XSRV_SSH="ssh -p 10022"
XSRV_HOST="xsrv"
XSRV_BASE="$XSRV_HOST:/home/minorugh/gospel-haiku.com/public_html"
DST="$HOME/Dropbox/GH"
PREVIEW="$HOME/Dropbox/backup/xsrv"
## *.dat が UTF-8 / ASCII 以外の場合の元の文字コード（例: CP932 / EUC-JP）
## 空のままなら変換せずそのままコピーする
LEGACY_ENC=""
ERRORS=0


log() { echo "${LOG_PREFIX} $1" | tee -a "$LOGFILE"; }

# ログファイルをリセット
echo "" > "$LOGFILE"
log "START: $(date '+%Y-%m-%d %H:%M:%S')"

# ロックチェック
if [ -f "$LOCKFILE" ]; then
    log "ロックファイルあり、スキップします"
    log "END: $(date '+%Y-%m-%d %H:%M:%S')"
    exit 0
fi

# 緊急停止チェック（make cron-stop で発行、make cron-start で解除）
[ -f "$HOME/.xsrv-backup-stop" ] && exit 0

run_rsync() {
    local label="$1"
    local src="$2"
    local dst="$3"
    rsync -az --mkpath -e "$XSRV_SSH" "$src" "$dst" >> "$LOGFILE" 2>&1
    if [ $? -eq 0 ]; then
        log "rsync ${label}: OK"
    else
        log "rsync ${label}: ERROR"
        ERRORS=$((ERRORS + 1))
    fi
}

# フォルダー単位
for dir in \
    danwa/data \
    danwa/html \
    dia/divoice \
    d_kukai/data \
    d_kukai/score \
    m_kukai/data \
    m_kukai/score \
    s_kukai/data \
    s_kukai/score \
    w_kukai/data \
    w_kukai/score; do
    run_rsync "$dir" "$XSRV_BASE/${dir}/" "$DST/${dir}/"
done

# 特定ファイル単位
for file in \
    d_kukai/_datefrag.dat \
    m_kukai/_progfrag.dat \
    s_kukai/_progfrag.dat \
    w_kukai/_progfrag.dat; do
    run_rsync "$file" "$XSRV_BASE/${file}" "$DST/${file}"
done

# *.dat のプレビュー用コピーを作る（失敗しても本体の ERRORS には数えない）
make_preview() {
    local f dir out enc tmp updated=0
    while IFS= read -r -d '' f; do
        dir=$(dirname "$f")
        out="$PREVIEW/${dir//\//.}/$(basename "$f").txt"
        mkdir -p "$(dirname "$out")"
        tmp=$(mktemp)
        enc=$(file -b --mime-encoding "$DST/$f")
        if [ -n "$LEGACY_ENC" ] && [ "$enc" != "us-ascii" ] && [ "$enc" != "utf-8" ]; then
            iconv -f "$LEGACY_ENC" -t UTF-8 "$DST/$f" > "$tmp" 2>/dev/null || cp "$DST/$f" "$tmp"
        else
            cp "$DST/$f" "$tmp"
        fi
        if ! cmp -s "$tmp" "$out"; then
            cp "$tmp" "$out" && updated=$((updated + 1))
        fi
        rm -f "$tmp"
    done < <(cd "$DST" && find \
        d_kukai/data m_kukai/data s_kukai/data w_kukai/data \
        -type f -size +0 -name '*.dat' -print0 2>/dev/null)
    log "preview: ${updated} updated"
}

if [ $ERRORS -eq 0 ]; then
    make_preview
fi

# make -C "$DST" git >> /dev/null 2>&1 && log "git push: OK" || log "git push: ERROR"

END=$(date '+%Y-%m-%d %H:%M:%S')
if [ $ERRORS -eq 0 ]; then
    log "END: ${END} (OK)"
else
    log "END: ${END} (ERRORS=${ERRORS})"
fi
