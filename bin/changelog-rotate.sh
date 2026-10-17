#!/bin/bash
set -eu

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/src/github.com/minorugh/dotfiles}"
README="$DOTFILES_DIR/README.md"
CHANGELOG="$DOTFILES_DIR/CHANGELOG.md"
KEEP=10
OLD_FOOTER='これ以前の履歴は [CHANGELOG.md](CHANGELOG.md) を参照してください。'
FOOTER='全履歴は [CHANGELOG.md](CHANGELOG.md) を参照してください。'

warn() {
    echo "changelog-rotate: $*" >&2
    exit 0
}

[ -f "$README" ] || warn "README.md が見つかりません"

start=$(grep -n '^## 更新履歴' "$README" | head -1 | cut -d: -f1)
[ -n "$start" ] || warn "「## 更新履歴」の節が見つかりません"

sed -n "$((start+1)),$((start+4))p" "$README" | grep -qF '| 日付 | 内容 |' \
    || warn "表の見出し行が想定と違うため中止します"

mapfile -t rows < <(awk -v s="$start" 'NR>s && /^\| 20/ {print NR}' "$README")
total=${#rows[@]}
[ "$total" -gt 0 ] || exit 0

first=${rows[0]}
last=${rows[$((total-1))]}
[ $((last-first+1)) -eq "$total" ] || warn "更新履歴の行が連続していないため中止します"

if [ ! -f "$CHANGELOG" ]; then
    {
        printf '# CHANGELOG\n\n'
        printf '全履歴です（新しい順）。README.md の更新履歴には直近 %s 件だけを載せています。\n\n' "$KEEP"
        printf '| 日付 | 内容 |\n|---|---|\n'
    } > "$CHANGELOG"
fi
grep -q '^|---|---|' "$CHANGELOG" || warn "CHANGELOG.md の表の区切り行が見つかりません"

new=$(mktemp)
out=$(mktemp)
trap 'rm -f "$new" "$out"' EXIT

while IFS= read -r line; do
    grep -qxF -- "$line" "$CHANGELOG" || printf '%s\n' "$line" >> "$new"
done < <(sed -n "${first},${last}p" "$README")

added=0
if [ -s "$new" ]; then
    added=$(wc -l < "$new")
    awk -v f="$new" '{print} !d && /^\|---\|---\|/ {while ((getline l < f) > 0) print l; d=1}' "$CHANGELOG" > "$out"
    cat "$out" > "$CHANGELOG"
fi

lastrow=$last
trimmed=0
if [ "$total" -gt "$KEEP" ]; then
    keep_last=${rows[$((KEEP-1))]}
    awk -v a="$((keep_last+1))" -v b="$last" 'NR>=a && NR<=b {next} {print}' "$README" > "$out"
    cat "$out" > "$README"
    lastrow=$keep_last
    trimmed=$((total-KEEP))
fi

if grep -qxF -- "$FOOTER" "$README"; then
    :
elif grep -qxF -- "$OLD_FOOTER" "$README"; then
    awk -v o="$OLD_FOOTER" -v n="$FOOTER" '$0==o {print n; next} {print}' "$README" > "$out"
    cat "$out" > "$README"
else
    awk -v r="$lastrow" -v ft="$FOOTER" 'NR==r {print; print ""; print ft; next} {print}' "$README" > "$out"
    cat "$out" > "$README"
fi

if [ "$added" -gt 0 ] || [ "$trimmed" -gt 0 ]; then
    echo "changelog-rotate: CHANGELOG.md へ ${added} 件追加、README.md から ${trimmed} 件削除"
fi
