#!/bin/bash
set -eu

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/src/github.com/minorugh/dotfiles}"
README="$DOTFILES_DIR/README.md"
CHANGELOG="$DOTFILES_DIR/CHANGELOG.md"
KEEP=10
FOOTER='これ以前の履歴は [CHANGELOG.md](CHANGELOG.md) を参照してください。'

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
[ "$total" -gt "$KEEP" ] || exit 0

keep_last=${rows[$((KEEP-1))]}
first=${rows[$KEEP]}
last=${rows[$((total-1))]}

[ $((first-keep_last)) -eq 1 ] && [ $((last-first+1)) -eq $((total-KEEP)) ] \
    || warn "更新履歴の行が連続していないため中止します"

moved=$(mktemp)
out=$(mktemp)
trap 'rm -f "$moved" "$out"' EXIT

sed -n "${first},${last}p" "$README" > "$moved"

if [ -f "$CHANGELOG" ]; then
    grep -q '^|---|---|' "$CHANGELOG" || warn "CHANGELOG.md の表の区切り行が見つかりません"
    awk -v f="$moved" '{print} !d && /^\|---\|---\|/ {while ((getline l < f) > 0) print l; d=1}' "$CHANGELOG" > "$out"
else
    {
        printf '# CHANGELOG\n\n'
        printf 'README.md の更新履歴から移した過去の履歴です（新しい順）。\n\n'
        printf '| 日付 | 内容 |\n|---|---|\n'
        cat "$moved"
    } > "$out"
fi
cat "$out" > "$CHANGELOG"

has=0
grep -qF 'CHANGELOG.md' "$README" && has=1

awk -v f="$first" -v l="$last" -v k="$keep_last" -v ft="$FOOTER" -v has="$has" '
    NR>=f && NR<=l {next}
    {print}
    NR==k && has==0 {print ""; print ft}
' "$README" > "$out"
cat "$out" > "$README"

echo "changelog-rotate: $((total-KEEP)) 件を CHANGELOG.md へ移しました"
