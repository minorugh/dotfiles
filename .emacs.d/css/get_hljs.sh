#!/bin/bash
# highlight.js をローカルにダウンロードするスクリプト
# 実行: bash get_hljs.sh

DEST="$HOME/.emacs.d/elisp"

echo "highlight.min.js をダウンロード中..."
curl -o "$DEST/highlight.min.js" \
  "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js"

echo "highlight テーマCSS (github-dark) をダウンロード中..."
curl -o "$DEST/highlight.min.css" \
  "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github-dark.min.css"

echo "完了: $DEST/highlight.min.js"
echo "完了: $DEST/highlight.min.css"
