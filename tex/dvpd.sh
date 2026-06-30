#!/bin/bash

# 生成されたPDFをevinceで開く
name=$1
dir=$(dirname "$name")
base=${name%.*}

dvipdfmx "$name" && evince "${base}.pdf"

# 最終ページをlastpage.datに記録(rmで中間ファイルを消す前に確保)
# lastpage.datへの書き込みはこのdvpd.shだけが行う(txt2tex.plは読むだけ)。
# 開始ページはtxt2tex.plが書き込んだ.texの\setcounter{page}{N}から読み取り、
# pdfinfoで物理ページ数を取得して 最終ページ=開始+物理ページ数-1 を計算する。
pages=$(pdfinfo "${base}.pdf" 2>/dev/null | awk '/^Pages:/{print $2}')
start_page=$(grep -oP '\\setcounter\{page\}\{\K[0-9]+' "${base}.tex" 2>/dev/null)
filedate=$(basename "$base" | grep -oP '\d{8}')

# 【初回セットアップ】d_/w_を新しく使い始める際は、最初に手動でlastpage.datを1回作っておく
# (このファイルが存在しない間は、下記の処理は一切書き込みを行わない)
# 例:以下の書式だが空ファイルで実行したあとで修正しても良い。
#   date=20260622
#   start=24
#   final=24
if [ -n "$pages" ] && [ -n "$start_page" ] && [ -n "$filedate" ] && [ -e "${dir}/lastpage.dat" ]; then
    final=$((start_page + pages - 1))
    {
        echo "date=$filedate"
        echo "start=$start_page"
        echo "final=$final"
    } > "${dir}/lastpage.dat"
fi

rm *.au* *.dv* *.lo*
