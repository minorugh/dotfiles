#!/bin/bash
# dvpd.sh
# LaTeXでコンパイルしたPDFをdvipdfmxで生成し、evinceで開く。
#
# 2026.08.04
# 最終ページ情報を書き出すための処理を追加。

name=$1
base=${name%.*}

dvipdfmx "$name" && evince "${base}.pdf"
rm -f *.au* *.dv* *.lo*

# -------------------------------------------------------------------
# 生成されたPDFの最終ページ情報を lastpage.dat に書き出すための処理
# -------------------------------------------------------------------
# lastpage.dat は txt2tex.pl へ「次号の開始ページ」
# を引き継ぐための橋渡しファイル。
# txt2tex.pl は lastpage.dat から final値を読み込み、
# その値に +1 して次号開始ページ(start_page)として使う。
# 以下のコードは、生成したPDFの総ページ数と開始ページから
# 最終ページ(final)を算出し lastpage.dat に書き出す

dir=$(dirname "$base")

pages=$(pdfinfo "${base}.pdf" 2>/dev/null | awk '/^Pages:/{print $2}')
start_page=$(grep -oP '\\setcounter\{page\}\{\K[0-9]+' "${base}.tex" 2>/dev/null)
filedate=$(basename "$base" | grep -oP '\d{8}')

if [ -n "$pages" ] && [ -n "$start_page" ] && [ -n "$filedate" ] && [ -e "${dir}/lastpage.dat" ]; then
    final=$((start_page + pages - 1))
    {
        echo "date=$filedate"
        echo "start=$start_page"
        echo "final=$final"
    } > "${dir}/lastpage.dat"
fi
