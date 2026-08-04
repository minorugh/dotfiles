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

# ---------------------------------------------------------
# 最終ページ情報を lastpage.dat に書き出す
# ---------------------------------------------------------
# 生成したPDFの総ページ数とtex側で指定した開始ページ番号から
# 最終ページ番号を算出し lastpage.dat に書き出す。
# プロジェクトディレクトリに lastpage.dat が存在すること。
# tex側で \setcounter{page}{N} により開始ページを指定していること。
# 機能が必要無い場合はこのブロックを削除してください。
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
