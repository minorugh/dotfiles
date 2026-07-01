#!/bin/bash

# 生成されたPDFをevinceで開く
name=$1
dir=$(dirname "$name")
base=${name%.*}

dvipdfmx "$name" && evince "${base}.pdf"

# コンパイル後にページ番号をlastpage.datに記録する(txt2tex.plが次号の開始ページ算出に使う)
# 初回のみ: 空のlastpage.datを手動で設置しておくこと(ファイルが無い場合は何もしない)
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

rm *.au* *.dv* *.lo*
