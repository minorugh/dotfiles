#!/bin/bash

# 生成されたPDFをevinceで開く
name=$1
dir=$(dirname "$name")
base=${name%.*}

dvipdfmx "$name" && evince "${base}.pdf"

# 最終ページ数をlastpage.datに記録(rmで中間ファイルを消す前に確保)
pages=$(pdfinfo "${base}.pdf" 2>/dev/null | awk '/^Pages:/{print $2}')
if [ -n "$pages" ]; then
    echo "$pages" > "${dir}/lastpage.dat"
fi

rm *.au* *.dv* *.lo*
