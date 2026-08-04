#!/bin/bash
# dvpd.sh
# LaTeXでコンパイルしたPDFをdvipdfmxで生成し、evinceで開く。
#
# 【カスタマイズについて】
# 生成後の後処理は update_lastpage() 関数にまとめてあります。
# ご自身の運用に合わせて自由に書き換え・削除してください。
# 不要な場合は下の呼び出し行（update_lastpage "$base"）を削除するか
# コメントアウトすれば、単なるPDFビューア起動スクリプトとして使えます。

name=$1
base=${name%.*}

dvipdfmx "$name" && evince "${base}.pdf"

# ---------------------------------------------------------
# 環境固有の後処理（必要な場合のみ編集してください）
# ---------------------------------------------------------
update_lastpage() {
    # 用途: 生成したPDFの総ページ数と、tex側で指定した開始ページ番号から
    #       最終ページ番号を算出し、lastpage.dat に書き出す。
    # 前提: プロジェクトディレクトリに lastpage.dat が既に存在すること。
    #       tex側で \setcounter{page}{N} により開始ページを指定していること。
    local base=$1
    local dir
    dir=$(dirname "$base")

    local pages start_page filedate
    pages=$(pdfinfo "${base}.pdf" 2>/dev/null | awk '/^Pages:/{print $2}')
    start_page=$(grep -oP '\\setcounter\{page\}\{\K[0-9]+' "${base}.tex" 2>/dev/null)
    filedate=$(basename "$base" | grep -oP '\d{8}')

    if [ -n "$pages" ] && [ -n "$start_page" ] && [ -n "$filedate" ] && [ -e "${dir}/lastpage.dat" ]; then
        local final=$((start_page + pages - 1))
        {
            echo "date=$filedate"
            echo "start=$start_page"
            echo "final=$final"
        } > "${dir}/lastpage.dat"
    fi
}

update_lastpage "$base"
# ---------------------------------------------------------

rm -f *.au* *.dv* *.lo*
