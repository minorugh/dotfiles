#!/bin/bash
# dvpd.sh
# LaTeXでコンパイルしたPDFをdvipdfmxで生成し、evinceで開く。
#
# 2026.08.04
# 最終ページ情報を書き出すための処理 update_lastpage() 関数を追加。

name=$1
base=${name%.*}

dvipdfmx "$name" && evince "${base}.pdf"

# ---------------------------------------------------------
# 環境固有の後処理関数
#
# 生成したPDFの総ページ数とtex側で指定した開始ページ番号から
# 最終ページ番号を算出し lastpage.dat に書き出す。
# この機能が必要無い場合はここの関数コード部分を削除してください。

update_lastpage() {
    # プロジェクトディレクトリに lastpage.dat が既に存在すること。
    # tex側で \setcounter{page}{N} により開始ページを指定していること。
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
