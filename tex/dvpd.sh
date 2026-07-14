#!/bin/bash

# 生成されたPDFをevinceで開く
name=$1
dir=$(dirname "$name")
base=${name%.*}

dvipdfmx "$name" && evince "${base}.pdf"

# プロジェクト固有の後処理があれば呼び出す（存在しなければ何もしない）
hook="${dir}/dvpd.hook.sh"
if [ -e "$hook" ]; then
    source "$hook" "$base"
fi

rm -f *.au* *.dv* *.lo*
