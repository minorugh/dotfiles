#!/bin/bash

# 生成されたPDFをWindowsの拡張子連動アプリで開く
name=$1

dvipdfmx ${name%.*} && wslstart ${name%.*}.pdf

# 不要ファイルを削除
rm *.au*
rm *.dv*
rm *.lo*
