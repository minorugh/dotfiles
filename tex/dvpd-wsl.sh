#!/bin/bash

# 生成されたPDFをPreview.appで開く
name=$1

dvipdfmx ${name%.*} && wslstart ${name%.*}.pdf

# 不要ファイルを削除
rm *.au*
rm *.dv*
rm *.lo*
