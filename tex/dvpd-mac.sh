#!/bin/bash

# 生成されたPDFをPreview.appで開く
name=$1

dvipdfmx $1 && evince ${name%.*}.pdf

rm *.aux *.dvi *.log
