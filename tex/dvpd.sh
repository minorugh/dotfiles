#!/bin/bash

# 生成されたPDFをevinceで開く
name=$1

dvipdfmx $1 && evince ${name%.*}.pdf

rm *.au* *.dv* *.lo*
