#!/bin/bash
# makefile-check.sh
# Created : 2026-11-06
#
# Makefile を点検する保守用スクリプト。実行はせず、読み取りだけを行う。
#
# 【Makefile の構文】
#  1. make -n baseinstall nextinstall を、今のホストと X250 で実行する
#  2. 未定義の変数を探す
#  3. 行末の \ のあとの空白と、行頭が空白のインデントを探す
#  4. 同じターゲットの二重定義を探す
#  5. 全ターゲットのレシピを bash -n に通す
#  6. ## の説明がないターゲットを探す
# 【パッケージ】
#  7. 複数の場所で入れているパッケージを表示する
#  8. PACKAGES・BASE_PKGS の名前と同じターゲットがあるものを表示する
#  9. BASE_PKGS と PACKAGES の入れ分けが怪しいものを表示する
# 10. apt-cache madison で、リポジトリにない名前を表示する
# 11. -c [版]: debian のコンテナで apt-get -s install を実行する
#
# 使い方: bin/makefile-check.sh [-c [版]]
# 依存: make, awk, apt-cache, docker（-c のとき）
# 関連: Makefile の PACKAGES, BASE_PKGS, 各ターゲットの $(APT) の行
#
# docker-install は外部リポジトリを追加してから入れるため、パッケージの点検の対象外にしている。

cd "$(dirname "$0")/.."

WORK=$(mktemp -d)

echo "########## Makefile の構文 ##########"

echo
echo "== 1. make -n（構文エラー・存在しないターゲット。今のホストと X250 の2回実行するため、エラーは2回出る）"
make -n baseinstall nextinstall > /dev/null && echo "$(hostname): OK"
make -n baseinstall nextinstall HOSTNAME=X250 > /dev/null && echo "X250: OK"

echo
echo "== 2. 未定義の変数"
UNDEF=$(make -n --warn-undefined-variables baseinstall nextinstall 2>&1 > /dev/null | grep -i 'undefined' | sort -u)
echo "${UNDEF:-なし}"

echo
echo "== 3. 行末の \\ のあとの空白・行頭が空白のインデント"
WS=$(grep -nE '\\[[:space:]]+$' Makefile; grep -nP '^ +\S' Makefile)
echo "${WS:-なし}"

echo
echo "== 4. 同じターゲットの二重定義"
OVR=$(make -n baseinstall nextinstall 2>&1 > /dev/null | grep -E 'overriding|ignoring old')
echo "${OVR:-なし}"

echo
echo "== 5. 全ターゲットのレシピのシェル構文（bash -n）"
if make -n help > /dev/null 2>&1; then
    for t in $(grep -oE '^[a-zA-Z_-]+:' Makefile | cut -d: -f1 | sort -u); do
        make -n ${t} > ${WORK}/recipe 2> /dev/null || { echo "make -n が失敗: ${t}"; continue; }
        bash -n ${WORK}/recipe || echo "↑ シェル構文エラー: ${t}"
    done
    echo "（上に何も出なければ、エラーなし）"
else
    echo "Makefile が読めないため省略（1 のエラーを先に直す）"
fi

echo
echo "== 6. ## の説明がないターゲット"
NODESC=$(grep -E '^[a-zA-Z_-]+:' Makefile | grep -v '##' | grep -vE '^(help|all|baseinstall|nextinstall):')
echo "${NODESC:-なし}"

echo
echo "########## パッケージ ##########"

printf 'pp-%%:\n\t@echo $($*)\n' > ${WORK}/pp.mk
make -s -f Makefile -f ${WORK}/pp.mk pp-PACKAGES | tr ' ' '\n' | grep -v '^$' > ${WORK}/packages
make -s -f Makefile -f ${WORK}/pp.mk pp-BASE_PKGS | tr ' ' '\n' | grep -v '^$' > ${WORK}/base

awk '/^[a-zA-Z_-]+:/{t=$1; sub(/:.*/,"",t)} /\$\(APT\)|apt install -y/ && !/\.deb/ && !/\$\(PACKAGES\)|\$\(BASE_PKGS\)/ {l=$0; sub(/.*\$\(APT\)[ ]*/,"",l); sub(/.*apt install -y[ ]*/,"",l); gsub(/\$@/,t,l); n=split(l,a," "); for(i=1;i<=n;i++) print a[i] "\t" t}' Makefile | awk -F'\t' '$2 != "docker-install"' > ${WORK}/targets

{ sed 's/$/\tPACKAGES/' ${WORK}/packages; sed 's/$/\tBASE_PKGS/' ${WORK}/base; cat ${WORK}/targets; } > ${WORK}/all

echo
echo "== 7. 重複（複数の場所で入れているパッケージ）"
DUP=$(sort ${WORK}/all | awk -F'\t' '{c[$1]=c[$1]" "$2; n[$1]++} END{for(k in n) if(n[k]>1) print k":"c[k]}' | sort)
echo "${DUP:-なし}"

echo
echo "== 8. PACKAGES・BASE_PKGS の名前と同じターゲットがあるもの"
SAME=$(for p in $(cat ${WORK}/packages ${WORK}/base); do grep -qE "^${p}:" Makefile && echo ${p}; done)
echo "${SAME:-なし}"

echo
echo "== 9. 分類の見直し（直すものだけ表示）"
MOVE=$(
    grep -vE '^lib|-dev$' ${WORK}/base | grep -vxE 'automake|autoconf|openssl|build-essential|python3-pip' | sed 's/$/: PACKAGES へ移動/'
    grep -E '^gcc$|^make$' ${WORK}/packages | sed 's/$/: PACKAGES から削除（build-essential が入れる）/'
    grep -E '^lib|-dev$|^build-essential$' ${WORK}/packages | sed 's/$/: BASE_PKGS へ移動/'
)
echo "${MOVE:-なし}"

echo
echo "== 10. リポジトリにない名前（apt-cache madison）"
MISSING=$(for p in $(cut -f1 ${WORK}/all | sort -u); do [ -z "$(apt-cache madison ${p})" ] && echo ${p}; done)
echo "${MISSING:-なし}"

if [ "$1" = "-c" ]; then
    VER=${2:-$(. /etc/os-release; echo ${VERSION_ID})}
    echo
    echo "== 11. debian:${VER} の空のコンテナで apt-get -s install"
    docker run --rm debian:${VER} bash -c "apt-get update -qq && apt-get -s install -y $(cut -f1 ${WORK}/all | sort -u | tr '\n' ' ')" | grep -E '^E:|newly installed'
fi

rm -rf ${WORK}
