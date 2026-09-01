#!/bin/bash
set -euo pipefail

latest_file=$(curl -sL https://ftpmirror.gnu.org/emacs/ | grep -oE 'emacs-[0-9]+\.[0-9]+(\.[0-9]+)?\.tar\.gz' | sort -V | tail -1)

if [ -z "${latest_file}" ]; then
  echo "##> Emacs最新版の情報が取得できませんでした（ネット接続を確認してください）。"
  exit 1
fi

latest_version=$(echo "${latest_file}" | sed -E 's/emacs-(.*)\.tar\.gz/\1/')
echo "##> 最新の安定版は emacs-${latest_version} です。"
