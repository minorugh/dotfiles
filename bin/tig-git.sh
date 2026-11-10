#!/bin/sh
#
# tig-git.sh - tig の status・stage 画面から commit と push を行う
#
# config の C・A・P から > で呼ばれる。
#   tig-git.sh commit FILE   カーソル位置のファイルだけ commit（C）
#   tig-git.sh all           全変更を commit（A）
#   tig-git.sh push          make git で push（P）
#
# > は、コマンド終了後に tig を再オープンする。そのとき status が最新になる。
# tig は外部コマンドをリポジトリのルートで実行するので、$PWD はルートになる。
# 前提: X11 環境と xdotool（commit のあと端末ウィンドウを前面に戻すために使う）。

# 開始時の端末ウィンドウを控える（commit のあと前面へ戻すため）
win=$(xdotool getactivewindow)

case "$1" in
# 02-git.el の関数が *commit-msg* を開き、Emacs を前面に出す
commit) emacsclient -e "(my-commit-file-from-tig \"$PWD/$2\")" > /dev/null ;;
all) emacsclient -e "(my-commit-all-from-tig \"$PWD\")" > /dev/null ;;
# 成功したら止めず、失敗したときだけ出力を残して Enter 待ちにする。
# GIT_PAGER=cat は、git のページャで止まるのを避ける。Emacs は経由しない
push)
  if [ -n "$(git status --porcelain)" ]; then
    echo "未コミットの変更があります。先に commit してください" | tig; exit
  fi
  GIT_PAGER=cat make git || { printf '\nEnter で tig に戻る '; read x; }; exit ;;
unpushed)
  if [ "$(git rev-list --count origin/main..HEAD)" = 0 ]; then
    echo "未 push のコミットはありません" | tig; exit
  fi
  tig origin/main..HEAD; exit ;;
reword)
  if git merge-base --is-ancestor HEAD @{upstream}; then
    echo "直前のコミットは push 済みです" | tig; exit
  fi
  emacsclient -e "(my-commit-reword-from-tig \"$PWD\")" > /dev/null ;;
help)
  cat <<'HELP' | tig
 E  選択コミットを git-peek で開く
 C  このファイルだけ commit
 A  全変更を commit
 P  push
 U  未 push のコミット一覧
 R  直前コミットの reword（push 済みは不可）
 h  このヘルプ
 H  tig 標準のヘルプ
HELP
  exit ;;
esac

# *commit-msg* が閉じるまで待つ（C-c C-c の確定も C-c C-k のキャンセルも同じ）。
# 変更がなくて開かなかったときは、すぐ抜ける
while emacsclient -e '(get-buffer "*commit-msg*")' | grep -q buffer; do sleep 0.25; done

# 控えておいた端末ウィンドウを前面に戻す
xdotool windowactivate "$win"
