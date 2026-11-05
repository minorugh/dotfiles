#!/bin/bash
# /usr/local/bin/emacs-start.sh
# Updated : 2026-11-05
# 使われている場所:
# - .autostart.sh: ログイン時に Emacs を起動する
# - emacsclient.desktop: emacsclient の --alternate-editor。サーバーがないとき起動する
# - bin/emacs-toggle: ウィンドウがなければ起動する
# - .emacs.d/inits/80-hydra-navication.el の my-restart-emacs: 終了後に起動し直す
# - bin/power-menu.sh: 実行中の Emacs プロセスを探す条件に名前を使う
# - Makefile の autostart: make emacs-start で /usr/local/bin にリンクを張る
# 詳細は bin/README.md を参照

exec zsh -ic "/usr/local/bin/emacs --maximized"
