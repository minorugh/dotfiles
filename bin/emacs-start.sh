#!/bin/bash
# /usr/local/bin/emacs-start.sh
# Updated : 2026-06-25

# keychain の SSH agent 環境変数を明示的に読み込む
[ -f "$HOME/.keychain/$(hostname)-sh" ] && source "$HOME/.keychain/$(hostname)-sh"

exec zsh -lc "/usr/local/bin/emacs --maximized"
