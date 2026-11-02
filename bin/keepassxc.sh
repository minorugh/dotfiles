#!/bin/bash
# KeePassXC 自動起動ラッパー（キーリングのマスターパスワードで解錠して起動する）
#
# キーリングのパスワードで kdbx が開けないときだけ、更新を案内する。
#   P1       : 更新ダイアログを出し、新パスワードで kdbx が開けたときだけ登録する。
#              登録後は keyring-backup で Dropbox に反映する。
#   P1以外   : 書き込みはしない。make keyring で P1 の状態を戻す案内だけを出す。
# マスターパスワードの変更は P1 でのみ行うこと。
# メイン機を変えるときは MAIN_HOSTNAME も書き換えること。
# キーリングが空の機械での初回登録のみ手動:
#   secret-tool store --label='keepassxc master' type kdb

MAIN_HOSTNAME=P1
KDBX=$HOME/Dropbox/backup/keepassxc/master.kdbx
PW=$(secret-tool lookup type kdb)

if [ -n "$PW" ] && [ -r "$KDBX" ] && ! printf '%s\n' "$PW" | keepassxc-cli ls "$KDBX" >/dev/null 2>&1; then
    if [ "$(hostname)" = "$MAIN_HOSTNAME" ]; then
        if zenity --question --width=420 --text="キーリングのマスターパスワードでは KeePassXC を開けません。\nパスワードを変更した場合は、新しいパスワードで更新できます。\n（データベースファイルの破損の可能性もあります）\n\n更新しますか？"; then
            NEW=$(zenity --password --title="KeePassXC 新しいマスターパスワード")
            if printf '%s\n' "$NEW" | keepassxc-cli ls "$KDBX" >/dev/null 2>&1; then
                printf '%s' "$NEW" | secret-tool store --label='keepassxc master' type kdb
                make -C "$HOME/src/github.com/minorugh/dotfiles/backup" keyring-backup
                PW=$NEW
                zenity --info --width=420 --text="キーリングを更新し、Dropbox に反映しました。\n\nサブ機でも make keyring を実行してください。"
            else
                zenity --error --width=420 --text="そのパスワードでは開けなかったため、登録しませんでした。"
            fi
        fi
    else
        zenity --info --width=420 --text="キーリングのパスワードが古い可能性があります。\nP1 で変更済みなら、Dropbox の同期後に make keyring を実行してください。\n（反映に再ログインが必要な場合があります）"
    fi
fi

printf '%s\n' "$PW" | keepassxc --pw-stdin "$KDBX"
