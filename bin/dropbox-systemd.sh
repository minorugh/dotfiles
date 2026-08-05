#!/bin/bash
set -e

DOTFILES="$HOME/src/github.com/minorugh/dotfiles"

echo "==> 既存のdropboxプロセスを停止"
if systemctl --user is-active --quiet dropbox.service 2>/dev/null; then
    systemctl --user stop dropbox.service
else
    dropbox stop || true
fi

echo "==> systemd --userサービスを設置"
mkdir -p "$HOME/.config/systemd/user"
ln -vsf "$DOTFILES/etc/systemd/user/dropbox.service" "$HOME/.config/systemd/user/dropbox.service"
systemctl --user daemon-reload
systemctl --user enable dropbox.service
systemctl --user restart dropbox.service

echo "==> dropboxdの起動を待機"
until systemctl --user is-active --quiet dropbox.service; do
    sleep 1
done
sleep 3

echo "==> GUI autostartを無効化"
dropbox autostart n
rm -f "$HOME/.config/autostart/dropbox.desktop"

echo "==> lingerを有効化"
sudo loginctl enable-linger minoru

echo "==> サスペンド復帰フックを設置"
sudo ln -vsf "$DOTFILES/etc/systemd/system-sleep/dropbox-resume" /usr/lib/systemd/system-sleep/dropbox-resume
sudo chmod +x /usr/lib/systemd/system-sleep/dropbox-resume

echo "==> 状態確認"
systemctl --user status dropbox.service --no-pager || true
