#!/bin/bash
# dropbox-restart.sh
#
# Dropbox のトレイアイコン対策（2026-10 の Dropbox 仕様変更）
# 新しい Dropbox はトレイに AppIndicator を必須とし、起動時の判定に失敗するとアイコンが出ない（同期は動く）。
# ログイン直後は xfce4-panel のトレイが未準備で、その時点で Dropbox が起動すると失敗する（推定）。
# Dropbox は起動のたびに ~/.config/autostart/dropbox.desktop を自分で再生成するため、起動を遅らせる設定は戻される。
# そこで、パネルの準備が整ったあとに Dropbox を止め、停止を確認してから起動し直す。
# 公式サポート外の Debian + Xfce4 で必要。Dropbox 側が修正されたら不要になる。
#
# 呼び出し元: .autostart.sh

until dbus-send --session --print-reply --dest=org.kde.StatusNotifierWatcher /StatusNotifierWatcher org.freedesktop.DBus.Properties.Get string:org.kde.StatusNotifierWatcher string:IsStatusNotifierHostRegistered | grep -q true; do sleep 1; done
date
dropbox stop
while pgrep -x dropbox > /dev/null; do sleep 1; done
dropbox start -i
