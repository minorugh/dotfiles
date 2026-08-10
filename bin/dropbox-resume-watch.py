#!/usr/bin/env python3
#
# dropbox-resume-watch.py
#
# 判定方法: systemd-logindが発する PrepareForSleep シグナルを、D-Busの
# 正規の購読機構（add_signal_receiver）で受信する。dbus-monitorのような
# eavesdropping方式ではないため、一般ユーザー権限で確実に動作する。
# サスペンド復帰時（false）を検知した瞬間にDropboxを再起動する。
#
# 実行方法: systemd --user サービスとして常駐させる（sudo不要）
#
# Author: Minoru Yamada (aodamo)
# Created: 2026-08-10
#
import subprocess
import time
from datetime import datetime
from pathlib import Path

import dbus
import dbus.mainloop.glib
from gi.repository import GLib

LOGFILE = Path.home() / ".cache" / "dropbox-watch.log"


def restart_dropbox():
    time.sleep(10)
    subprocess.run(["pkill", "-x", "dropbox"])
    time.sleep(3)
    subprocess.Popen(
        ["dropbox", "start"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    with open(LOGFILE, "a") as f:
        f.write(f"{timestamp} dropbox restarted (resume detected via dbus)\n")
    rotate_log()


def rotate_log():
    if not LOGFILE.exists():
        return
    lines = LOGFILE.read_text().splitlines()
    if len(lines) > 1000:
        LOGFILE.write_text("\n".join(lines[-200:]) + "\n")


def on_prepare_for_sleep(sleeping):
    if not sleeping:
        restart_dropbox()


def main():
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
    bus = dbus.SystemBus()
    bus.add_signal_receiver(
        on_prepare_for_sleep,
        signal_name="PrepareForSleep",
        dbus_interface="org.freedesktop.login1.Manager",
    )
    loop = GLib.MainLoop()
    loop.run()


if __name__ == "__main__":
    main()
