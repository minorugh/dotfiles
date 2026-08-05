#!/bin/bash
LOGFILE="$HOME/.cache/dropbox-resume.log"
mkdir -p "$(dirname "$LOGFILE")"

# dbusの出力を監視
dbus-monitor --system "type='signal',interface='org.freedesktop.login1.Manager',member='PrepareForSleep'" 2>&1 |
while read -r line; do
    # PrepareForSleep シグナル本体を検知したら、次の行（引数）を読み込む
    if [[ "$line" == *"member=PrepareForSleep"* ]]; then
        read -r arg_line

        if [[ "$arg_line" == *"boolean true"* ]]; then
            echo "$(date '+%Y-%m-%d %H:%M:%S') suspend detected" >> "$LOGFILE"

        elif [[ "$arg_line" == *"boolean false"* ]]; then
            echo "$(date '+%Y-%m-%d %H:%M:%S') resume detected, restarting dropbox" >> "$LOGFILE"
            dropbox stop
            sleep 2
            dropbox start -i > /dev/null 2>&1
        fi
    fi
done
