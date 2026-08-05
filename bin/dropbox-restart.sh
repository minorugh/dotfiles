#!/bin/bash
LOGFILE="$HOME/.cache/dropbox-resume.log"
mkdir -p "$(dirname "$LOGFILE")"

dbus-monitor --system "type='signal',interface='org.freedesktop.login1.Manager',member='PrepareForSleep'" 2>&1 |
while read -r line; do
    case "$line" in
        *member=PrepareForSleep*)
            waiting=1
            ;;
        *boolean\ true*)
            if [ "$waiting" = "1" ]; then
                echo "$(date '+%Y-%m-%d %H:%M:%S') suspend detected" >> "$LOGFILE"
                waiting=0
            fi
            ;;
        *boolean\ false*)
            if [ "$waiting" = "1" ]; then
                echo "$(date '+%Y-%m-%d %H:%M:%S') resume detected, restarting dropbox" >> "$LOGFILE"
                dropbox stop
                sleep 5
                dropbox start -i > /dev/null 2>&1
                waiting=0
            fi
            ;;
    esac
done
