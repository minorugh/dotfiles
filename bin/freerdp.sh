#!/bin/bash
# Connect to windows on sub-machine (Thinkpad X250) via rdp
# The ipv4 address may change, so check with [cmd→ipconfig]
# Adjust size: according to display

xfreerdp /d:x250-win /p:gospel /size:1920x1180 /v:192.168.1.143

