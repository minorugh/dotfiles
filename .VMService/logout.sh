#!/bin/sh
VBoxManage controlvm Windows11 poweroff
sleep 1
xfce4-session-logout
exit
