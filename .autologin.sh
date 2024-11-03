#!/usr/bin/expect
# Run ssh-add with passphrase auto input at GUI startup
# Look ${PWD}/.config/autostart/autologin.desktop

# Include Password
source ~/backup/zsh/env.sh

spawn ssh-add /home/minoru/.ssh/id_rsa
expect "Enter passphrase for /home/minoru/.ssh/id_rsa:"
send "${PW}\n";
interact

exit
