#!/bin/bash

# Inherit SSH_AUTH_SOCK from keychain
source ~/.keychain/$(hostname)-sh 2>/dev/null

# Debug
echo "SSH_AUTH_SOCK=$SSH_AUTH_SOCK" > /tmp/fzilla-debug.log
ls -la $SSH_AUTH_SOCK >> /tmp/fzilla-debug.log 2>&1

filezilla "$@" &

exit
