#!/bin/bash

# Inherit SSH_AUTH_SOCK from keychain
source ~/.keychain/$(hostname)-sh 2>/dev/null

filezilla "$@" &

exit
