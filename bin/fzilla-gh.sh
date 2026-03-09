#!/bin/bash

# Inherit SSH_AUTH_SOCK from keychain
source ~/.keychain/${HOSTNAME}-sh 2>/dev/null

filezilla --site='0/gospel-haiku.com' &

exit
