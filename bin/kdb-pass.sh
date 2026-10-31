#!/bin/bash
secret-tool store --label='keepassxc master' type kdb
make -f ~/src/github.com/minorugh/dotfiles/backup/Makefile keyring-backup
