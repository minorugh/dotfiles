#!/bin/bash
su - minoru -c '/usr/local/bin/automerge.sh >> /tmp/cron.log 2>&1'
su - minoru -c '/usr/local/bin/autobackup.sh >> /tmp/cron.log 2>&1'
