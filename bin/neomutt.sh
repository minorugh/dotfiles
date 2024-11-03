#!/bin/bash

# Change mutt startup directory so that attachments are stored in ~/Downloads.
# Add option to start gonome-terminal with maximized mode

cd ~/Downloads && gnome-terminal --maximize -- neomutt

exit
