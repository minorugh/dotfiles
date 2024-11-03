# Mozc copy for submachine (conflict prevention)
if [[string-match "x250" (shell-command-to-string "uname -n")]]then
'cp -rf ~/Dropbox/mozc/.mozc ~/'
fi
