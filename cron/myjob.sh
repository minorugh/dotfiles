##################################################
## Backup and update GH member password file
## Created 2026.2.12
##################################################

## Backup GH member files on server to remote location
rsync -azu --delete xsrv:/home/minorugh/gospel-haiku.com/passwd/* ${HOME}/Dropbox/GH/reg/passwd/
rsync -azu --delete xsrv:/home/minorugh/gospel-haiku.com/passwd/* ${HOME}/Dropbox/backup/passwd/ghuser

## If there is additional data in dmembar, add it to wmember as well
cd ${HOME}/Dropbox/GH/reg/passwd
perl uppasswd.pl

## Copy dmembar to smembar (to prevent conflicts)
rsync -azu ${HOME}/Dropbox/GH/reg/passwd/dmember.cgi xsrv:/home/minorugh/gospel-haiku.com/passwd/smember.cgi
rsync -azu ${HOME}/Dropbox/GH/reg/passwd/wmember.cgi xsrv:/home/minorugh/gospel-haiku.com/passwd/wmember.cgi

## Copy dmember to minorugh.com (to use in other content)
rsync -azu ${HOME}/Dropbox/GH/reg/passwd/dmember.cgi xsrv:/home/minorugh/minorugh.com/passwd/dmember.cgi

## end here
