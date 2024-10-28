# zshrc
plugins=(… zsh-completions)
autoload -U compinit promptinit
compinit
promptinit

export LANG=ja_JP.UTF-8

autoload -Uz colors
colors

# last line (\n) probrem countermeasure
unsetopt promptcr

# Custom keyboard map
setxkbmap -option ctrl:nocaps
xmodmap ~/.Xmodmap
# Suppres "Couldn't connect to accessibility bus:" gnome-terminal on Emacs
export NO_AT_BRIDGE=1

# zsh_history
HISTFILE=~/backup/zsh/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000
# ask you if you have over 10000 history
LISTMAX=10000

zshaddhistory() {
    local line=${1%%$'\n'}
    local cmd=${line%% *}

    # Only those that satisfy all of the following conditions are added to the history
    [[ ${#line} -ge 5
       && ${cmd} != ll
       && ${cmd} != ls
       && ${cmd} != la
       && ${cmd} != cd
       && ${cmd} != man
       && ${cmd} != scp
       && ${cmd} != vim
       && ${cmd} != nvim
       && ${cmd} != less
       && ${cmd} != ping
       && ${cmd} != open
       && ${cmd} != file
       && ${cmd} != which
       && ${cmd} != whois
       && ${cmd} != drill
       && ${cmd} != uname
       && ${cmd} != md5sum
       && ${cmd} != pacman
       && ${cmd} != blog-jpg
       && ${cmd} != xdg-open
       && ${cmd} != mpv-music
       && ${cmd} != mpv-video
       && ${cmd} != traceroute
       && ${cmd} != speedtest-cli
    ]]
}

unsetopt extended_history
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt hist_no_store
# Add history
setopt append_history
# Add history incremental
setopt inc_append_history
# Share history other terminal
setopt share_history
# Duplicate command delete it older
setopt hist_ignore_all_dups
# Same command as before don't add to history
setopt hist_ignore_dups
# Commands beginning with a space delete from history list
setopt hist_ignore_space
# While calling history and executing stop editing once
unsetopt hist_verify
# Extra white space packed and recorded
setopt hist_reduce_blanks
# When writing to the history file, ignore the same as the old command.
setopt hist_save_no_dups
# Do not register the history command in the history
setopt hist_no_store
# Automatically expand history on completion
setopt hist_expand
# Complementary completion list displayed
setopt list_packed
unsetopt auto_remove_slash
# Matching directory with expanding file name appending / to the end
setopt mark_dirs
# Identification of the type of file in complementary candidate list
setopt list_types
# When there are multiple completion candidates, list display
unsetopt menu_complete
# When there are multiple completion candidates, list display
setopt auto_list
# Automatically complement parentheses' correspondence etc
setopt auto_param_keys
# If you execute the same command name as the suspended process, resume
setopt auto_resume
# Move by directory only
setopt auto_cd
# Do not emit beep with command input error
setopt no_beep
# Enable brace expansion function
setopt brace_ccl
setopt bsd_echo
setopt complete_in_word
# = Expand COMMAND to the path name of COMMAND
setopt equals
# Enable extended globbing
setopt extended_glob
# (Within shell editor) Disable C-s and C-q
unsetopt flow_control
# Do not use flow control by C-s/C-q
setopt no_flow_control
# Hash the path when each command is executed
setopt hash_cmds
# Do not kill background jobs when logging out
setopt no_hup
# By default, jobs -L is set as the output of the internal command jobs
setopt long_list_jobs
setopt mail_warning
# Interpret numbers as numbers and sort
setopt numeric_glob_sort
# Search for subdirectories in PATH when / is included in command name
setopt path_dirs
# Appropriate display of Japanese in completion candidate list
setopt print_eight_bit
# With command line arguments you can complement even after = = PREFIX = / USR etc
setopt magic_equal_subst
# When completing completion candidates, display as compacted as possible.
setopt list_packed
# Include alias as a candidate for completion.
setopt complete_aliases
# Do not delete the last / when the directory name is an argument
setopt noautoremoveslash
# TEE and CAT functions such as multiple redirects and pipes are used as necessary
setopt multios
# You will be able to use simplified grammar with FOR, REPEAT, SELECT, IF, FUNCTION
setopt short_loops
# Automatically add / at the end with directory name completion to prepare for the next completion
setopt auto_param_slash
# Completion key Completion candidate is complemented automatically in order by repeated hitting
setopt auto_menu

# Completion of sudo
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

zstyle ':completion:*' use-cache true
# Select completion candidate with ← ↓ ↑ →
zstyle ':completion:*:default' menu select=1
# Since there may be uniquely determined files, first complement them
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'
# Candidate directories on cdpath only when there is no candidate in the current directory
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
# Process name completion of ps command
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'
# Color a completion candidate
eval `dircolors -b`
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# prompt
PROMPT="%{%(?.$fg_bold[cyan].$fg_bold[red])%}%m%{$fg_bold[white]%}%%%{$reset_color%} "
PROMPT2="%{$fg[magenta]%}%_%{$reset_color%}%{$fg_bold[white]%}>>%{$reset_color%} "
# Show your current location on the right prompt
RPROMPT="%{$fg_bold[white]%}[%{$reset_color%}%{$fg[cyan]%}%~%{$reset_color%}%{$fg_bold[white]%}]%{$reset_color%}"

# emacs keybind
bindkey -e

# Present candidate for moved directory
setopt auto_pushd

# Do not record duplicate directories with auto_pushd.
setopt pushd_ignore_dups

# It points out the misspelling of the command and presents the expected correct command.
setopt correct

# Permission when creating files
umask 022

# vcs_info
RPROMPT="%{${fg[blue]}%}[%~]%{${reset_color}%}"
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () { vcs_info }
RPROMPT=$RPROMPT'${vcs_info_msg_0_}'

# Tmux, pass the name of the command currently executed to screen
case "${TERM}"
in screen-256color)
       preexec() {
	   echo -ne "\ek#${1%% *}\e\\"
       }
       precmd() {
	   echo -ne "\ek$(basename $(pwd))\e\\"
	   vcs_info
       };;
   tmux-256color)
       preexec() {
	   echo -ne "\ek#${1%% *}\e\\"
       }
       precmd() {
	   echo -ne "\ek$(basename $(pwd))\e\\"
	   vcs_info
       };;
   xterm)
       preexec() {
	   echo -ne "\ek#${1%% *}\e\\"
       }
       precmd() {
	   echo -ne "\ek$(basename $(pwd))\e\\"
	   vcs_info
       };;
esac

# Delete by word with C-w
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# If command not found then find it when using arch linux(sudo pacman -S pkgfile)
# if [ -f /usr/share/doc/pkgfile/command-not-found.zsh ]; then
#     source /usr/share/doc/pkgfile/command-not-found.zsh
# fi

# keychain config
# /usr/bin/keychain $HOME/.ssh/id_rsa
# source $HOME/.keychain/$HOST-sh


# completion mosh
compdef mosh=ssh

# aliases
alias ls='ls -v -F --color=auto'
alias ll='ls -al'
alias la='ls -A'
alias cl='clear'
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=aunpack #./hogefuga.tar.gz(pacman -S atool)
alias cp='cp -ip'
alias mv='mv -i'
alias rm='rm -i'
alias du='du -h'
alias df='df -h'
alias free='free -h'
alias iv='sxiv'
alias fz='filezilla -s'
alias is='whois'
alias myip="ip -4 a show wlp2s0 | grep -oP '(?<=inet\s)\d+(\.\d+){3}'"
alias open='xdg-open'
alias ex='exit'
alias pass='secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx'
alias sftp='sftp -oPort=10022 minorugh@minorugh.xsrv.jp'
alias xsrv='ssh xsrv'
alias lepton='/home/minoru/Appimage/Lepton-1.10.0.AppImage'

# For docker-compose
alias dcbd="docker-compose up -d --build"
alias dcup="docker-compose up -d"
alias dcdn="docker-compose down -v"
alias dcex="docker container exec -it webcgi bash"
alias dcps="docker-compose ps"

# For neomutt to start at ~/Downloads
alias neomutt='/usr/local/bin/neomutt.sh'

# For vim
alias v='vim'
alias sv='sudo vim'

# For emacs
alias e='emacs'
alias ec='/usr/local/bin/emacsclient -n'
# alias e="emacsclient -t -a ''"
alias eq='emacs -q -l ~/.emacs.d/mini-init.el'

# Password generator
alias pw12='pwgen 12 16'
alias pw24='pwgen 24 8'
alias pw40='pwgen 40 4'

alias pinta='flatpak run com.github.PintaProject.Pinta'

## apt install net-tools to use ifconfig on Debian and set $PATH
alias ifconfig='/sbin/ifconfig'

## apt update upgrade
alias update='sudo apt update'
alias upgrade='sudo apt -y upgrade'

## sudo shutdown
alias sd='sudo shutdown -h now'
## sudo reboot
alias boot='sudo reboot'
## standby mode
alias by='xset dpms force standby'
## Session logout
alias logout='xfce4-session-logout'

## get api token from github
function get-github-api () {
curl -u 'minorugh' -d '{"scopes":["repo"],"note":"Help example"}' https://api.github.com/authorizations
}

# create howm memo with vim
function memo () {
echo "# memo:
[`date '+%Y-%m-%d %a %H:%M'`]" > ${HOME}/Dropbox/howm/`date '+%Y/%m/%Y%m%d%H%M'`.md
chmod 755 ${HOME}/Dropbox/howm/`date '+%Y/%m/%Y%m%d%H%M'`.md
vim -c 'startinsert' ${HOME}/Dropbox/howm/`date '+%Y/%m/%Y%m%d%H%M'`.md
}

# PATH
export GOPATH="/usr/local/go"
export PATH="$PATH:$GOPATH/bin:$HOME/bin:/usr/sbin"
export EDITOR='emacsclient'
export XDG_CONFIG_HOME=$HOME/.config
export PAGER=less
export LESS='-g -i -M -R -S -W -z-4 -x4'
export PATH="$PATH:$HOME/.opam/default/bin"

# cdr
autoload -Uz is-at-least
if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]; then
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
  add-zsh-hook chpwd chpwd_recent_dirs
  zstyle ':completion:*:*:cdr:*:*' menu selection
  zstyle ':completion:*' recent-dirs-insert both
  zstyle ':chpwd:*' recent-dirs-max 5000
  zstyle ':chpwd:*' recent-dirs-default true
  zstyle ':chpwd:*' recent-dirs-file "$HOME/backup/zsh/chpwd-recent-dirs"
  zstyle ':chpwd:*' recent-dirs-pushd true
fi

# cd after then ls
function chpwd() {
    ls -v -F --color=auto
}

# Chdir to the ``default-directory'' of currently opened in Emacs buffer.
function cde() {
    EMACS_CWD=`emacsclient -e "
     (expand-file-name
      (with-current-buffer
          (if (featurep 'elscreen)
              (let* ((frame-confs (elscreen-get-frame-confs (selected-frame)))
                     (num (nth 1 (assoc 'screen-history frame-confs)))
                     (cur-window-conf (cadr (assoc num (assoc 'screen-property frame-confs))))
                     (marker (nth 2 cur-window-conf)))
                (marker-buffer marker))
            (nth 1
                 (assoc 'buffer-list
                        (nth 1 (nth 1 (current-frame-configuration))))))
        default-directory))" | sed 's/^"\(.*\)"$/\1/'`
    echo "chdir to $EMACS_CWD"
    cd "$EMACS_CWD"
}

# Invoke the ``dired'' of current working directory in Emacs buffer.
function dired() {
    emacsclient -e "(dired \"${1:-$PWD}\")" & wmctrl -a emacs
}

function github-new() {
    if [ $# = 1 ]; then
	ghq root && cat ~/.config/hub | grep user \
	    && cd $(ghq root)/github.com/$(cat ~/.config/hub \
					       | grep user | awk '{print $3}') && mkdir $1
	if [ $? = 0 ]; then
	    cd $1
	    git init .
	    hub create
	    touch README.md
	    git add README.md
	    git commit -m 'first commit'
	    git push origin master
	fi
    else
	echo 'usage: github-new reponame'
    fi
}

function webm2gif() {
    if [ $# = 1 ]; then
	fname_ext=$1
	fname="${fname_ext%.*}"
	ffmpeg -i $1 -pix_fmt rgb24 $fname.gif
    else
	echo 'usage: webm2gif file.webm'
    fi
}

function md2pdf() {
    if [ $# = 1 ]; then
	fname_ext=$1
	fname="${fname_ext%.*}"
	pandoc $1 -o $fname.pdf -V mainfont=IPAPGothic -V fontsize=16pt --pdf-engine=lualatex
    else
	echo 'usage: md2pdf file.md'
    fi
}

function md2docx() {
    if [ $# = 1 ]; then
	fname_ext=$1
	fname="${fname_ext%.*}"
	pandoc $1 -t docx -o $fname.docx -V mainfont=Gothic -V fontsize=16pt --toc --highlight-style=zenburn
    else
	echo 'usage: md2docx file.md'
    fi
}

function optimize-jpg() {
    if [ $# = 1 ]; then
	fname_ext=$1
	fname="${fname_ext%.*}"
	convert $1 -sampling-factor 4:2:0 -strip -quality 85 -interlace JPEG -colorspace sRGB ${fname}_converted.jpg
    else
	echo 'usage: optimize-jpg sample.jpg'
    fi
}

function blog-jpg() {
    if [ $# = 1 ]; then
	fname_ext=$1
	fname="${fname_ext%.*}"
	convert $1 -resize 800x re_${fname}.jpg
	# rm -rf $1
    else
	echo 'usage: blog-jpg sample.jpg'
    fi
}

# Resize all in directory
function resize-all() {
	mkdir resize
	mogrify -path ./resize -strip -format jpg -unsharp 0.125x1.0+1+0.05 -quality 90 -modulate 105 -contrast -resize 1024x\> *.*
}

function optimize-png() {
    if [ $# = 1 ]; then
	fname_ext=$1
	fname="${fname_ext%.*}"
	convert $1 -strip ${fname}_converted.png
    else
	echo 'usage: optimize-png sample.png'
    fi
}

# zsh-syntax-highlighting : sudo apt install zsh-syntax-highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

### end
