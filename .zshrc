# zshrc
plugins=(… zsh-completions)
autoload -U compinit promptinit
compinit
promptinit

export LANG=ja_JP.UTF-8

autoload -Uz colors
colors

# last line (\n) problem countermeasure
unsetopt promptcr
# Suppress "Couldn't connect to accessibility bus:" gnome-terminal on Emacs
export NO_AT_BRIDGE=1

########################################
# History
########################################
if [ $(uname -n) = "P1" ]; then
    HISTFILE=~/Dropbox/backup/env/zsh/.zsh_history
else
    HISTFILE=~/.zsh_history
fi

HISTSIZE=1000000
SAVEHIST=1000000
LISTMAX=10000

zshaddhistory() {
    local line=${1%%$'\n'}
    local cmd=${line%% *}
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
setopt append_history
setopt inc_append_history
setopt share_history
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
unsetopt hist_verify
setopt hist_save_no_dups
setopt hist_expand

########################################
# Completion
########################################
setopt list_packed
setopt list_types
setopt auto_list
setopt auto_menu
setopt auto_param_keys
setopt auto_param_slash
setopt complete_in_word
setopt complete_aliases
setopt magic_equal_subst
unsetopt auto_remove_slash
unsetopt menu_complete

zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin
zstyle ':completion:*' use-cache true
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'
eval `dircolors -b`
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# completion for mosh
compdef mosh=ssh

########################################
# Options
########################################
setopt auto_resume
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt no_beep
setopt brace_ccl
setopt bsd_echo
setopt correct
setopt equals
setopt extended_glob
unsetopt flow_control
setopt no_flow_control
setopt hash_cmds
setopt no_hup
setopt long_list_jobs
setopt mail_warning
setopt numeric_glob_sort
setopt path_dirs
setopt print_eight_bit
setopt multios
setopt short_loops
setopt mark_dirs
setopt prompt_subst

umask 022

########################################
# Prompt
########################################
PROMPT="%{%(?.$fg_bold[cyan].$fg_bold[red])%}%m%{$fg_bold[white]%}%%%{$reset_color%} "
PROMPT2="%{$fg[magenta]%}%_%{$reset_color%}%{$fg_bold[white]%}>>%{$reset_color%} "
RPROMPT="%{${fg[blue]}%}[%~]%{${reset_color}%}"

autoload -Uz vcs_info
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
RPROMPT=$RPROMPT'${vcs_info_msg_0_}'

# Tmux: pass the name of the command currently executed to screen
case "${TERM}" in
    screen-256color|tmux-256color|xterm)
        preexec() {
            echo -ne "\ek#${1%% *}\e\\"
        }
        precmd() {
            echo -ne "\ek$(basename $(pwd))\e\\"
            vcs_info
        }
        ;;
    *)
        precmd() { vcs_info }
        ;;
esac

########################################
# Key bindings
########################################
bindkey -e
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

########################################
# cdr
########################################
autoload -Uz is-at-least
if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]; then
    autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
    add-zsh-hook chpwd chpwd_recent_dirs
    zstyle ':completion:*:*:cdr:*:*' menu selection
    zstyle ':completion:*' recent-dirs-insert both
    zstyle ':chpwd:*' recent-dirs-max 5000
    zstyle ':chpwd:*' recent-dirs-default true
    zstyle ':chpwd:*' recent-dirs-file "$HOME/Dropbox/backup/env/zsh/chpwd-recent-dirs"
    zstyle ':chpwd:*' recent-dirs-pushd true
fi

########################################
# PATH / Environment
########################################
export GOROOT="/usr/local/go"
export GOPATH="$HOME/src/go"
export PATH="$HOME/.local/bin:$GOROOT/bin:$GOPATH/bin:/usr/sbin:$PATH"

export EDITOR=nano
export XDG_CONFIG_HOME=$HOME/.config
export PAGER=less
export LESS='-g -i -M -R -S -W -z-4 -x4'

########################################
# keychain
########################################
[ -f $HOME/.keychain/$HOST-sh ] && source $HOME/.keychain/$HOST-sh

########################################
# Aliases
########################################
alias ls='ls -v -F --color=auto'
alias ll='ls -al'
alias la='ls -A'
alias cl='clear'
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=aunpack
alias cp='cp -ip'
alias mv='mv -i'
alias rm='rm -i'
alias du='du -h'
alias df='df -h'
alias free='free -h'
alias iv='sxiv'
alias fz='filezilla -s'
alias is='whois'
alias myip="ip -4 a | grep -oP '(?<=inet\s)\d+(\.\d+){3}' | grep -v 127"
alias open='xdg-open'
alias ex='exit'
alias pass='secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx'
alias sftp='sftp -oPort=10022 minorugh@minorugh.xsrv.jp'
alias xsrv='ssh xsrv'
alias lepton='~/Apps/Lepton-1.10.0.AppImage --no-sandbox --disable-gpu'

# Rclone
alias syncdrive='time rclone sync ${HOME}/Dropbox/Documents onedrive:Documents'
alias bsyncdrive='time rclone sync onedrive:Documents ${HOME}/Dropbox/Documents'
alias syncbackup='time rclone sync ${HOME}/Dropbox/backup onedrive:backup'

# neomutt
alias mutt='/usr/local/bin/neomutt.sh'

# vim
alias v='vim'

# emacs
alias e='emacs'
alias eq='emacs -q -l ~/.emacs.d/init-mini.el'
alias ekill='ps -u $USER -o pid,stat,time,command | grep -i emacs | grep -v "<defunct>" | fzf --reverse --header="[Kill Emacs]" --multi | awk "{print \$1}" | xargs kill -9'

# crontab
alias ce='crontab -e'

# password generator
alias pw12='pwgen 12 16'
alias pw24='pwgen 24 8'
alias pw40='pwgen 40 4'

# apps
alias pinta='flatpak run com.github.PintaProject.Pinta'
alias ifconfig='/sbin/ifconfig'

# apt
alias update='sudo apt update'
alias upgrade='sudo apt -y upgrade'

# system
alias sd='sudo shutdown -h now'
alias boot='sudo reboot'
alias by='xset dpms force standby'
alias logout='xfce4-session-logout'

# git log on xserver
alias dflog="ssh xsrv 'git -C ~/git/dotfiles.git log --oneline -10'"
alias ghlog="ssh xsrv 'git -C ~/git/GH.git log --oneline -10'"
alias mglog="ssh xsrv 'git -C ~/git/minorugh.com.git log --oneline -10'"

########################################
# Functions
########################################

# Mattermost restart
function mm() {
    local dir=~/src/github.com/minorugh/dotfiles/docker/mattermost
    docker compose -f $dir/docker-compose.yml down
    docker compose -f $dir/docker-compose.yml up -d
}

# cd してから ls
function chpwd() {
    ls -v -F --color=auto
}

# chmod helpers
function chmod-d() {
    echo "chmod 755 dirs: $(pwd)"
    find . -type d -exec chmod 755 {} +
}
function chmod-pl() {
    echo "chmod 755 cgi/pl: $(pwd)"
    find . -type f \( -name "*.cgi" -o -name "*.pl" \) -exec chmod 755 {} +
}
function chmod-web() {
    chmod-d
    chmod-pl
    echo "done."
}

# Emacs: カレントバッファのディレクトリに cd
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

# Emacs: dired を開く
function dired() {
    emacsclient -e "(dired \"${1:-$PWD}\")" & wmctrl -a emacs
}

# howm メモ作成（vim）
function memo() {
    echo "# memo:
[`date '+%Y-%m-%d %a %H:%M'`]" > ${HOME}/Dropbox/howm/`date '+%Y/%m/%Y%m%d%H%M'`.md
    chmod 755 ${HOME}/Dropbox/howm/`date '+%Y/%m/%Y%m%d%H%M'`.md
    vim -c 'startinsert' ${HOME}/Dropbox/howm/`date '+%Y/%m/%Y%m%d%H%M'`.md
}

# 画像変換
function webm2gif() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        ffmpeg -i $1 -pix_fmt rgb24 $fname.gif
    else
        echo 'usage: webm2gif file.webm'
    fi
}

function md2pdf() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        pandoc $1 -o $fname.pdf -V mainfont=IPAPGothic -V fontsize=16pt --pdf-engine=lualatex
    else
        echo 'usage: md2pdf file.md'
    fi
}

function md2docx() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        pandoc $1 -t docx -o $fname.docx -V mainfont=Gothic -V fontsize=16pt --toc --highlight-style=zenburn
    else
        echo 'usage: md2docx file.md'
    fi
}

function optimize-jpg() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        convert $1 -sampling-factor 4:2:0 -strip -quality 85 -interlace JPEG -colorspace sRGB ${fname}_converted.jpg
    else
        echo 'usage: optimize-jpg sample.jpg'
    fi
}

function blog-jpg() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        convert $1 -resize 800x re_${fname}.jpg
    else
        echo 'usage: blog-jpg sample.jpg'
    fi
}

function resize-all() {
    mkdir resize
    mogrify -path ./resize -strip -format jpg -unsharp 0.125x1.0+1+0.05 -quality 90 -modulate 105 -contrast -resize 1024x\> *.*
}

function optimize-png() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        convert $1 -strip ${fname}_converted.png
    else
        echo 'usage: optimize-png sample.png'
    fi
}

function ssh-fzf () {
    # configからHostを抜き出し、選んだらそのまま ssh するだけ
    local selected_host=$(grep -i "^Host " ~/.ssh/config | grep -v '[*?]' | grep -v "github.com" | awk '{print $2}' | fzf --reverse --height 40% --prompt="SSH-JUMP > ")

    if [ -n "$selected_host" ]; then
        BUFFER="ssh ${selected_host}"
        zle accept-line
    fi
    zle reset-prompt
}
zle -N ssh-fzf
bindkey '^\' ssh-fzf

########################################
# Plugins
########################################
# zsh-syntax-highlighting: sudo apt install zsh-syntax-highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

eval "$(gh completion -s zsh)"

### end
