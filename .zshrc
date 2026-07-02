# zshrc

########################################
# Initialization
########################################
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
HISTFILE=~/.zsh_history
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

########################################
# Options
########################################
setopt auto_resume
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt no_beep
setopt brace_ccl
setopt correct
setopt equals
setopt extended_glob
setopt no_flow_control
setopt hash_cmds
setopt no_hup
setopt long_list_jobs
setopt numeric_glob_sort
setopt path_dirs
setopt print_eight_bit
setopt multios
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

precmd() { vcs_info }

########################################
# Key bindings
########################################
bindkey -e
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# 履歴をfzfでインクリメンタル検索
function fzf-history() {
    BUFFER=$(fc -l -n 1 | tac | fzf --reverse --no-sort --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N fzf-history
bindkey '^r' fzf-history

########################################
# PATH / Environment
########################################
export GOROOT="/usr/local/go"
export GOPATH="$HOME/src/go"
export PATH="$HOME/.local/bin:$GOROOT/bin:$GOPATH/bin:/usr/sbin:$PATH"

export EDITOR=emacsclient
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
# 基本コマンド
alias ls='ls -v -F --color=auto'
alias ll='ls -al'
alias la='ls -A'
alias cl='clear'
alias cp='cp -ip'
alias mv='mv -i'
alias rm='rm -i'
alias du='du -h'
alias df='df -h'
alias free='free -h'
alias open='xdg-open'
alias ex='exit'
alias myip="ip -4 a | grep -oP '(?<=inet\s)\d+(\.\d+){3}' | grep -v 127"
alias is='whois'

# アーカイブ展開
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=aunpack

# アプリ
alias iv='sxiv'
alias fz='filezilla -s'
alias v='vim'
alias e='emacs'
alias eq='emacs -q -l ~/.emacs.d/init-mini.el'
alias ekill='ps -u $USER -o pid,stat,time,command | grep -i emacs | grep -v "<defunct>" | fzf --reverse --header="[Kill Emacs]" --multi | awk "{print \$1}" | xargs kill -9'
alias mutt='/usr/local/bin/neomutt.sh'
alias xsrv='ssh xsrv'
alias pass='secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx'
alias lepton='~/Apps/Lepton-1.10.0.AppImage --no-sandbox --disable-gpu'
alias pinta='flatpak run com.github.PintaProject.Pinta'
alias ifconfig='/sbin/ifconfig'

# パスワード生成
alias pw12='pwgen 12 16'
alias pw24='pwgen 24 8'
alias pw40='pwgen 40 4'

# apt
alias update='sudo apt update'
alias upgrade='sudo apt -y upgrade'

# crontab
alias ce='crontab -e'

alias pm='power-menu.sh'
########################################
# Functions
########################################

# cd してから ls
function chpwd() {
    ls -v -F --color=auto
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

function optimize-jpg() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        convert $1 -sampling-factor 4:2:0 -strip -quality 85 -interlace JPEG -colorspace sRGB ${fname}_converted.jpg
    else
        echo 'usage: optimize-jpg sample.jpg'
    fi
}

function optimize-png() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        convert $1 -strip ${fname}_converted.png
    else
        echo 'usage: optimize-png sample.png'
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

# ドキュメント変換
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

# README をインデックスディレクトリにシンボリックリンクで集約する
function mklink-readme() {
    local INDEX_DIR="$HOME/Dropbox/READMES"
    if [ ! -f "README.md" ]; then
        echo "❌ エラー: このディレクトリに README.md は見つかりません。"
        return 1
    fi
    mkdir -p "$INDEX_DIR"
    local link_name="$(basename $PWD)-README.md"
    local relative_target=$(python3 -c "import os; print(os.path.relpath('$PWD/README.md', '$INDEX_DIR'))")
    ln -sf "$relative_target" "$INDEX_DIR/$link_name"
    echo "✅ $INDEX_DIR/$link_name -> $relative_target"
}

########################################
# Plugins
########################################
# zsh-syntax-highlighting: sudo apt install zsh-syntax-highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# gh コマンド補完
eval "$(gh completion -s zsh)"

### end
export PATH=~/.npm-global/bin:$PATH
