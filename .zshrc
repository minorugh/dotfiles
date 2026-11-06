########################################################################
##
##   .zshrc  -  zsh configuration
##
##   Author  : Minoru Yamada
##   Created : 2024-10-26
##   Updated : 2026-11-05
##   Version : 2.0
##   Target  : Debian 13
##   Repo    : ~/src/github.com/minorugh/dotfiles/.zshrc
##             (~/.zshrc へシンボリックリンク)
##
##   Sections:
##     Environment / Options / History / Completion / Prompt
##     Key bindings / Aliases / Functions / Plugins
##
########################################################################

########################################
# Environment
########################################
export LANG=ja_JP.UTF-8
# gnome-terminal on Emacs: "Couldn't connect to accessibility bus:" を抑止
export NO_AT_BRIDGE=1

export GOROOT="/usr/local/go"
export GOPATH="$HOME/src/go"
export PATH="$HOME/.npm-global/bin:$HOME/.local/bin:$GOROOT/bin:$GOPATH/bin:/usr/sbin:$PATH"

export EDITOR=emacsclient
export VISUAL=emacsclient
export SUDO_EDITOR=vim
export XDG_CONFIG_HOME=$HOME/.config
export PAGER=less
export LESS='-g -i -M -R -S -W -z-4 -x4'

umask 022
eval `dircolors -b`

########################################
# Options
########################################
# 最終行の改行問題の対策
unsetopt promptcr

setopt auto_resume
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt no_beep
setopt brace_ccl
setopt correct
setopt extended_glob
setopt no_flow_control
setopt no_hup
setopt long_list_jobs
setopt numeric_glob_sort
setopt path_dirs
setopt print_eight_bit
setopt mark_dirs

########################################
# History
########################################
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

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
       && ${cmd} != uname
       && ${cmd} != md5sum
       && ${cmd} != blog-jpg
       && ${cmd} != xdg-open
       && ${cmd} != mpv-music
       && ${cmd} != mpv-video
       && ${cmd} != traceroute
       && ${cmd} != speedtest-cli
    ]]
}

setopt share_history
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_no_store

########################################
# Completion
########################################
autoload -Uz compinit
compinit

setopt list_packed
setopt list_types
setopt complete_in_word
setopt complete_aliases
setopt magic_equal_subst
unsetopt auto_remove_slash

LISTMAX=10000

zstyle ':completion:*' use-cache true
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

eval "$(gh completion -s zsh)"

########################################
# Prompt
########################################
autoload -Uz colors
colors

setopt prompt_subst

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

# 履歴を fzf でインクリメンタル検索
fzf-history() {
    BUFFER=$(fc -l -n 1 | tac | fzf --reverse --no-sort --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N fzf-history
bindkey '^r' fzf-history

########################################
# Aliases
########################################
# 基本コマンド
alias ls='ls -v -F --color=auto --group-directories-first'
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
alias myip="ip -4 a | grep -oP '(?<=inet\s)\d+(\.\d+){3}' | grep -v 127"

# アーカイブ展開
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=aunpack

# アプリ・ツール
alias e='emacs'
alias v='vim'
alias gd='gnome-disks'
alias mutt='/usr/local/bin/neomutt.sh'
alias pinta='flatpak run com.github.PintaProject.Pinta'
alias ig='CHROMIUM=chromium sh ~/i.minorugh.com/open_public.sh &'

# dotfiles/Makefile のチェック
alias mfcheck='~/src/github.com/minorugh/dotfiles/bin/makefile-check.sh'

########################################
# Functions
########################################
# cd してから ls
chpwd() {
    ls -v -F --color=auto
}

# 最小構成で Emacs を起動する（復旧用）
# init.el を読まず init-mini.el だけを読む
eq() {
    emacs -q -l ~/.emacs.d/init-mini.el
}

# ブログ用に画像を幅 800px へ縮小する
blog-jpg() {
    if [ $# = 1 ]; then
        fname="${1%.*}"
        convert $1 -resize 800x re_${fname}.jpg
    else
        echo 'usage: blog-jpg sample.jpg'
    fi
}

########################################
# Plugins
########################################
# zsh-syntax-highlighting: sudo apt install zsh-syntax-highlighting
# 補完や bindkey の設定より後に読み込む
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
