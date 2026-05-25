# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# $PATH
export PATH=$PATH:$HOME/local/bin
export PATH=$HOME/opt/bin:$PATH
export PATH="$HOME/.local/bin:$PATH"

# history
HISTFILE=~/.bash_history
HISTSIZE=10000
HISTFILESIZE=20000
export HISTCONTROL=ignoreboth
export HISTIGNORE="ll:ls:la:cd:cd *:man:man *:scp:vim:nvim:less:ping:which:whois:uname:exit:clear"

# aliases - file operations
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# aliases - ls
alias ls='ls -v -F --color=auto --group-directories-first'
alias ll='ls -al'
alias la='ls -A'

# aliases - navigation
alias ..='cd ..'
alias ...='cd ../..'

# aliases - misc
alias cl='clear'
alias ex='exit'
alias reload='exec $SHELL -l'
alias v='exec vim'
alias bashrc='vim ~/.bashrc'
alias vimrc='vim ~/.vimrc'

# aliases - git
alias gs='git status'
alias ga='git add'
alias gd='git diff'
alias gds='git diff --staged'
alias gcm='git commit -m'
alias gl='git log --oneline --graph --decorate'
alias gp='git push origin HEAD'
alias gc='git checkout'

# prompt
export PS1="[\u@\h] \w \$ "

# cd してから ls
_last_dir="$PWD"
prompt_cd_ls() {
    if [ "$PWD" != "$_last_dir" ]; then
        _last_dir="$PWD"
        ls -a
    fi
}
PROMPT_COMMAND="prompt_cd_ls"

# fzf
if command -v fzf &>/dev/null; then
    eval "$(fzf --bash)"
    export FZF_DEFAULT_OPTS="--tac"
fi
