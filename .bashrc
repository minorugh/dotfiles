# bash_history
HISTFILE=~/.bash_history

# alias
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias ll='ls -l'
alias la='ls -a'
alias bashrc='vim ~/.bashrc'
alias vimrc='vim ~/.vimrc'
alias tmuxconf='vim ~/.tmux.conf'
alias ..='cd ..'
alias ...='cd ../..'
alias reload='exec $SHELL -l'

# alias for git
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

# execute ls after cd
cd ()
{
  builtin cd "$@" && ls --color=auto
}

if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi
