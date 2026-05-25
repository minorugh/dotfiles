# alias
alias ls='ls -v --color=auto'
alias ll='ls -al'
alias la='ls -A'
alias cl='clear'
alias ex='exit'

# cd してから ls
_last_dir="$PWD"
prompt_cd_ls() {
    if [ "$PWD" != "$_last_dir" ]; then
        _last_dir="$PWD"
        ls -a
    fi
}
PROMPT_COMMAND="prompt_cd_ls"
