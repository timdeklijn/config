# Author: Tim
# Description: Bash configuration

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export LANG=en_US.UTF-8
export LC_ALL="en_US.UTF-8"

# Some listing aliases
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias grep='grep --color=auto'

# Git shortcuts
alias gs="git status"
alias gc="git commit"
alias gp="git push"
alias ga="git add"
alias gd="git diff"

# I do not want to type nvim
alias vim="nvim"
alias h="helix"

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# direnv
eval "$(direnv hook bash)"

# starship
eval "$(starship init bash)"

# local bin
PATH=$PATH:~/bin

# source /home/tim/.config/broot/launcher/bash/br

# source /Users/timdeklijn/.config/broot/launcher/bash/br
