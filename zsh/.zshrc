# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source $HOME/.bash_profile

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/go/bin:$PATH
export GOPATH=$HOME/go
export PATH=$PATH:$(go env GOPATH)/bin
export PATH=$PATH:$GOPATH/bin
export PATH="/usr/local/opt/libarchive/bin:$PATH"

# sublime
export PATH="/Applications/Sublime Text.app/Contents/SharedSupport/bin:$PATH"
# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  # TODO: fix difference in naming for helix
  if [[ $OSTYPE == darwin* ]]; then
    export EDITOR="nvim"
  else
    export EDITOR="nvim"
  fi
fi

# START USING DIRENV ===========================================================

eval "$(direnv hook zsh)"
setopt PROMPT_SUBST

show_virtual_env() {
  if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
    echo "($(basename $VIRTUAL_ENV))"
  fi
}
PS1='$(show_virtual_env)'$PS1

# what keybind to use
set -o vi

# ALIASES =====================================================================

# Kubernetes
alias k="kubectl"

alias vim="nvim"
alias h="helix"
alias h="hx"
alias y="yazi"

# Git
alias gu="gitu"
alias lg="lazygit"
alias gc="git commit"
alias gs="git status"
alias gd="git diff"
alias gp="git push"
alias glog="git log --all --decorate --oneline --graph"

# nicer ls
alias ls="eza"
alias "ls -l"="eza -l"
alias ll="eza -l"

# Paths =======================================================================

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

# This is cloned into workspace
source ~/workspace/powerlevel10k/powerlevel10k.zsh-theme

export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

eval "$(direnv hook zsh)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# NOTE:install nvm (on ubuntu): 
#   https://www.digitalocean.com/community/tutorials/how-to-install-node-js-on-ubuntu-22-04
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
