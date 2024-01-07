# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# You may need to manually set your language environment
# export LANG=en_US.UTF-8
# export LC_ALL="en_US.UTF-8"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# START USING DIRENV ===========================================================

eval "$(direnv hook zsh)"

setopt PROMPT_SUBST

show_virtual_env() {
  if [[ -n "$VIRTUAL_ENV" && -n "$DIRENV_DIR" ]]; then
    echo "($(basename $VIRTUAL_ENV))"
  fi
}
PS1='$(show_virtual_env)'$PS1

set -o emacs

# ALIASES =====================================================================

# Kubernetes
alias k="kubectl"

alias vim="nvim"

# Git
alias lg="lazygit"
alias gc="git commit"
alias gs="git status"
alias gd="git diff"
alias gp="git push"
alias glog="git log --all --decorate --oneline --graph"

# nicer ls
alias ls="eza --icons"
alias "ls -l"="eza -l --icons"
alias ll="eza -l --icons"

# Paths =======================================================================

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

# Go bin
export GOPATH="$HOME/go"
export PATH=$PATH:$(go env GOPATH)/bin
export PATH="/usr/local/opt/libarchive/bin:$PATH"

# STARSHIP
eval "$(starship init zsh)"
