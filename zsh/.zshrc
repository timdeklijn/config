# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

# You may need to manually set your language environment
export LANG=en_US.UTF-8
export LC_ALL="en_US.UTF-8"

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

# ALIASES =====================================================================

# VIM
alias vim="nvim"
alias h="hx"

# Kubernetes
alias k="kubectl"

# Git
alias lg="lazygit"
alias gc="git commit"
alias gs="git status"
alias gd="git diff"
alias glog="git log --all --decorate --oneline --graph"

# nicer ls
alias ls="exa --icons"
alias "ls -l"="exa -l --icons"
alias ll="exa -l --icons"

# Paths =======================================================================

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

export PATH="~/.pyenv/bin:$PATH"
eval "$(pyenv init --path)"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path)"
fi

# Go bin
export GOPATH="$HOME/go"
export PATH=$PATH:$(go env GOPATH)/bin
export PATH="/usr/local/opt/libarchive/bin:$PATH"

# TMUX in kitty
export LC_ALL=en_GB.UTF-8  
export LANG=en_GB.UTF-8

# STARSHIP
eval "$(starship init zsh)"

# For `broot` a shell script is required. This is installed in different
# locations based on the current OS.
if [[ "$OSTYPE" == "darwin"* ]]; then
  source /Users/timdeklijn/.config/broot/launcher/bash/br
else
  source /home/tim/.config/broot/launcher/bash/br
fi
