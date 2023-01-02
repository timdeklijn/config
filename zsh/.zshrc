# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

export ZSH="/home/tim/.oh-my-zsh"
# Path to your oh-my-zsh installation.
case "$OSTYPE" in
  darwin*)
    export ZSH="/Users/timdeklijn/.oh-my-zsh"
  ;;
  linux*)
    export ZSH="/home/tim/.oh-my-zsh"
  ;;
esac

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
	git
	zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

# User configuration
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# Use vi bindings in terminal =================================================

set -o vi

# use zsh functions ============================================================

fpath=(~/dotfiles/zsh/functions/ $fpath)
# one function per file so far...
autoload -Uz ~/dotfiles/zsh/functions/f

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

# fix ctop
alias ctop='TERM="${TERM/#tmux/screen}" ctop'

# Configs
alias zshconfig="vim ~/.zshrc"

# VIM
alias vim="nvim"

# Kubernetes
alias k="kubectl"

# Git
alias lg="lazygit"
alias gc="git commit"
alias gs="git status"
alias gd="git diff"
alias glog="git log --all --decorate --oneline --graph"

# Paths =======================================================================

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/timdeklijn/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/timdeklijn/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/timdeklijn/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/timdeklijn/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

export NVM_DIR="/Users/timdeklijn/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
export PATH="/usr/local/opt/sbt@0.13/bin:$PATH"
export PATH="/usr/local/opt/openjdk@8/bin:$PATH"


export PATH="~/.pyenv/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv virtualenv-init -)"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path)"
fi
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/usr/local/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
        . "/usr/local/Caskroom/miniconda/base/etc/profile.d/conda.sh"
    else
        export PATH="/usr/local/Caskroom/miniconda/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

# update X11 display (this is overwritten in tmux)
echo $DISPLAY > ~/.display.txt
alias up_disp='export DISPLAY=`cat ~/.display.txt`'

# Go bin
export GOPATH="$HOME/go"
export PATH=$PATH:$(go env GOPATH)/bin
# export PATH="$GOPATH/bin:$PATH"
export PATH="/usr/local/opt/libarchive/bin:$PATH"

# TMUX in kitty
export LC_ALL=en_GB.UTF-8  
export LANG=en_GB.UTF-8

# STARSHIP
eval "$(starship init zsh)"

# asdf
. /usr/local/opt/asdf/libexec/asdf.sh
export PATH="/usr/local/opt/llvm/bin:$PATH"
