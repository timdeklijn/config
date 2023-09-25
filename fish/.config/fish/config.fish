if status is-interactive
    # Commands to run in interactive sessions can go here
end

# startup message ==============================================================
set -U fish_greeting "🏎️"

# aliases ======================================================================

# editor -----------------------------------------------------------------------
# alias might depend on operating system. Helix is called `hx` on mac an `helix`
# on linux.
switch (uname)
    case Linux
        alias h="helix"
    case Darwin
        alias h="hx"
    case '*'
        echo OS is not Linux or Mac
end

alias vim="nvim"

# git --------------------------------------------------------------------------
alias gs="git status"
alias ga="git add"
alias gp="git push"
alias gc="git commit"
alias gd="git diff"

alias ls="exa --icons"
alias ll="exa --icons -l"

# paths ========================================================================
fish_add_path /usr/local/bin
fish_add_path ~/bin

# pyenv ========================================================================
pyenv init - | source

# starship =====================================================================
starship init fish | source

# direnv =======================================================================
direnv hook fish | source
