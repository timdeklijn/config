if status is-interactive
    # Commands to run in interactive sessions can go here
end

# startup message ==============================================================
set -U fish_greeting "🏎️"

# aliases ======================================================================

# editor -----------------------------------------------------------------------
alias h="hx"

# git --------------------------------------------------------------------------
alias gs="git status"
alias ga="git add"
alias gp="git push"
alias gc="git commit"
alias gd="git diff"

# paths ========================================================================
fish_add_path /usr/local/bin
fish_add_path ~/bin

# pyenv ========================================================================
pyenv init - | source

# starship =====================================================================
starship init fish | source

# direnv =======================================================================
direnv hook fish | source
