#!/bin/bash

path=$(find ~/projects ~/projects/hbr ~/go/src/dev.azure.com/PORDTS/machine-learning-inspector ~/go/src/github.com/timdeklijn -type d -maxdepth 1 | fzf )
base=$(basename "$path")
tmux new-window -c $path -n $base
