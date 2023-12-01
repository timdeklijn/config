#!/bin/sh
# because the nixos configuration file is placed into
# a directory outside of home a custom install path is
# required. The path is given by `-t` (target) and should
# simply be the dir to write into.
sudo stow -t /etc/nixos nixos/
